;;; agent-shell-auto-retry.el --- Auto-retry turns that end with a retriable error -*- lexical-binding: t; -*-

;; Auto-retry for agent-shell, built on the public event API
;; (`agent-message-chunk' added in agent-shell 0.70.2 for this use
;; case) after upstream preferred this live outside core.
;;
;; Enable per shell with M-x my/agent-shell-retry-mode, or everywhere
;; with M-x my/agent-shell-global-retry-mode (or from config:
;; (my/agent-shell-global-retry-mode 1)).
;;
;; Known limitation vs an in-core implementation: there is no
;; interrupt event, so interrupting during the backoff window does not
;; cancel the pending retry; toggle the mode off (or interrupt the
;; retry turn itself) if one slips through.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-prompt-queue)
(require 'map)
(require 'seq)

(defvar my/agent-shell-retry-max-retries 5
  "Maximum auto-retry turns per user prompt before giving up.")

(defvar my/agent-shell-retry-backoff-seconds '(5 10 20 40 80)
  "Backoff delays indexed by 1-based retry number; last entry repeats.")

(defvar my/agent-shell-retry-regexps
  '("^Error: RetriableError:"
    "exceeded retry limit, last status:[[:space:]]*429[[:space:]]+Too Many Requests$")
  "Case-insensitive patterns a turn's final line must match to retry.")

(defvar my/agent-shell-retry-prompt
  "[auto-retry] The previous turn failed with a retriable error. Please continue working on the task where you left off."
  "Prompt sent as a new user turn to recover from a retriable error.")

(defvar-local my/agent-shell-retry--tail ""
  "Bounded tail of the current turn's agent message text.")

(defvar-local my/agent-shell-retry--attempt 0
  "Auto-retries sent for the current user prompt.")

(defvar-local my/agent-shell-retry--last-user-prompt nil
  "Most recent user-authored prompt, from the `input-submitted' event.")

(defvar-local my/agent-shell-retry--timer nil
  "Pending retry timer, if any.")

(defvar-local my/agent-shell-retry--own-submit nil
  "Non-nil while the next `input-submitted' is our own retry prompt.")

(defvar-local my/agent-shell-retry--subscriptions nil
  "Event subscription tokens while the mode is enabled.")

(defun my/agent-shell-retry--tail-line (text)
  "Return TEXT's final non-blank line when it matches a retriable pattern."
  (when-let* ((line (car (last (seq-remove #'string-empty-p
                                           (seq-map #'string-trim
                                                    (split-string (or text "") "\n"))))))
              (case-fold-search t)
              ((seq-some (lambda (regexp) (string-match-p regexp line))
                         my/agent-shell-retry-regexps)))
    line))

(defun my/agent-shell-retry--compose-prompt ()
  "Return the retry prompt, quoting the last user prompt when known."
  (if-let* ((last my/agent-shell-retry--last-user-prompt)
            ((not (string-empty-p (string-trim last)))))
      (format "%s

Last user prompt before the failure:

%s" my/agent-shell-retry-prompt last)
    my/agent-shell-retry-prompt))

(defun my/agent-shell-retry--on-chunk (event)
  "Append EVENT's text chunk to the bounded tail."
  (let ((combined (concat my/agent-shell-retry--tail
                          (or (map-nested-elt event '(:data :text-chunk)) ""))))
    (setq my/agent-shell-retry--tail
          (if (> (length combined) 4000)
              (substring combined -4000)
            combined))))

(defun my/agent-shell-retry--on-input-submitted (event)
  "Reset per-turn state; a user-authored prompt also resets the chain.
EVENT's `:prompt' (agent-shell 0.71.1+) is captured as the last user
prompt, so a retry turn can quote the request that failed."
  (if my/agent-shell-retry--own-submit
      (setq my/agent-shell-retry--own-submit nil)
    (when (timerp my/agent-shell-retry--timer)
      (cancel-timer my/agent-shell-retry--timer))
    (setq my/agent-shell-retry--timer nil)
    (setq my/agent-shell-retry--attempt 0)
    (when-let* ((prompt (map-nested-elt event '(:data :prompt))))
      (setq my/agent-shell-retry--last-user-prompt prompt)))
  (setq my/agent-shell-retry--tail ""))

(defun my/agent-shell-retry--on-turn-complete (event)
  "Schedule a retry when EVENT ended a turn on a retriable final line."
  (when-let* (((equal (map-nested-elt event '(:data :stop-reason)) "end_turn"))
              ((< my/agent-shell-retry--attempt my/agent-shell-retry-max-retries))
              (line (my/agent-shell-retry--tail-line my/agent-shell-retry--tail))
              (buffer (current-buffer)))
    (setq my/agent-shell-retry--attempt (1+ my/agent-shell-retry--attempt))
    (let ((delay (or (nth (1- my/agent-shell-retry--attempt)
                          my/agent-shell-retry-backoff-seconds)
                     (car (last my/agent-shell-retry-backoff-seconds))
                     0)))
      (message "agent-shell auto-retry: %s (retry %d/%d in %ds)"
               line my/agent-shell-retry--attempt
               my/agent-shell-retry-max-retries delay)
      (setq my/agent-shell-retry--timer
            (run-at-time
             delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq my/agent-shell-retry--timer nil)
                   (setq my/agent-shell-retry--own-submit t)
                   (message "agent-shell auto-retry: sending retry %d/%d"
                            my/agent-shell-retry--attempt
                            my/agent-shell-retry-max-retries)
                   (agent-shell-prompt-queue
                    (my/agent-shell-retry--compose-prompt))))))))))

(defun my/agent-shell-retry--subscribe ()
  "Subscribe the current shell buffer's retry event handlers."
  (setq my/agent-shell-retry--subscriptions
        (list (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'agent-message-chunk
                                        :on-event #'my/agent-shell-retry--on-chunk)
              (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'input-submitted
                                        :on-event #'my/agent-shell-retry--on-input-submitted)
              (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'turn-complete
                                        :on-event #'my/agent-shell-retry--on-turn-complete))))

(defun my/agent-shell-retry--teardown ()
  "Unsubscribe, cancel any pending retry, and reset per-buffer state."
  (dolist (token my/agent-shell-retry--subscriptions)
    (ignore-errors (agent-shell-unsubscribe :subscription token)))
  (setq my/agent-shell-retry--subscriptions nil)
  (when (timerp my/agent-shell-retry--timer)
    (cancel-timer my/agent-shell-retry--timer))
  (setq my/agent-shell-retry--timer nil)
  (setq my/agent-shell-retry--attempt 0)
  (setq my/agent-shell-retry--tail ""))

(defun my/agent-shell-retry--setup ()
  "Reset state and subscribe, deferring until the shell is initialized.
Mode hooks can run before the shell's state exists, so when it is not
ready yet, subscription is retried once shortly after."
  (my/agent-shell-retry--teardown)
  (if (bound-and-true-p agent-shell--state)
      (my/agent-shell-retry--subscribe)
    (run-at-time 0.1 nil
                 (lambda (buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (and my/agent-shell-retry-mode
                                  (null my/agent-shell-retry--subscriptions))
                         (my/agent-shell-retry--subscribe)))))
                 (current-buffer))))

(define-minor-mode my/agent-shell-retry-mode
  "Auto-retry turns that end with a retriable error in this shell.

When a turn completes normally but its final output line matches
`my/agent-shell-retry-regexps' (e.g. Cursor's
\"Error: RetriableError: ...\"), resend a continue prompt (quoting the
last user prompt) after a short backoff, up to
`my/agent-shell-retry-max-retries' times per user prompt."
  :lighter " Retry"
  :group 'agent-shell
  (if my/agent-shell-retry-mode
      (if (derived-mode-p 'agent-shell-mode)
          (my/agent-shell-retry--setup)
        (setq my/agent-shell-retry-mode nil)
        (user-error "Not in an agent-shell buffer"))
    (my/agent-shell-retry--teardown)))

(defun my/agent-shell-retry-mode--turn-on ()
  "Turn on `my/agent-shell-retry-mode' in agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (my/agent-shell-retry-mode 1)))

(define-globalized-minor-mode my/agent-shell-global-retry-mode
  my/agent-shell-retry-mode my/agent-shell-retry-mode--turn-on
  :group 'agent-shell)

(provide 'agent-shell-auto-retry)

;;; agent-shell-auto-retry.el ends here
