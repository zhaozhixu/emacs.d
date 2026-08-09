;;; agent-shell-auto-retry.el --- Auto-retry turns that end with a retriable error -*- lexical-binding: t; -*-

;;; Code:

(require 'agent-shell)
(require 'agent-shell-prompt-queue)
(require 'map)
(require 'seq)

;; Auto-retry turns that end with a retriable error.
;;
;; Some agents (e.g. Cursor) catch a transient stream error internally,
;; print it as ordinary output ("Error: RetriableError: ..."), and end
;; the turn as if it succeeded.  Built on agent-shell's public event API
;; (`agent-message-chunk' added in 0.70.2 for this use case) after
;; upstream preferred this live outside core.
;;
;; Enable per shell with M-x my/agent-shell-retry-enable, or for all new
;; shells with (setq my/agent-shell-retry-auto-enable t).  No interrupt
;; event exists, so interrupting during backoff does not cancel the
;; pending retry; use M-x my/agent-shell-retry-disable if one slips
;; through.

(defvar my/agent-shell-retry-max-retries 2
  "Maximum auto-retry turns per user prompt before giving up.")

(defvar my/agent-shell-retry-backoff-seconds '(2 5)
  "Backoff delays indexed by 1-based retry number; last entry repeats.")

(defvar my/agent-shell-retry-regexps '("^Error: RetriableError:")
  "Case-insensitive patterns a turn's final line must match to retry.")

(defvar my/agent-shell-retry-prompt
  "[auto-retry] The previous turn failed with a retriable error. Please continue working on the task where you left off."
  "Prompt sent as a new user turn to recover from a retriable error.")

(defvar my/agent-shell-retry-auto-enable nil
  "Non-nil to enable auto-retry in every new agent-shell buffer.")

(defvar-local my/agent-shell-retry--tail ""
  "Bounded tail of the current turn's agent message text.")

(defvar-local my/agent-shell-retry--attempt 0
  "Auto-retries sent for the current user prompt.")

(defvar-local my/agent-shell-retry--last-user-prompt nil
  "Most recent user-authored prompt, captured from `comint-input-ring'.")

(defvar-local my/agent-shell-retry--timer nil
  "Pending retry timer, if any.")

(defvar-local my/agent-shell-retry--own-submit nil
  "Non-nil while the next `input-submitted' is our own retry prompt.")

(defvar-local my/agent-shell-retry--subscriptions nil
  "Event subscription tokens while enabled.")

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

(defun my/agent-shell-retry--on-input-submitted (_event)
  "Reset per-turn state; a user-authored prompt also resets the chain."
  (if my/agent-shell-retry--own-submit
      (setq my/agent-shell-retry--own-submit nil)
    (when (timerp my/agent-shell-retry--timer)
      (cancel-timer my/agent-shell-retry--timer))
    (setq my/agent-shell-retry--timer nil)
    (setq my/agent-shell-retry--attempt 0)
    (when (and (bound-and-true-p comint-input-ring)
               (ring-p comint-input-ring)
               (not (ring-empty-p comint-input-ring)))
      (setq my/agent-shell-retry--last-user-prompt
            (substring-no-properties (ring-ref comint-input-ring 0)))))
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

(defun my/agent-shell-retry-enable ()
  "Enable auto-retry in the current agent-shell buffer."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (my/agent-shell-retry-disable)
  (setq my/agent-shell-retry--subscriptions
        (list (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'agent-message-chunk
                                        :on-event #'my/agent-shell-retry--on-chunk)
              (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'input-submitted
                                        :on-event #'my/agent-shell-retry--on-input-submitted)
              (agent-shell-subscribe-to :shell-buffer (current-buffer)
                                        :event 'turn-complete
                                        :on-event #'my/agent-shell-retry--on-turn-complete)))
  (message "agent-shell auto-retry enabled in %s" (buffer-name)))

(defun my/agent-shell-retry-disable ()
  "Disable auto-retry in the current agent-shell buffer."
  (interactive)
  (dolist (token my/agent-shell-retry--subscriptions)
    (ignore-errors (agent-shell-unsubscribe :subscription token)))
  (setq my/agent-shell-retry--subscriptions nil)
  (when (timerp my/agent-shell-retry--timer)
    (cancel-timer my/agent-shell-retry--timer))
  (setq my/agent-shell-retry--timer nil)
  (setq my/agent-shell-retry--attempt 0)
  (setq my/agent-shell-retry--tail ""))

(defun my/agent-shell-retry--maybe-enable ()
  "Enable auto-retry in new shells when `my/agent-shell-retry-auto-enable'."
  (when my/agent-shell-retry-auto-enable
    ;; Defer so the shell's state is fully initialized before subscribing.
    (run-at-time 0 nil
                 (lambda (buffer)
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (derived-mode-p 'agent-shell-mode)
                         (my/agent-shell-retry-enable)))))
                 (current-buffer))))

(add-hook 'agent-shell-mode-hook #'my/agent-shell-retry--maybe-enable)

(provide 'agent-shell-auto-retry)

;;; agent-shell-auto-retry.el ends here
