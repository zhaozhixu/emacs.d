// Render a TeX math formula to SVG on stdout.
// Usage: node tex2svg.js 'tex source' > out.svg
require('mathjax').init({
  loader: { load: ['input/tex', 'output/svg'] },
}).then((MathJax) => {
  const node = MathJax.tex2svg(process.argv[2] || '', { display: true });
  const adaptor = MathJax.startup.adaptor;
  process.stdout.write(adaptor.outerHTML(adaptor.firstChild(node)));
}).catch((err) => {
  console.error(err.message);
  process.exit(1);
});
