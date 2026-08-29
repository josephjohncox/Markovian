// Configure the pinned local MathJax bundle before it loads.
//
// mdBook applies Markdown emphasis and smart punctuation before MathJax sees a
// display block. TeX subscripts such as `K_p ... L_q` can therefore arrive as
// `K<em>p ... L</em>q`, which prevents MathJax from finding the delimiters.
// Recover the original punctuation before the default startup typesetter runs.
function markdownMathText(node) {
  if (node.nodeType === Node.TEXT_NODE) {
    return node.data.replaceAll("’", "'");
  }

  const content = Array.from(node.childNodes, markdownMathText).join("");
  if (node.nodeName === "EM") {
    return `_${content}_`;
  }
  if (node.nodeName === "STRONG") {
    return `**${content}**`;
  }
  return content;
}

function restoreMarkdownMath() {
  for (const paragraph of document.querySelectorAll("p")) {
    const text = paragraph.textContent.trim();
    if (text.startsWith("\\[") && text.endsWith("\\]")) {
      paragraph.textContent = markdownMathText(paragraph);
    }
  }
}

window.MathJax = {
  tex: {
    inlineMath: [["\\(", "\\)"]],
    displayMath: [["\\[", "\\]"]],
    processEscapes: true,
  },
  svg: {
    fontCache: "global",
  },
  options: {
    skipHtmlTags: ["script", "noscript", "style", "textarea", "pre", "code"],
  },
  startup: {
    pageReady: () => {
      restoreMarkdownMath();
      return window.MathJax.startup.defaultPageReady();
    },
  },
};
