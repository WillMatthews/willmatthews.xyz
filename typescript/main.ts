const paragraph = document.querySelector("p:not([class])");
if (paragraph) {
  const text = paragraph.textContent;
  const firstSentence = text?.split(". ")[0];
  if (firstSentence) {
    const capitalizedSentence =
      firstSentence.charAt(0).toUpperCase() + firstSentence.slice(1);
    paragraph.textContent = text.replace(firstSentence, capitalizedSentence);
  }
}
