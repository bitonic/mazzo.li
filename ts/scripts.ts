import "katex/dist/katex.css";
import "@fontsource/ibm-plex-sans"
import "@fontsource/ibm-plex-sans"
import "@fontsource/ibm-plex-sans/500.css";
import "@fontsource/ibm-plex-sans/500-italic.css";
import "@fontsource/ibm-plex-serif"
import "@fontsource/ibm-plex-serif/500.css";
import "@fontsource/ibm-plex-serif/400-italic.css";
import "@fontsource/ibm-plex-mono"
import "@fontsource/ibm-plex-mono/500.css"

document.addEventListener("DOMContentLoaded", async () => {
  const katex = await import(/* webpackChunkName: "katex" */ "katex");
  
  const mathElements = document.getElementsByClassName("math");
  for (let i = 0; i < mathElements.length; i++) {
    const texText = mathElements[i].firstChild;
    if (mathElements[i].tagName == "SPAN") {
      katex.render((texText as any).data, mathElements[i] as HTMLElement, {
        displayMode: mathElements[i].classList.contains('display'),
        throwOnError: false,
        macros: [],
      });
    }
  }
});

declare global {
  interface Window {
    enablePresentationMode: () => void;
  }
}

window.enablePresentationMode = () => {
  document.addEventListener("DOMContentLoaded", async () => {
    // break up the article into sections
    const article = document.getElementsByClassName("article")[0];
    if (!article) { return; }
    const sections: HTMLDivElement[] = [];
    const createSection = () => {
      const section = document.createElement("div");
      section.classList.add("presentation-section");
      sections.push(section);
    }
    createSection();
    const addToSection = (node: Node) => {
      sections[sections.length-1]!.appendChild(node);
    }
    while (article.firstChild) {
      const child = article.firstChild;
      if (child instanceof HTMLHRElement) {
        createSection();
        article.removeChild(child);
      } else {
        addToSection(child);
      }
    }
    for (const section of sections) {
      article.appendChild(section);
    }
    let currentSection = 0;
    if (document.location.hash && !isNaN(parseInt(document.location.hash.slice(1)))) {
      currentSection = parseInt(document.location.hash.slice(1));
      sections[currentSection].scrollIntoView();
    }
    document.body.addEventListener("keydown", (ev) => {
      let scrollTo = false;
      if (ev.key === "PageDown") {
        if (currentSection < sections.length - 1) {
          currentSection++;
          scrollTo = true;
        }
      } else if (ev.key === "PageUp") {
        if (currentSection > 0) {
          currentSection--;
          scrollTo = true;
        }
      } else if (ev.key === " ") {
        scrollTo = currentSection >= 0 && currentSection < sections.length;
      }
      if (scrollTo) {
        ev.preventDefault();
        sections[currentSection].scrollIntoView();
        document.location.hash = currentSection.toString();
      }
    })
  });
}
