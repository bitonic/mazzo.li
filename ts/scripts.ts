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

// KaTeX
document.addEventListener("DOMContentLoaded", async () => {
  const katex = await import(/* webpackChunkName: "katex" */ "katex");
  
  const mathElements = document.getElementsByClassName("math");
  for (let i = 0; i < mathElements.length; i++) {
    const texText = mathElements[i].firstChild;
    if (mathElements[i].tagName == "SPAN") {
      const opts: katex.KatexOptions = {
        displayMode: mathElements[i].classList.contains("display"),
        throwOnError: false,
        macros: [],
        fleqn: (window as any).fleqn || false,
      }
      katex.render((texText as any).data, mathElements[i] as HTMLElement, opts);
    }
  }
});

// Comment box
document.addEventListener("DOMContentLoaded", async () => {
  const commentContainer = document.getElementById("comments")
  if (!commentContainer) { return ; }

  const comments = await import(/* webpackChunkName: "comments" */ "comments")
  comments.run(commentContainer)
})

declare global {
  interface Window {
    enablePresentationMode: () => void;
    lanczosImageScaling: () => void;
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

window.lanczosImageScaling = () => {
  const lastDevicePixelRatio: null | number = null;
  const adjustImages = () => {
    if (lastDevicePixelRatio !== window.devicePixelRatio) {
      const update = (cls: string, edge: number) => {
        const els = document.querySelectorAll(cls);
        const pxs = edge/window.devicePixelRatio;
        for (let i = 0; i < els.length; i++) {
          const el = els[i] as HTMLImageElement;
          el.style.width = `${pxs}px`;
          el.style.height = `${pxs}px`;
        }
      }
      update(".sample img", 300)
      update(".sample", 300)
      update(".sample-big img", 600)
    }
    window.requestAnimationFrame(adjustImages);
  };
  window.requestAnimationFrame(adjustImages);
}
