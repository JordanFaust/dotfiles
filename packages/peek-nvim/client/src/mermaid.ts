import Mermaid from 'https://cdn.esm.sh/mermaid@9?no-dts';
import { getInjectConfig } from './util.ts';

declare const mermaid: typeof Mermaid;

function init() {
  const peek = getInjectConfig();

  mermaid.initialize({
    startOnLoad: false,
    theme: peek?.theme === 'light' ? 'neutral' : 'dark',
  });
}

async function render(id: string, definition: string, container: Element) {
  try {
    return (await mermaid.render(id, definition, container)).svg;
  } catch { /**/ }
}

export default { init, render };
