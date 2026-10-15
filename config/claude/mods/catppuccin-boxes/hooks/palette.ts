// Catppuccin Macchiato — https://catppuccin.com/palette
export const macchiato = {
  rosewater: '#f4dbd6',
  flamingo: '#f0c6c6',
  pink: '#f5bde6',
  mauve: '#c6a0f6',
  red: '#ed8796',
  maroon: '#ee99a0',
  peach: '#f5a97f',
  yellow: '#eed49f',
  green: '#a6da95',
  teal: '#8bd5ca',
  sky: '#91d7e3',
  sapphire: '#7dc4e4',
  blue: '#8aadf4',
  lavender: '#b7bdf8',
  text: '#cad3f5',
  subtext1: '#b8c0e0',
  subtext0: '#a5adcb',
  overlay2: '#939ab7',
  overlay1: '#8087a2',
  overlay0: '#6e738d',
  surface2: '#5b6078',
  surface1: '#494d64',
  surface0: '#363a4f',
  base: '#24273a',
  mantle: '#1e2030',
  crust: '#181926',
} as const

// What each part of the UI is drawn in
export const theme = {
  // red bar on a faint rose tint (12% red over base)
  prompt: { bar: macchiato.red, bg: '#3c3345', fg: macchiato.text },
  // a card raised just above base (35% of the way to surface0)
  reply: { bg: '#2a2e41', fg: macchiato.text },
  md: {
    code: macchiato.green,
    bold: macchiato.text,
    italic: macchiato.flamingo,
    heading: macchiato.red,
    link: macchiato.blue,
    bullet: macchiato.red,
    quote: macchiato.subtext0,
    rule: macchiato.surface2,
  },
} as const
