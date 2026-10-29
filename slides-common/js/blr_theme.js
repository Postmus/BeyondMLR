const COLORS = {
  background: "F4F1EA",
  primary: "173F48",
  primaryDark: "102F36",
  secondary: "6F9878",
  secondaryLight: "A9C4AD",
  accent: "D58A3D",
  accentLight: "F1D4AF",
  text: "19343B",
  muted: "5C6E72",
  card: "FFFFFF",
  cardLine: "D3DDD6",
  surfaceMuted: "E6EEE8",
  surfaceWarm: "F5E7D5",
  surfaceCool: "E2EEF0",
  onPrimary: "FAF8F2",
  onPrimaryMuted: "C8D8D2",
};

const FONT_FAMILIES = {
  sans: "Liberation Sans",
};

const TYPOGRAPHY = {
  slideTitle: { fontFace: FONT_FAMILIES.sans, fontSize: 29, bold: true },
  heroEyebrow: { fontFace: FONT_FAMILIES.sans, fontSize: 14, bold: true },
  heroTitle: { fontFace: FONT_FAMILIES.sans, fontSize: 36, bold: true },
  heroMeta: { fontFace: FONT_FAMILIES.sans, fontSize: 17 },
  sectionLabel: { fontFace: FONT_FAMILIES.sans, fontSize: 16, italic: true },
  sectionTitle: { fontFace: FONT_FAMILIES.sans, fontSize: 39, bold: true },
  body: { fontFace: FONT_FAMILIES.sans, fontSize: 18 },
  bodySmall: { fontFace: FONT_FAMILIES.sans, fontSize: 14 },
  panelTitle: { fontFace: FONT_FAMILIES.sans, fontSize: 16, bold: true },
  caption: { fontFace: FONT_FAMILIES.sans, fontSize: 11, italic: true },
  footer: { fontFace: FONT_FAMILIES.sans, fontSize: 11 },
};

const SPACING = {
  edge: 0.56,
  gutter: 0.28,
  cardPad: 0.24,
  smallGap: 0.12,
  footerH: 0.18,
};

const STROKES = {
  card: { color: COLORS.cardLine, width: 1.1 },
  primary: { color: COLORS.primary, width: 1.2 },
  accent: { color: COLORS.accent, width: 1.2 },
};

const RADII = {
  card: 0.08,
  note: 0.06,
};

const LAYOUT = {
  slideW: 13.333,
  slideH: 7.5,
  title: { x: 0.56, y: 0.24, w: 12.21, h: 0.62 },
  body: { x: 0.56, y: 1.04, w: 12.21, h: 5.98 },
  left: { x: 0.56, y: 1.04, w: 5.965, h: 5.98 },
  right: { x: 6.805, y: 1.04, w: 5.965, h: 5.98 },
  footer: { x: 0.56, y: 7.18, w: 12.21, h: 0.22 },
};

const MASTERS = {
  content: "BLR_CONTENT",
  section: "BLR_SECTION",
};

module.exports = {
  COLORS,
  FONT_FAMILIES,
  LAYOUT,
  MASTERS,
  RADII,
  SPACING,
  STROKES,
  TYPOGRAPHY,
};
