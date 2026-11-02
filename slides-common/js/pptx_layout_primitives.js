const fs = require("fs");

const {
  autoFontSize,
  calcTextBox,
  imageSizingContain,
  imageSizingCrop,
} = require("../../lectures_2627/week1/pptxgenjs_helpers");
const {
  COLORS,
  LAYOUT,
  MASTERS,
  RADII,
  SPACING,
  STROKES,
  TYPOGRAPHY,
} = require("./blr_theme");

function defineTemplateMasters(pptx) {
  pptx.defineSlideMaster({
    title: MASTERS.content,
    background: { color: COLORS.background },
  });
  pptx.defineSlideMaster({
    title: MASTERS.section,
    background: { color: COLORS.primary },
  });
}

function getSlots({ reverse = false, captionHeight = 0, noteHeight = 0 } = {}) {
  const left = { ...LAYOUT.left };
  const right = { ...LAYOUT.right };
  const text = reverse ? right : left;
  const visual = reverse ? left : right;
  const reserved = Math.max(0, captionHeight) + Math.max(0, noteHeight);
  const mainH = Math.max(0.4, text.h - reserved - (reserved > 0 ? SPACING.smallGap : 0));

  return {
    title: { ...LAYOUT.title },
    body: { ...LAYOUT.body },
    left,
    right,
    text: { ...text, h: mainH },
    visual: { ...visual, h: visual.h - Math.max(0, captionHeight) },
    note: noteHeight > 0
      ? {
        x: text.x,
        y: text.y + mainH + SPACING.smallGap,
        w: text.w,
        h: noteHeight,
      }
      : null,
    caption: captionHeight > 0
      ? {
        x: visual.x,
        y: visual.y + visual.h - captionHeight,
        w: visual.w,
        h: captionHeight,
      }
      : null,
    footer: { ...LAYOUT.footer },
  };
}

function addBase(slide, color = COLORS.background) {
  slide.background = { color };
}

function addCard(slide, box, { fill = COLORS.card, line = STROKES.card } = {}) {
  slide.addShape("roundRect", {
    ...box,
    rectRadius: RADII.card,
    fill: { color: fill },
    line,
  });
}

function addSlideTitle(slide, title, box = LAYOUT.title) {
  const textOptions = autoFontSize(title, TYPOGRAPHY.slideTitle.fontFace, {
    ...box,
    minFontSize: 24,
    maxFontSize: TYPOGRAPHY.slideTitle.fontSize,
    mode: "auto",
    bold: true,
    color: COLORS.primary,
    margin: 0,
    valign: "mid",
    breakLine: false,
  });
  slide.addText(title, textOptions);
}

function measuredTextHeight(text, fontSize, width, {
  bold = false,
  italic = false,
  paraSpaceAfter = 0,
  margin = 0,
  padding = 0.02,
} = {}) {
  if (!text || (Array.isArray(text) && text.length === 0)) return 0;
  return calcTextBox(fontSize, {
    text,
    w: width,
    fontFace: TYPOGRAPHY.body.fontFace,
    bold,
    italic,
    paraSpaceAfter,
    margin,
    padding,
  }).h;
}

function addTextBox(slide, {
  box,
  text = "",
  title = "",
  fontSize = TYPOGRAPHY.body.fontSize,
  minFontSize = 13,
  valign = "top",
  paraSpaceAfter = 4,
  fill = COLORS.card,
  line = STROKES.card,
} = {}) {
  addCard(slide, box, { fill, line });
  const titleH = title ? 0.42 : 0;
  const contentBox = {
    x: box.x + SPACING.cardPad,
    y: box.y + 0.22 + titleH,
    w: box.w - (2 * SPACING.cardPad),
    h: box.h - 0.42 - titleH,
  };

  if (title) {
    const titleBox = {
      x: box.x + SPACING.cardPad,
      y: box.y + 0.17,
      w: box.w - (2 * SPACING.cardPad),
      h: 0.3,
    };
    slide.addText(title, autoFontSize(title, TYPOGRAPHY.panelTitle.fontFace, {
      ...titleBox,
      minFontSize: 13,
      maxFontSize: TYPOGRAPHY.panelTitle.fontSize,
      mode: "auto",
      bold: true,
      color: COLORS.primary,
      margin: 0,
    }));
  }

  const naturalH = measuredTextHeight(text, fontSize, contentBox.w, { margin: 0 });
  const resolvedBox = {
    ...contentBox,
    h: Math.min(contentBox.h, Math.max(0.3, naturalH)),
  };
  slide.addText(text, autoFontSize(text, TYPOGRAPHY.body.fontFace, {
    ...resolvedBox,
    minFontSize,
    maxFontSize: fontSize,
    mode: "auto",
    color: COLORS.text,
    margin: 0,
    valign: "top",
    breakLine: true,
  }));
}

function normalizeBullets(bullets) {
  return bullets.flatMap((entry, index) => {
    if (typeof entry === "string") {
      return [{
        text: entry,
        options: { bullet: true, breakLine: index < bullets.length - 1 },
      }];
    }

    if (Array.isArray(entry.textRuns)) {
      return entry.textRuns.map((run, runIndex) => ({
        text: run.text || "",
        options: {
          ...(run.options || {}),
          ...(runIndex === 0 && !Object.prototype.hasOwnProperty.call(run.options || {}, "bullet")
            ? { bullet: Object.prototype.hasOwnProperty.call(entry, "bullet") ? entry.bullet : true }
            : {}),
          ...(runIndex === entry.textRuns.length - 1
            ? { breakLine: Object.prototype.hasOwnProperty.call(entry, "breakLine")
              ? entry.breakLine
              : index < bullets.length - 1 }
            : {}),
        },
      }));
    }

    return [{
      text: entry.text || "",
      options: {
        bullet: Object.prototype.hasOwnProperty.call(entry, "bullet") ? entry.bullet : true,
        indentLevel: Number.isInteger(entry.indentLevel) ? entry.indentLevel : undefined,
        bold: entry.bold,
        italic: entry.italic,
        breakLine: Object.prototype.hasOwnProperty.call(entry, "breakLine")
          ? entry.breakLine
          : index < bullets.length - 1,
      },
    }];
  });
}

function addBulletBox(slide, {
  box,
  bullets = [],
  title = "",
  fontSize = TYPOGRAPHY.body.fontSize,
  minFontSize = 13,
  valign = "top",
  paraSpaceAfter = 4,
  fill = COLORS.card,
  line = STROKES.card,
} = {}) {
  addCard(slide, box, { fill, line });
  const titleH = title ? 0.42 : 0;

  if (title) {
    const titleBox = {
      x: box.x + SPACING.cardPad,
      y: box.y + 0.17,
      w: box.w - (2 * SPACING.cardPad),
      h: 0.3,
    };
    slide.addText(title, autoFontSize(title, TYPOGRAPHY.panelTitle.fontFace, {
      ...titleBox,
      minFontSize: 13,
      maxFontSize: TYPOGRAPHY.panelTitle.fontSize,
      mode: "auto",
      bold: true,
      color: COLORS.primary,
      margin: 0,
    }));
  }

  const runs = normalizeBullets(bullets);
  const contentBox = {
    x: box.x + SPACING.cardPad,
    y: box.y + 0.23 + titleH,
    w: box.w - (2 * SPACING.cardPad),
    h: box.h - 0.46 - titleH,
  };
  slide.addText(runs, autoFontSize(runs, TYPOGRAPHY.body.fontFace, {
    ...contentBox,
    minFontSize,
    maxFontSize: fontSize,
    mode: "auto",
    color: COLORS.text,
    margin: 0,
    valign,
    paraSpaceAfter,
    breakLine: true,
  }));
}

function addNoteBox(slide, {
  box,
  text = "",
  title = "",
  fontSize = TYPOGRAPHY.bodySmall.fontSize,
  minFontSize = 10,
  titleFontSize = 13,
  fill = COLORS.surfaceMuted,
  line = STROKES.primary,
} = {}) {
  slide.addShape("roundRect", {
    ...box,
    rectRadius: RADII.note,
    fill: { color: fill },
    line,
  });
  const titleH = title ? 0.3 : 0;
  if (title) {
    slide.addText(title, {
      x: box.x + 0.18,
      y: box.y + 0.1,
      w: box.w - 0.36,
      h: 0.22,
      ...TYPOGRAPHY.panelTitle,
      fontSize: titleFontSize,
      color: COLORS.primary,
      margin: 0,
    });
  }
  const textBox = {
    x: box.x + 0.18,
    y: box.y + 0.1 + titleH,
    w: box.w - 0.36,
    h: box.h - 0.2 - titleH,
  };
  slide.addText(text, autoFontSize(text, TYPOGRAPHY.bodySmall.fontFace, {
    ...textBox,
    minFontSize,
    maxFontSize: fontSize,
    mode: "auto",
    color: COLORS.primary,
    margin: 0,
    valign: title ? "top" : "mid",
  }));
}

function addAssetFrame(slide, {
  box,
  assetPath,
  fit = "contain",
  framed = true,
  fill = COLORS.card,
  line = STROKES.card,
} = {}) {
  if (!assetPath || !fs.existsSync(assetPath)) {
    throw new Error(`Asset not found: ${assetPath || "<empty path>"}`);
  }
  if (framed) addCard(slide, box, { fill, line });
  const inset = framed ? 0.08 : 0;
  const inner = {
    x: box.x + inset,
    y: box.y + inset,
    w: box.w - (2 * inset),
    h: box.h - (2 * inset),
  };
  const placement = fit === "cover"
    ? imageSizingCrop(assetPath, inner.x, inner.y, inner.w, inner.h)
    : imageSizingContain(assetPath, inner.x, inner.y, inner.w, inner.h);
  slide.addImage({ path: assetPath, ...placement });
}

function addCaption(slide, { box, text = "", fontSize = TYPOGRAPHY.caption.fontSize, minFontSize = 9 } = {}) {
  const textBox = {
    x: box.x + 0.08,
    y: box.y + 0.04,
    w: box.w - 0.16,
    h: box.h - 0.08,
  };
  slide.addText(text, autoFontSize(text, TYPOGRAPHY.caption.fontFace, {
    ...textBox,
    minFontSize,
    maxFontSize: fontSize,
    mode: "auto",
    italic: true,
    color: COLORS.muted,
    margin: 0,
    align: "center",
    valign: "mid",
  }));
}

function addSlideNumber(slide, {
  color = COLORS.muted,
} = {}) {
  slide.slideNumber = {
    ...LAYOUT.footer,
    ...TYPOGRAPHY.footer,
    color,
    margin: 0,
    align: "right",
    valign: "middle",
  };
}

function addFooter(slide, _shortTitle = "") {
  // Retain the legacy helper name for existing builders, but use a native
  // PowerPoint slide-number field so numbering follows the final slide order.
  addSlideNumber(slide);
}

function addBrandedTitleSlide(pptx, {
  courseLabel = "PRINCIPLES OF LABORATORY ANIMAL SCIENCE",
  mainTitle = "Methodological and statistical aspects",
  author = "Dr Douwe Postmus",
  affiliation = "UMCG · Department of Epidemiology",
  badge = "LAS",
} = {}) {
  const slide = pptx.addSlide();

  // Use a canvas-sized shape instead of a slide background so the generated
  // title slide contains only ordinary, positive-extent drawing geometry.
  slide.addShape("rect", {
    x: 0,
    y: 0,
    w: LAYOUT.slideW,
    h: LAYOUT.slideH,
    fill: { color: COLORS.background },
    line: { color: COLORS.background, width: 0 },
  });
  slide.addShape("rect", {
    x: 7.45,
    y: 0,
    w: LAYOUT.slideW - 7.45,
    h: LAYOUT.slideH,
    fill: { color: COLORS.primary },
    line: { color: COLORS.primary, width: 0 },
  });

  slide.addShape("ellipse", {
    x: 8.12,
    y: 0.76,
    w: 4.55,
    h: 4.55,
    fill: { color: COLORS.secondary, transparency: 18 },
    line: { color: COLORS.secondary, transparency: 100, width: 0 },
  });
  slide.addShape("ellipse", {
    x: 9.22,
    y: 1.86,
    w: 2.35,
    h: 2.35,
    fill: { color: COLORS.primaryDark, transparency: 6 },
    line: { color: COLORS.onPrimaryMuted, transparency: 55, width: 1.2 },
  });
  slide.addShape("ellipse", {
    x: 9.88,
    y: 2.52,
    w: 1.03,
    h: 1.03,
    fill: { color: COLORS.accent, transparency: 3 },
    line: { color: COLORS.accent, width: 0 },
  });

  const observations = [
    { x: 8.18, y: 5.56, r: 0.16 },
    { x: 8.82, y: 5.12, r: 0.12 },
    { x: 9.42, y: 5.68, r: 0.18 },
    { x: 10.12, y: 5.04, r: 0.13 },
    { x: 10.84, y: 5.45, r: 0.17 },
    { x: 11.48, y: 4.88, r: 0.12 },
    { x: 12.06, y: 5.24, r: 0.16 },
  ];
  observations.forEach(({ x, y, r }, index) => {
    slide.addShape("ellipse", {
      x,
      y,
      w: r,
      h: r,
      fill: { color: index === 4 ? COLORS.accentLight : COLORS.onPrimaryMuted },
      line: { color: index === 4 ? COLORS.accentLight : COLORS.onPrimaryMuted, width: 0 },
    });
  });

  // `lineInv` gives the rising diagonal without a negative height or flip.
  slide.addShape("lineInv", {
    x: 8.14,
    y: 4.96,
    w: 4.2,
    h: 0.86,
    line: { color: COLORS.accentLight, width: 2.2, transparency: 8 },
  });

  slide.addText(courseLabel, autoFontSize(courseLabel, TYPOGRAPHY.heroEyebrow.fontFace, {
    x: 0.78,
    y: 0.82,
    w: 5.95,
    h: 0.38,
    minFontSize: 11,
    maxFontSize: TYPOGRAPHY.heroEyebrow.fontSize,
    mode: "auto",
    bold: true,
    charSpacing: 1.2,
    color: COLORS.secondary,
    margin: 0,
  }));

  slide.addText(mainTitle, autoFontSize(mainTitle, TYPOGRAPHY.heroTitle.fontFace, {
    x: 0.78,
    y: 1.44,
    w: 6.12,
    h: 2.15,
    minFontSize: 29,
    maxFontSize: TYPOGRAPHY.heroTitle.fontSize,
    mode: "auto",
    bold: true,
    color: COLORS.primary,
    margin: 0,
    valign: "mid",
    breakLine: true,
  }));

  slide.addShape("line", {
    x: 0.78,
    y: 4.08,
    w: 5.35,
    h: 0,
    line: { color: COLORS.cardLine, width: 1.5 },
  });

  slide.addText(author, {
    x: 0.78,
    y: 4.43,
    w: 4.6,
    h: 0.32,
    ...TYPOGRAPHY.heroMeta,
    bold: true,
    color: COLORS.text,
    margin: 0,
  });
  slide.addText(affiliation, {
    x: 0.78,
    y: 4.83,
    w: 5.3,
    h: 0.3,
    ...TYPOGRAPHY.bodySmall,
    color: COLORS.muted,
    margin: 0,
  });

  slide.addText(badge, {
    x: 9.22,
    y: 2.64,
    w: 2.35,
    h: 0.55,
    fontFace: TYPOGRAPHY.heroTitle.fontFace,
    fontSize: 25,
    bold: true,
    color: COLORS.onPrimary,
    margin: 0,
    align: "center",
    valign: "mid",
  });
  slide.addText("METHODS · DESIGN · STATISTICS", {
    x: 8.16,
    y: 6.38,
    w: 4.35,
    h: 0.28,
    fontFace: TYPOGRAPHY.heroEyebrow.fontFace,
    fontSize: 10.5,
    bold: true,
    charSpacing: 1.6,
    color: COLORS.onPrimaryMuted,
    margin: 0,
    align: "center",
  });

  return slide;
}

function addTextVisualSlide(pptx, {
  title,
  text,
  textTitle = "",
  assetPath,
  reverse = false,
  caption = "",
  note = "",
  noteTitle = "",
  footerShortTitle = "",
  fit = "contain",
} = {}) {
  const captionHeight = caption ? 0.46 : 0;
  const noteHeight = note ? 0.9 : 0;
  const slots = getSlots({ reverse, captionHeight, noteHeight });
  const slide = pptx.addSlide({ masterName: MASTERS.content });
  addBase(slide);
  addSlideTitle(slide, title);
  addTextBox(slide, { box: slots.text, text, title: textTitle });
  addAssetFrame(slide, { box: slots.visual, assetPath, fit });
  if (slots.caption) addCaption(slide, { box: slots.caption, text: caption });
  if (slots.note) addNoteBox(slide, { box: slots.note, text: note, title: noteTitle });
  addFooter(slide, footerShortTitle);
  return slide;
}

function addBulletVisualSlide(pptx, {
  title,
  bullets = [],
  bulletTitle = "",
  bulletFontSize = TYPOGRAPHY.body.fontSize,
  bulletMinFontSize = 13,
  assetPath,
  reverse = false,
  caption = "",
  note = "",
  noteTitle = "",
  noteHeight: requestedNoteHeight = 0.9,
  noteFontSize = TYPOGRAPHY.bodySmall.fontSize,
  noteTitleFontSize = 13,
  noteFill = COLORS.surfaceMuted,
  noteLine = STROKES.primary,
  footerShortTitle = "",
  fit = "contain",
  framed = true,
} = {}) {
  const captionHeight = caption ? 0.46 : 0;
  const noteHeight = note ? requestedNoteHeight : 0;
  const slots = getSlots({ reverse, captionHeight, noteHeight });
  const slide = pptx.addSlide({ masterName: MASTERS.content });
  addBase(slide);
  addSlideTitle(slide, title);
  addBulletBox(slide, {
    box: slots.text,
    bullets,
    title: bulletTitle,
    fontSize: bulletFontSize,
    minFontSize: bulletMinFontSize,
  });
  addAssetFrame(slide, { box: slots.visual, assetPath, fit, framed });
  if (slots.caption) addCaption(slide, { box: slots.caption, text: caption });
  if (slots.note) addNoteBox(slide, {
    box: slots.note,
    text: note,
    title: noteTitle,
    fontSize: noteFontSize,
    titleFontSize: noteTitleFontSize,
    fill: noteFill,
    line: noteLine,
  });
  addFooter(slide, footerShortTitle);
  return slide;
}

function addFigureSlide(pptx, {
  title,
  assetPath,
  caption = "",
  captionFontSize = TYPOGRAPHY.caption.fontSize,
  captionMinFontSize = 9,
  footerShortTitle = "",
  fit = "contain",
} = {}) {
  const captionHeight = caption ? 0.48 : 0;
  const slide = pptx.addSlide({ masterName: MASTERS.content });
  const figureBox = {
    ...LAYOUT.body,
    h: LAYOUT.body.h - captionHeight,
  };
  addBase(slide);
  addSlideTitle(slide, title);
  addAssetFrame(slide, { box: figureBox, assetPath, fit });
  if (caption) {
    addCaption(slide, {
      box: {
        x: LAYOUT.body.x,
        y: LAYOUT.body.y + LAYOUT.body.h - captionHeight,
        w: LAYOUT.body.w,
        h: captionHeight,
      },
      text: caption,
      fontSize: captionFontSize,
      minFontSize: captionMinFontSize,
    });
  }
  addFooter(slide, footerShortTitle);
  return slide;
}

function addComparisonSlide(pptx, {
  title,
  leftTitle = "",
  leftText = "",
  leftBullets,
  rightTitle = "",
  rightText = "",
  rightBullets,
  footerShortTitle = "",
} = {}) {
  const slide = pptx.addSlide({ masterName: MASTERS.content });
  addBase(slide);
  addSlideTitle(slide, title);
  if (leftBullets) {
    addBulletBox(slide, { box: LAYOUT.left, bullets: leftBullets, title: leftTitle });
  } else {
    addTextBox(slide, { box: LAYOUT.left, text: leftText, title: leftTitle });
  }
  if (rightBullets) {
    addBulletBox(slide, { box: LAYOUT.right, bullets: rightBullets, title: rightTitle });
  } else {
    addTextBox(slide, { box: LAYOUT.right, text: rightText, title: rightTitle });
  }
  addFooter(slide, footerShortTitle);
  return slide;
}

function addSectionSlide(pptx, {
  sectionLabel = "",
  sectionTitle = "",
} = {}) {
  const slide = pptx.addSlide();
  addBase(slide, COLORS.primary);
  slide.addText(sectionLabel, {
    x: 0.82,
    y: 2.02,
    w: 5.2,
    h: 0.34,
    ...TYPOGRAPHY.sectionLabel,
    color: COLORS.onPrimaryMuted,
    margin: 0,
  });
  slide.addText(sectionTitle, autoFontSize(sectionTitle, TYPOGRAPHY.sectionTitle.fontFace, {
    x: 0.82,
    y: 2.48,
    w: 8.6,
    h: 1.55,
    minFontSize: 29,
    maxFontSize: TYPOGRAPHY.sectionTitle.fontSize,
    mode: "auto",
    bold: true,
    color: COLORS.onPrimary,
    margin: 0,
    valign: "mid",
  }));

  // Decorative circles intentionally extend into one another but remain on-canvas.
  slide.addShape("ellipse", {
    x: 9.55,
    y: 0.55,
    w: 3.35,
    h: 3.35,
    fill: { color: COLORS.secondary, transparency: 22 },
    line: { color: COLORS.secondary, width: 0 },
  });
  slide.addShape("ellipse", {
    x: 10.35,
    y: 4.65,
    w: 1.75,
    h: 1.75,
    fill: { color: COLORS.accent, transparency: 30 },
    line: { color: COLORS.accent, width: 0 },
  });
  return slide;
}

// Semantic card rows keep repeated teaching comparisons on a shared geometry.
// Card backgrounds intentionally contain their heading and body text.
function addTeachingCards(slide, { cards, y = 2.1, h = 3.65, x = 0.72, w = 11.89, gap = 0.3 }) {
  const cardW = (w - gap * (cards.length - 1)) / cards.length;
  cards.forEach((card, index) => {
    const left = x + index * (cardW + gap);
    const accent = card.accent || COLORS.primary;
    addCard(slide, { x: left, y, w: cardW, h }, {
      fill: card.fill || COLORS.card,
      line: { color: accent, width: 1.1 },
    });
    slide.addText(card.title, {
      x: left + 0.28, y: y + 0.25, w: cardW - 0.56, h: 0.65,
      fontFace: TYPOGRAPHY.body.fontFace, fontSize: 20,
      bold: true, color: accent, margin: 0, valign: "mid",
    });
    slide.addText(card.body, {
      x: left + 0.28, y: y + 1.08, w: cardW - 0.56, h: h - 1.36,
      fontFace: TYPOGRAPHY.body.fontFace, fontSize: card.fontSize || 20,
      color: COLORS.text, margin: 0, valign: "top", paraSpaceAfter: 12,
    });
  });
}

// Numbered teaching points with a heading and short explanation in each row.
function addTeachingPointRows(slide, { points, x = 0.72, y = 1.06, w = 11.89, rowH = 1.17, gap = 0.07 }) {
  let nextTop = y;
  points.forEach((point, index) => {
    const top = nextTop;
    const height = point.height || rowH;
    nextTop += height + gap;
    const accent = point.accent || COLORS.primary;
    addCard(slide, { x, y: top, w, h: height }, {
      fill: point.fill || COLORS.card,
      line: { color: COLORS.cardLine, width: 1.0 },
    });
    slide.addText(String(index + 1), {
      x: x + 0.2, y: top + 0.33, w: 0.5, h: 0.42,
      fontFace: TYPOGRAPHY.body.fontFace, fontSize: 23, bold: true,
      color: accent, margin: 0, align: "center",
    });
    slide.addText(point.title, {
      x: x + 0.92, y: top + 0.13, w: w - 1.2, h: 0.32,
      fontFace: TYPOGRAPHY.body.fontFace, fontSize: 20, bold: true,
      color: accent, margin: 0,
    });
    slide.addText(point.body, {
      x: x + 0.92, y: top + 0.46, w: w - 1.2, h: height - 0.59,
      fontFace: TYPOGRAPHY.body.fontFace, fontSize: 18,
      color: COLORS.text, margin: 0, valign: "top",
    });
  });
}

// Shared conceptual expression, introduced in Part I and revisited in Part III.
function addContributionExpression(slide) {
  addCard(slide, { x: 0.72, y: 1.12, w: 11.89, h: 1.7 }, {
    fill: COLORS.surfaceMuted, line: { color: COLORS.cardLine, width: 1.1 },
  });
  slide.addText("Observed group difference = true treatment effect\n+ contributions from other observed and unobserved factors\nthat differ between the groups", {
    x: 1.04, y: 1.33, w: 11.25, h: 1.28,
    fontFace: TYPOGRAPHY.body.fontFace, fontSize: 23, bold: true,
    color: COLORS.primary, align: "center", margin: 0,
  });
}

module.exports = {
  addTeachingPointRows,
  addContributionExpression,
  addTeachingCards,
  COLORS,
  LAYOUT,
  MASTERS,
  TYPOGRAPHY,
  addAssetFrame,
  addBase,
  addBrandedTitleSlide,
  addBulletBox,
  addBulletVisualSlide,
  addCaption,
  addComparisonSlide,
  addFigureSlide,
  addFooter,
  addNoteBox,
  addSectionSlide,
  addSlideNumber,
  addSlideTitle,
  addTextBox,
  addTextVisualSlide,
  defineTemplateMasters,
  getSlots,
};
