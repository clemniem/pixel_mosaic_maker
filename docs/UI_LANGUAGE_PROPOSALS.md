# UI language: technical → normal language

Analysis of user-facing text that may sound too technical, with proposed friendlier alternatives.

---

## Overview & navigation

| Current | Proposal | Reason |
|--------|----------|--------|
| **Define Grid** (title) | **Layout** | "Grid" is technical; "Layout" = how the mosaic is split. |
| **Grid configs (plate layouts)** (card desc) | **How your mosaic is split into sections** | Explains purpose without "grid config" or "plate". |
| **Grid config** (editor title) | **Edit layout** | Clear action + concept. |
| **Create Palettes** | **Palettes** (or keep) | "Create" is implied by the screen. |
| **Color palettes** (card desc) | **Color palettes** (ok) or **Your color sets** | Slightly friendlier. |
| **Upload and manage pixel images** | **Upload and manage your images** | "Pixel" is redundant in this app. |
| **Mosaic Configurator** (title) | **Mosaic setup** | "Configurator" is jargony. |
| **Build configurations (grid + image + palette)** | **Choose layout, image and colors** | Plain language. |
| **Build config** (editor title) | **Mosaic setup** | Consistent. |
| **Mosaic Builder** (title) | **Build** or **Step-by-step build** | "Builder" is a bit technical. |
| **Step-by-step build runs** (card desc) | **Step-by-step building instructions** | "Runs" is dev jargon. |
| **Build** (screen title) | **Building steps** | Clarifies it’s the step view. |
| **Print Instructions** (title) | **Print** or **Print guide** | Shorter, clear. |
| **Generate PDF print instructions** (card desc) | **Create a printable PDF guide** | Normal language. |

---

## Print Instructions screen

| Current | Proposal |
|--------|----------|
| Choose a build config and set the booklet title, then generate the PDF. | Pick a mosaic setup, add a title, then create your PDF. |
| Build config (label) | Mosaic setup |
| No build configs saved. Create one from the BuildConfigs gallery. | No setups saved yet. Create one from Mosaic setup. |
| Grid overview (section title) | Layout preview |
| Step size (px) | Section size (pixels) |
| Only values that divide every plate width and height are enabled. | Only sizes that fit your layout evenly are available. |
| Hex (e.g. #fdfbe6). Used for all PDF pages. | Color code (e.g. #fdfbe6) for all pages. |
| Printer margin (mm) | Margin (mm) |
| White border on each side (for booklet printing). Default 3 mm. | White border around each page. Default 3 mm. |

---

## Grid / Layout editor (GridConfigScreen)

| Current | Proposal |
|--------|----------|
| ← GridConfigs (back) | ← Layouts |
| Grid is not a rectangle (rows have different total widths or columns different heights). Choose how to fix it; then press Save. | Your layout doesn’t line up (rows or columns have different lengths). Choose how to fix it, then Save. |
| Enlarge existing cells | Stretch existing sections |
| Add new plates to fill gaps | Add new sections to fill gaps |
| Define the grid of plates (Lego-style). Instructions are generated per plate. Choose by rows or by columns; each row/column can have a different number of cells. | Set up how your mosaic is split into sections (like LEGO plates). You can define by rows or columns; each can have a different number of sections. |
| Define by: | Set up by: |
| Preview · 320×240 px · 4 plate(s) | Preview · 320×240 pixels · 4 section(s) |
| − cell / + cell | − section / + section |
| When on, all cells in this row share the same width (tooltip) | When on, all sections in this row share the same width |
| Unnamed grid (default name) | Unnamed layout |

---

## Mosaic setup editor (BuildConfigScreen)

| Current | Proposal |
|--------|----------|
| ← Build configs (back) | ← Mosaic setups |
| Grid (label) | Layout |
| No grids saved. / No images saved. / No palettes saved. | No layouts saved. / No images saved. / No palettes saved. |
| Offset X: / Offset Y: | Position X: / Position Y: (or keep Offset) |
| Select image and palette for overview (canvas placeholder) | Select an image and colors for preview |
| Grid region outside image bounds | Layout area is outside the image |
| Select image, palette and grid for preview | Select image, colors and layout for preview |

---

## Build screen (step-by-step)

| Current | Proposal |
|--------|----------|
| Patch background (label) | Background for unfinished areas |
| Current patch by color (least → most) | This section: colors from fewest to most bricks |

---

## Galleries (empty states & buttons)

| Current | Proposal |
|--------|----------|
| No grid configs yet. / + Create GridConfig | No layouts yet. / + New layout |
| No build configs yet. / + Create BuildConfig | No setups yet. / + New mosaic setup |
| No palettes yet. / + Create Palette | (keep or: + New palette) |
| No builds yet. / + Start new build | (keep) |
| New build from config: (dropdown) | New build from setup: |
| From image (palette) | (keep) |

---

## PDF booklet (PdfUtils)

| Current | Proposal |
|--------|----------|
| Note: not all plates divisible by step size 16; only divisible plates have step sections. | Note: some sections don’t match the step size (16); only matching sections get step-by-step pages. |
| Plate 2 dimensions (48×32) are not divisible by step size 16; step-by-step sections omitted. | This section’s size (48×32) doesn’t work with the step size (16). Step-by-step pages are skipped for it. |

---

## Other

| Current | Proposal |
|--------|----------|
| Choose a gallery to manage saved items, or create new ones. (Overview) | Pick a step to manage your saved items or create new ones. (or keep "gallery") |
| Name (placeholder in header) | (keep) |
| Loading… | (keep) |
| Image must be at most 500×500 px | Image can be at most 500×500 pixels |

---

## Summary of term mapping

- **Grid / Grid config** → **Layout** (the shape/split of the mosaic).
- **Build config** → **Mosaic setup** or **Setup** (layout + image + colors + position).
- **Plate** (in UI) → **Section** where it means one tile/cell of the layout.
- **Cell** (in grid editor) → **Section**.
- **Step size (px)** → **Section size (pixels)** or keep with clearer helper.
- **Patch** (build screen) → **Section** or keep with short explanation.
- **Palette** → Keep (familiar); optionally "Colors" in some labels.
- **Offset X/Y** → **Position X/Y** (optional).

Implementing these in the codebase will make the app read more like normal language while keeping the same behavior.
