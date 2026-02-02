# Pixel Mosaic Maker – App Flow

This document describes the six steps of the single-page app. Each step is a **Screen** with its own model, messages, and view. Data flows between steps via **ScreenOutput** and **NavigateNext**.

Inspired by [gbcamutil](https://github.com/your-repo/gbcamutil); this app is a rewrite with a clearer step-by-step flow.

---

## 1. GridConfig

**Purpose:** Define a grid of **plates** (Lego-style plates that together form the whole image).

- The grid is the subdivision of the final mosaic into plates.
- Important for instructions: instructions are generated **per plate**.
- Each plate has an internal grid (cell resolution). For now this is fixed at **16×16 pixels**; later it can be configured.

**Output:** The chosen `GridConfig` (e.g. `GridConfigDone(grid)`).

---

## 2. ImageUpload

**Purpose:** Upload pixel art and prepare it for the mosaic pipeline.

- Upload one or more images.
- Make them **pixel-true**: downscale if needed so pixels align with the grid.
- Store in a **PixelImage** format (similar to `PixelPic` in gbcamutil) to simplify later instruction generation.
- **Color reduction:** downscale colors to a fixed palette (e.g. **4 colors** for Game Boy Camera; palette size to be configurable later).

**Output:** The prepared pixel image(s) (e.g. `ImageUploaded(pixelImage)`).

---

## 3. Palettes

**Purpose:** Define different **palette combinations** (which colors map to which slots).

- Same idea as palette selection in gbcamutil: e.g. 4 palette slots mapped to Lego/Game Boy colors.
- User can define and name several palettes and pick one per build.

**Output:** The chosen palette (e.g. `PaletteChosen(palette)` or id).

---

## 4. BuildConfig

**Purpose:** Define **what** to build – stateless configuration.

- A **build** is: **Palette** + **GridConfig** + **Image** + **Offset**.
- The image is usually larger than the mosaic, so an **offset** selects which part of the image is visible in the mosaic.
- **BuildConfig** is stateless: it only describes the combination; it does not hold “current step” or UI state.

**Output:** The build config (e.g. `BuildConfigDone(config)`).

---

## 5. Starting a Build (Build runner)

**Purpose:** Run one **instance** of a build with state.

- Takes a **BuildConfig** and creates one “build instance”.
- State: current page/step, etc.
- Computes Lego instructions **per plate**, **per cell**.
- UI: next step, previous step, etc.

**Output:** Optional (e.g. “done” or “go to print”); can navigate to Print Instructions with the same config.

---

## 6. Printing Instructions

**Purpose:** Produce a PDF of the full instruction set.

- Same inputs as “Starting a Build” (BuildConfig).
- Instead of interactive step-by-step, it **prints every step** into a PDF (one instruction per step, per plate/cell as needed).

**Output:** PDF generated (download / open).

---

## Flow summary

```
GridConfig → ImageUpload → Palettes → BuildConfig → [Start Build] → [Print Instructions]
     │              │           │           │              │                    │
     └──────────────┴───────────┴───────────┴──────────────┴────────────────────┘
                    (ScreenOutput / NavigateNext between steps)
```

Screens can be reached in different orders (e.g. from a home menu) by navigating with the right `ScreenOutput`; the diagram above is the main “create a mosaic” path.
