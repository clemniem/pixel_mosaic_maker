# Print instructions – Book layout

This document describes the layout of the printed book (PDF) for the mosaic.

---

## 1. Cover

- First page (20×20 cm): cover of the book with a **title** only.

---

## 2. Overview

- **Page (20×20 cm).**
- **Total mosaic**
  - One view of the full mosaic (pixel grid, scaled to fit with margins).
  - **Plate grid** overlay: plate boundaries drawn in red so the reader sees how the mosaic is split into plates.

---

## 3. One chapter per plate

Each plate gets its own chapter. Currently only **Chapter 1 – Plate 1** is generated.

### 3.1 Chapter start – Plate overview

- **Page (20×20 cm):**
  - **Header:** “Chapter 1 – Plate 1”.
  - **General overview with current plate marked:** small view of the full mosaic with plate grid (red) and the **current plate** outlined in blue.
  - **Plate image:** the cropped plate at full pixel resolution (same as the plate area in the mosaic).
  - **Colors for this plate:** for each color used on this plate, a **small color swatch** (rectangle) plus the **count** of pixels (e.g. “× 42”). Colors are listed by count (most used first).

### 3.2 Chapter body – Color layer (16×16 step) pages

- After the plate overview, the chapter continues with **layer patch pages**.
- **Page size:** **20×20 cm** (200×200 mm), square.
- **Per page:** exactly **4** patches in a 2×2 grid (fewer on the last page if the number of layers is not a multiple of 4).
- **Each patch** represents one **cumulative color layer**:
  - Colors are ordered by **least-used first** (ascending count).
  - **Layer 1** = only the rarest color (as 16×16 blocks).
  - **Layer 2** = rarest + next rarest; **Layer 3** = first three colors; … up to the full plate.
- **16×16 block resolution:** each patch is drawn as a grid of **16×16 pixel blocks**. One drawn cell = one 16×16 region of the plate; if that region contains any pixel in the current cumulative color set, the block is shown in that color; otherwise in the **layer background** (currently light grey, intended to be configurable later).
- **Overlay:** each of the four patches has a **4×4 grid** drawn on top (dark grey lines) to align with the block layout.
- **Labels:** under each patch, “Layer 1”, “Layer 2”, etc. (global across pages).
- **Section title** on each layer page: “Plate 1 – color layers (16×16 steps)”.

---

## 4. Final overview (planned)

- Before closing the book: a **final overview** page (not yet implemented).
- Same as the opening overview (full mosaic with plate grid), so the reader can refer back at the end.

---

## Summary

| Section        | Content |
|----------------|--------|
| Cover          | **title** only |
| Overview       | full mosaic + **plate grid** (red) |
| Ch. per plate  | Plate overview: small mosaic (current plate in blue), plate image, **color swatches + counts**; then pages with **4 layer patches** each (full-resolution cumulative layers, light grey background, 16×16 step grid, “Layer N” labels) |
| Final overview | Planned: full mosaic + plate grid again |

---

## Page size

**All pages are 20×20 cm** (200×200 mm).
