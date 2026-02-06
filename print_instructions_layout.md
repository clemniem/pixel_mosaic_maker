# Print instructions – Book layout

This document describes the layout of the printed book (PDF) for the mosaic.

---

## 1. Cover

- First page (20×20 cm): **title** at the top, then the **full mosaic** (pixel grid, scaled to fit below the title). No plate grid or other overlays on the cover.

---

## 2. One chapter per plate

Each plate gets its own chapter. Currently only **Chapter 1 – Plate 1** is generated.

### 2.1 Chapter start – Plate overview

- **Page (20×20 cm):**
  - **Header:** “Chapter 1 – Plate 1”.
  - **General overview with current plate marked:** small view of the full mosaic with plate grid (red) and the **current plate** outlined in blue.
  - **Plate image:** the cropped plate at full pixel resolution (same as the plate area in the mosaic).
  - **Colors for this plate:** for each color used on this plate, a **small color swatch** (rectangle) plus the **count** of pixels (e.g. “× 42”). Colors are listed by count (most used first).
  - **Validation note (if applicable):** if not all plates in the grid are divisible by the configured step size, a note is shown: “Note: not all plates divisible by step size N; only divisible plates have step sections.”

### 2.2 Step size and validation

- **Step size** is **configurable** (Print Instructions screen), **default 16** (px). Each build step is a **stepSize×stepSize** subpart of the plate (e.g. 16×16 px).
- **Validation:** All plates must be divisible by the step size (plate width % stepSize == 0 and plate height % stepSize == 0). This is checked for every plate in the grid.
  - If the **current plate** (e.g. Plate 1) is **not** divisible, the chapter still gets the plate overview page, then a **dedicated page** with the message: “Plate 1 dimensions (W×H) are not divisible by step size N; step-by-step sections omitted.” No step sections are generated for that plate.
  - If the current plate **is** divisible, the chapter body is made of **one section per step** (see below). If any *other* plate in the grid is not divisible, a note appears on the plate overview (see 2.1).

### 2.3 Chapter body – One section per build step

- After the plate overview, the chapter body is split into **sections**. **One section = one build step** (one stepSize×stepSize patch in row-major order over the plate).
- For each step:

  **Section overview (all colors)**  
  - One page: **“Plate 1 – Step N (section overview)”** with subtitle **“All colors – step region highlighted”**.
  - The **full plate** is drawn at the same scale as on the chapter overview.
  - The **step’s region** (stepSize×stepSize at position (cx, cy) in the step grid) is **highlighted** with a green stroke rectangle so the reader sees where this step lies in the plate.

  **Layered canvases (least → most)**  
  - Then one or more **layer pages** for this step only.
  - **Page title:** “Plate 1 – Step N – color layers (least → most)”.
  - Each patch is the **step’s stepSize×stepSize crop** of the plate, drawn at **full pixel resolution**, with **cumulative color layers** (same idea as the Build screen’s “Current patch by color”):
    - Colors in the patch are ordered by **least-used first** (ascending count).
    - **Layer 1** = only the rarest color in that patch; **Layer 2** = rarest + next; … up to all colors in the patch.
  - **Layout:** up to **4** patches per page in a **2×2** grid (same 20×20 cm page).
  - **Background:** pixels not in the current cumulative color set are drawn in **light grey** (layer patch background).
  - **Overlays:** each patch has (1) a **step grid** (lines every stepSize px) and (2) a **4×4 grid**, both in dark grey.
  - **Labels:** under each patch, “Layer 1”, “Layer 2”, etc. (per step).

- Steps are ordered in **row-major** order (same as the build): first row left-to-right, then next row, etc.

---

## 3. Final overview (planned)

- Before closing the book: a **final overview** page (not yet implemented).
- Full mosaic with plate grid so the reader can refer back at the end.

---

## Summary

| Section        | Content |
|----------------|--------|
| Cover          | **Title** above **full mosaic** (no grids) |
| Ch. per plate  | Plate overview (small mosaic, current plate in blue, plate image, **color swatches + counts**, optional divisibility note); **validate** plate divisibility by step size; then **one section per step**: section overview (plate + step highlighted) + **layered canvases** for that step only (4 per page, cumulative colors least→most, step grid + 4×4 grid) |
| Final overview | Planned: full mosaic + plate grid |

---

## Page size

**All pages are 20×20 cm** (200×200 mm).
