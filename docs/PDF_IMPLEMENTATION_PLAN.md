# PDF book implementation plan (jsPDF)

Step-by-step plan to implement the print-instructions book from `print_instructions_layout.md`, with checkpoints so you can confirm each part works before moving on. Includes how to unit-test layout logic.

---

## 1. Testing strategy: what we can unit test

- **Layout logic (pure Scala):** We will have functions that take data (title, mosaic, plates, colors) and return `List[Instruction]`. These do **not** call jsPDF. We can unit test them:
  - **Cover:** `coverInstructions(title)` → list starts with `PageSize`, contains a `Text` (or `Title`) with the title, no `Save` in the middle.
  - **Overview:** `overviewInstructions(...)` → list has one or more pages, first page has `PageSize`, contains instructions for “total mosaic” and “color summary” (e.g. specific `Text` / structure we define).
  - **Plate overview:** `plateOverviewInstructions(..., plateIndex)` → list contains instructions that represent “full mosaic + current plate marked” and “per-plate color counts”.
  - **Patch page:** `patchPageInstructions(...)` → list has up to 4 patch-related instructions per page.
  - **Final overview:** same shape as overview (we can reuse or compare instruction structure).

- **What we do not unit test:** The actual rendering in the browser (jsPDF turning instructions into PDF bytes). That is confirmed **manually** at each step (or later with a browser automation test if you add one).

- **Where tests live:** Same project, `src/test/scala/.../pdf/` (e.g. `PdfLayoutSpec.scala`). Tests run with `sbt test` (Scala.js test runtime; no jsPDF needed for layout tests).

---

## 2. Data and types to agree up front

- **Input to the full book:** A single **build config** (e.g. `StoredBuildConfig` or resolved `BuildConfig` + `PixelPic` + palette). So: grid (plates = `GridConfig.parts`), cropped pixel image, palette colors.
- **Title:** From `StoredBuildConfig.name` (or a dedicated “print title” field).
- **Plates:** `grid.parts` — each `GridPart` is one plate (one chapter).
- **16×16 patches:** Each plate is subdivided in 16×16 logical pixels; each “patch” is one 16×16 cell. So “single-color patch” = one 16×16 pattern for one color on that plate.
- **Color counts:** From the cropped `PixelPic`: total per color (overview / final), per-plate per color (chapter overview).

We will add a small **PDF input model** (e.g. `PdfBookInput`) that holds: title, cropped `PixelPic`, grid, and precomputed per-plate and global color counts, so layout functions stay pure and testable.

---

## 3. Step-by-step implementation (with checkpoints)

Each step ends with: “Run the app, trigger PDF, **confirm in the browser** that the new part looks correct.”

### Phase A: Extend instructions and runner

| Step | What | Checkpoint |
|------|------|------------|
| **A.1** | Add instructions needed for the book: at least `AddPage` (new page, same size), and optionally `Title` (cover), `TextBlock`, `Image` (data URL), `Rect` / `Line` for grids. Keep existing `PageSize`, `FontSize`, `Text`, `Save`. | After A.1: `sbt test` still passes (no new layout tests yet). Existing “Print PDF” still produces the same test PDF. |
| **A.2** | Extend `JsPDF.runNow` to handle new instructions (`AddPage`, etc.). | Run app, click “Print PDF”: still works. Then add one `AddPage` + second page with text “Page 2” and confirm the PDF has two pages. |

**Checkpoint 1:** You have a multi-page PDF (e.g. cover + “Page 2”) generated from the app. Confirm in the browser that both pages open and look correct.

---

### Phase B: Cover

| Step | What | Checkpoint |
|------|------|------------|
| **B.1** | Add pure function `coverInstructions(title: String): List[Instruction]` that returns: one page (e.g. A4), large title text centered (or top-centered). No `Save` in this list (caller appends `Save` at end of full book). | Add a unit test: e.g. “coverInstructions("My Mosaic") contains exactly one PageSize, and at least one Text whose value contains "My Mosaic"”. |
| **B.2** | In `PdfUtils`, add `printBookPdf(input: PdfBookInput)` (or similar) that builds: `coverInstructions(input.title) ++ ...` and for now only cover + `Save("mosaic-book.pdf")`. Call it from the Print button (you can temporarily use a hardcoded title or the selected build config name). | Run app, open a build config (or gallery), click Print. **Confirm:** PDF opens with one cover page and the title visible. |

**Checkpoint 2:** Cover page with title appears in the generated PDF. Unit test for `coverInstructions` passes.

---

### Phase C: Overview (first content page)

| Step | What | Checkpoint |
|------|------|------------|
| **C.1** | Define `overviewInstructions(...)` that returns instructions for: (1) “Total mosaic” (image + plate grid + 16×16 patch grid), (2) “Color summary” (list of colors and total count). Use placeholders if needed: e.g. text “Mosaic image” and a table/list of “Color A: 42” etc. from `PdfBookInput`. | Unit test: e.g. overview list has `AddPage` (or second page), and contains `Text` entries that reflect the global color counts from the input. |
| **C.2** | Implement in JsPDF any missing instructions (e.g. draw image from data URL, draw lines for grid). Keep overview simple at first (e.g. title “Overview”, then text list of colors; add actual image in a follow-up if needed). | Run app, Print. **Confirm:** PDF has Cover + Overview page; overview shows at least the list of colors and totals. |

**Checkpoint 3:** Overview page appears with total mosaic representation and per-color totals. You confirm layout and numbers.

---

### Phase D: One chapter per plate

| Step | What | Checkpoint |
|------|------|------------|
| **D.1** | `plateOverviewInstructions(input, plateIndex)` returning: (1) general overview with current plate marked (same as overview but with one plate highlighted), (2) per-plate color counts. | Unit test: for a grid with 2 plates, `plateOverviewInstructions(..., 1)` contains instructions that represent “plate 1” (e.g. text or structure we define for “current plate”). |
| **D.2** | Wire chapter order: after overview, for each plate index: append `plateOverviewInstructions(...)`, then append patch pages for that plate. | Print. **Confirm:** PDF has Cover, Overview, then Chapter 1 (plate 0): plate overview with full mosaic + current plate marked, and per-plate color list. |
| **D.3** | `patchPageInstructions(patchesForPage, ...)`: up to 4 single-color patches per page. Each “patch” = one 16×16 pattern for one color. | Unit test: patchPageInstructions with 4 patches returns one page’s worth of instructions; with 5 patches, two pages (or equivalent structure). |
| **D.4** | For each plate, compute list of (color, 16×16 patch) and chunk into pages of 4; append all `patchPageInstructions` for that plate. | Print. **Confirm:** First chapter has plate overview followed by pages of up to 4 patches each; patches look correct. Then repeat for second plate chapter. |

**Checkpoint 4:** Full “one chapter per plate” with plate overview (general + marked) and patch pages. You confirm one full chapter.

---

### Phase E: Final overview and wiring

| Step | What | Checkpoint |
|------|------|------------|
| **E.1** | Reuse overview instructions at the end: append `overviewInstructions(...)` again (or a dedicated `finalOverviewInstructions` that reuses the same layout). | Unit test: full book instruction list ends with the same “shape” as overview (e.g. same number of pages or same instruction types) before `Save`. |
| **E.2** | Full book = Cover + Overview + (for each plate: plate overview + patch pages) + Final overview + Save. Remove or keep test PDF button; ensure Print uses `PdfBookInput` from current build config (name, image, palette, grid). | Print. **Confirm:** PDF ends with a final overview page (same content as first overview), then closes. Full flow is correct. |

**Checkpoint 5:** Book structure matches `print_instructions_layout.md`: Cover → Overview → Chapters (plate overview + patch pages) → Final overview. You do a final visual pass.

---

## 4. Suggested file layout

- `src/main/scala/clemniem/common/pdf/Instruction.scala` — extend with new instruction types.
- `src/main/scala/clemniem/common/pdf/JsPDF.scala` — interpret new instructions.
- `src/main/scala/clemniem/common/pdf/PdfBookInput.scala` (or in `PdfUtils`) — input model: title, cropped pic, grid, global and per-plate color counts.
- `src/main/scala/clemniem/common/pdf/PdfLayout.scala` — pure functions: `coverInstructions`, `overviewInstructions`, `plateOverviewInstructions`, `patchPageInstructions`, `finalOverviewInstructions`, and `fullBookInstructions(input)` that concatenates all.
- `src/main/scala/clemniem/common/PdfUtils.scala` — build `PdfBookInput` from `StoredBuildConfig` (resolve image + palette, crop, compute counts), then call `PdfLayout.fullBookInstructions` and `JsPDF.run`.
- `src/test/scala/clemniem/common/pdf/PdfLayoutSpec.scala` — unit tests for each layout function (instruction count, presence of title text, page breaks, etc.).

---

## 5. Unit test examples (summary)

- **Cover:** `coverInstructions("X")` → contains `PageSize`, and some `Text` with `"X"`.
- **Overview:** `overviewInstructions(input)` → first instruction is `PageSize` (or AddPage after cover); list contains texts for each color in `input.globalColorCounts`.
- **Plate overview:** `plateOverviewInstructions(input, 0)` → contains representation of “current plate” (e.g. plate index or id in a Text/instruction).
- **Patch page:** `patchPageInstructions(patches, pageSize)` with 4 patches → length/count consistent with one page; with 5 patches → two pages.
- **Full book:** `fullBookInstructions(input)` → ends with `Save`; before that, structure matches Cover + Overview + N chapters + Final overview (e.g. count of `AddPage` or page-sized chunks).

You can add these tests as you implement each layout function (TDD-style) or right after each phase. Keeping layout pure ensures we can run these tests without jsPDF or a browser.

---

## 6. Keeping you in the loop

- After **each checkpoint** (1–5), we pause so you can run the app and confirm the PDF in the browser.
- For each phase we can implement one step at a time and you can confirm before the next step (e.g. confirm cover only, then we add overview).
- If you want, we can start with **Phase A + B** (extend instructions, then cover + unit test) and you confirm the two-page “Cover + Page 2” and then the real cover with title, before moving to Overview.
