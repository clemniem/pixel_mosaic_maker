# Plan: Steps 11-15 — Minor Refactorings

These steps are lower-priority improvements to be done after MS10. Each is independent and can be done in any order.

**Downstream target:** The `tyrian-scaffold` project (`/Users/clem/Projects/tyrian-scaffold/`) is the canonical template repo for new Tyrian SPAs. Step 11 extracts framework code within this repo; Step 16 syncs improvements back to the scaffold. All refactoring steps (12-14) should keep scaffold compatibility in mind — extracted framework code must remain generic enough to work in the scaffold's simpler context.

---

## Step 11. Separate framework/app packages (m4)

**Goal:** Extract reusable Tyrian SPA framework code into a `framework` package so a new app can be built without touching domain code.

**Current state:** Everything is under `clemniem`. Framework code (`Screen`, `ActiveScreen`, `ScreenRegistry`, `CmdUtils`, `LocalStorageUtils`, `CanvasUtils`, `Loadable`, `Pagination`, `circe/*`, `nescss/*`) is mixed with domain code (`Color`, `BuildConfig`, `PixelPic`, `StoredEntities`, `PaletteUtils`, etc.).

**Relation to tyrian-scaffold:** The scaffold already has its own copies of the framework files (`App.scala`, `Screen.scala`, `CmdUtils.scala`, etc.) under the `scaffold` package. This step cleanly separates them here so Step 16 can sync the improved versions back. The scaffold is missing several utilities that this project has evolved: `CanvasUtils`, `ImageUtils`, `Loadable`, `Pagination`, `MapAsList`, `Gallery.scala`, `PixelPreviewBox`, and the richer PDF system (`PdfLayout`, `PdfLayoutConfig`, `PdfPreviewRenderer`).

**Proposed package layout:**

```
clemniem/
  framework/
    Screen.scala           (Screen trait, ActiveScreen, RootMsg, HandleScreenMsg)
    ScreenRegistry.scala
    common/
      CmdUtils.scala
      CanvasUtils.scala       (generic parts only: timing, retry, canvas lookup, scaleToFit)
      LocalStorageUtils.scala
      ImageUtils.scala        (generic parts: load, downscale, offscreen canvas)
      Loadable.scala
      Pagination.scala
      circe/MapAsList.scala
      nescss/NesCss.scala
      pdf/Instruction.scala
      pdf/JsPDF.scala
      pdf/PdfLayout.scala
      pdf/PdfLayoutConfig.scala
      pdf/PdfPreviewRenderer.scala
    screens/
      Gallery.scala          (generic gallery helpers)
      GalleryLayout.scala    (gallery layout/pagination view)
      GalleryEmptyState.scala
      ScreenHeader.scala
      PixelPreviewBox.scala
  app/
    PixelMosaicMaker.scala   (TyrianIOApp entry point)
    ScreenOutput.scala       (app-specific navigation payloads)
    ScreenFlow.scala         (app-specific screen ordering)
    Color.scala
    BuildConfig.scala
    Layout.scala
    PixelPic.scala
    PixelPicService.scala
    PaletteUtils.scala
    StoredEntities.scala
    ImgSize.scala, Pixel.scala, Rectangle.scala
    common/
      PdfUtils.scala         (domain-specific PDF generation)
      CanvasDrawing.scala    (domain-specific: drawPixelPic, drawFullImageWithGrid)
      image/Dithering.scala, ColorQuantizationService.scala, etc.
    screens/
      (all 14 screen files)
      PaletteStripView.scala (app-specific component)
```

**CanvasUtils split note:** The current `CanvasUtils` mixes generic helpers (timing, retry, canvas lookup, `scaleToFit`, `drawPlaceholder`, `drawCenteredErrorText`) with domain-specific drawing (`drawPixelPic`, `drawFullImageWithGrid` — these depend on `PixelPic`, `Layout`, `Color`). During extraction, move domain-specific drawing methods to `app/common/CanvasDrawing.scala`. The scaffold currently has no canvas utils at all, so the generic portion is net-new for the scaffold.

**ImageUtils split note:** Most of `ImageUtils` is generic (file loading, downscaling, offscreen canvas creation). Only `rawFromImageData`/`imageDataFromRaw` depend on the app's `RawImage` type — these stay in `app/`.

**Steps:**
1. Create `framework` and `app` sub-packages
2. Move files (no logic changes, only `package` declarations and imports)
3. Split `CanvasUtils` into generic (framework) + domain-specific (app) parts
4. Split `ImageUtils` similarly (generic to framework, `RawImage` conversions to app)
5. Verify: `sbt compile` + `sbt test` + manual smoke test
6. Verify: create a minimal "hello world" Tyrian app using only `framework` imports

**Effort:** Medium (many files to move, but no logic changes)

**Risk:** Low — purely mechanical. Git will track moves as renames.

---

## Step 12. Extract canvas renderers (m3)

**Goal:** Move domain-specific canvas drawing logic out of screen files into dedicated renderer objects.

**Current state:** 8 screens contain private canvas drawing methods (30-80 lines each). All use `CanvasUtils` helpers but contain domain-specific rendering logic (pixel art, grid overlays, build progress highlighting).

**Scaffold relevance:** These renderers are all domain-specific (pixel art, grids, build steps) and will live in `app/renderers/`. They won't go to the scaffold. However, extracting them makes Step 11's framework/app split cleaner — screen files become thinner and the dependency on domain types is concentrated in renderer objects rather than scattered across screens.

**Proposed renderers:**

| Renderer | Extracted from | What it draws |
|----------|---------------|---------------|
| `ImageRenderer` | ImagesGalleryScreen | PixelPic scaled to fit |
| `LayoutRenderer` | LayoutGalleryScreen, LayoutScreen | Grid cells with checkerboard + borders |
| `BuildConfigRenderer` | BuildConfigGalleryScreen | Cropped pic + grid overlay |
| `BuildPreviewRenderer` | BuildsGalleryScreen | Cropped pic + grid overlay + current step highlight |
| `BuildStepRenderer` | BuildScreen | Step-by-step build canvas |
| `ImageUploadRenderer` | ImageUploadScreen | Upload preview canvas |

**Steps:**
1. Create `app/renderers/` package (or `screens/renderers/`)
2. Extract each renderer as an `object` with a single public `draw` method
3. Screen files call `FooRenderer.draw(...)` instead of private methods
4. No behavior change — pure move refactoring

**Effort:** Medium (~300 lines moved, no logic changes)

---

## Step 13. Use CmdUtils.fireAndForget everywhere (m2)

**Goal:** Replace 57 bare `Cmd.SideEffect(io)` calls with `CmdUtils.fireAndForget` so canvas drawing failures are catchable.

**Current state:** `CmdUtils` has `run` and `fireAndForget` but they have **zero usage**. All screens use `Cmd.SideEffect` directly, which swallows exceptions to the console.

**Scaffold relevance:** The scaffold's `AboutScreen` already uses `CmdUtils.fireAndForget` correctly. After this step, the mosaic maker will match the scaffold's pattern. This validates that `CmdUtils` is the right abstraction for both projects.

**Approach:**
1. For each screen, add a `NoOp` message variant (or reuse an existing one)
2. Add a `DrawError(message: String)` message variant (can log to console, or just ignore)
3. Replace `Cmd.SideEffect(io)` with `CmdUtils.fireAndForget(io, Msg.NoOp, e => Msg.DrawError(e.getMessage))`
4. Optionally: add a global error toast/banner in the root model for surfacing draw errors

**Impact per screen:**

| Screen | Cmd.SideEffect count | Notes |
|--------|---------------------|-------|
| LayoutScreen | 18 | Heaviest user — grid canvas |
| PrintInstructionsScreen | 15 | PDF preview drawing |
| BuildsGalleryScreen | 6 | Build gallery previews |
| BuildConfigGalleryScreen | 3 | Config gallery previews |
| LayoutGalleryScreen | 3 | Layout gallery previews |
| ImagesGalleryScreen | 2 | Image gallery previews |
| BuildScreen | 2 | Build step canvas |
| BuildConfigScreen | 1 | Config preview |
| AboutScreen | 1 | `dom.window.open` |

**Effort:** Small-Medium (mechanical replacement, ~57 sites)

---

## Step 14. Error states for corrupted storage (m5)

**Goal:** When LocalStorage contains corrupted JSON, show an error state instead of silently displaying an empty gallery.

**Current state:** 33 call sites pass `(_, _) => SomeMsg(Nil)` as the failure handler, discarding decode errors. The `Loadable` ADT (`Loading | Loaded[A] | Failed(error)`) was created in Step 3 but is not yet integrated into screens.

**Scaffold relevance:** The scaffold does not have `Loadable` yet. After this step proves the pattern works in the mosaic maker, Step 16 can add `Loadable` to the scaffold and update `NotesScreen` as a reference implementation of the pattern.

**Approach:**
1. **Gallery screens** (6 screens): Replace `Option[List[A]]` with `Loadable[List[A]]` in `Gallery.State`
   - `Loadable.Loading` → show "Loading..." spinner
   - `Loadable.Loaded(list)` → show list (possibly empty)
   - `Loadable.Failed(error)` → show error with "Clear and retry" button
2. **Editor screens** (4 screens): Add `LoadFailed` message variant that sets an error in the model
3. **Gallery.loadCmd**: Return `Loadable.Failed` on decode error instead of `Loaded(Nil)`
4. **Gallery.view**: Add a `Failed` branch showing the error message

**UI for failed state:**
```
+-----------------------------------+
| Layouts                    [Back] |
|                                   |
|  Could not load layouts.          |
|  Error: Unexpected token at pos 3 |
|                                   |
|  [Clear data] [Retry]            |
+-----------------------------------+
```

**Effort:** Small-Medium (6 gallery + 4 editor screens)

---

## Step 15. Move overviewDescription out of framework (m6)

**Status: ALREADY DONE** — `overviewDescription` was moved to `ScreenFlow.scala` during Step 9. No further work needed.

---

## Step 16. Sync framework improvements to tyrian-scaffold (m7)

**Goal:** Update the `tyrian-scaffold` repo with all framework-level improvements made during steps 11-14 so the scaffold stays current.

**What the scaffold already has (keep, update in place):**
- `App.scala` — equivalent of `PixelMosaicMaker.scala` root app pattern
- `Screen.scala` — Screen trait, ScreenId, RootMsg, ActiveScreen, navigation
- `ScreenRegistry.scala`
- `CmdUtils.scala` — `run` + `fireAndForget`
- `LocalStorageUtils.scala` — save/load/saveList/loadList/confirmDelete
- `NesCss.scala` — CSS class constants
- `pdf/Instruction.scala` — basic: PageSize, FontSize, Text, AddPage, FillRect, Save
- `pdf/JsPDF.scala` — basic: run, runNow, getJsPDFConstructor
- `pdf/PdfUtils.scala` — `generateSamplePdf` (simple demo)
- `GalleryLayout.scala`, `GalleryEmptyState.scala`, `ScreenHeader.scala`
- `StoredEntities.scala` — demo `StoredNote` + `StorageKeys`
- Demo screens: `HomeScreen`, `AboutScreen`, `NotesScreen`
- `setup.sh` — interactive wizard for project customization
- `.cursor/rules/` — 6 rule files for AI assistance
- Deploy: GitHub Actions workflow, service worker, prod-scripts.html

**What to add to the scaffold (net-new from mosaic maker):**
- `CanvasUtils.scala` (generic portion) — timing, retry, canvas lookup, scaleToFit, drawPlaceholder
- `ImageUtils.scala` (generic portion) — file loading, downscaling, offscreen canvas
- `Loadable.scala` — Loading/Loaded/Failed ADT
- `Pagination.scala` — totalPagesFor, clampPage, sliceForPage
- `circe/MapAsList.scala` — Map-as-List Circe codec
- `Gallery.scala` — generic gallery state machine + view helper
- `PixelPreviewBox.scala` — reusable canvas wrapper component
- `StoredEntity` trait — generic marker trait (extract from `StoredEntities.scala`)

**What to update in the scaffold (mosaic maker has improved versions):**
- `Instruction.scala` — mosaic maker has many more variants (TextAligned, DrawLine, TextWithBackground, DrawPixelGrid, DrawStrokeRects, FillRectWithOpacity, DrawSwatchRow, RoundedFillRect, RoundedStrokeRect)
- `JsPDF.scala` — mosaic maker has page numbering, printer margin support, background color, font embedding
- Add `PdfLayout.scala`, `PdfLayoutConfig.scala`, `PdfPreviewRenderer.scala` — richer PDF system
- `NotesScreen` — update to use `Gallery.scala` + `Loadable` pattern as reference implementation

**What to update in scaffold docs:**
- `README.md` — add CanvasUtils, Loadable, Gallery, Pagination to structure listing
- `.cursor/rules/` — update with canvas patterns, Loadable usage, Gallery pattern
- `setup.sh` — add Canvas as optional module (toggle for canvas-related files)

**Steps:**
1. Copy framework files from mosaic maker's `framework/` package to scaffold's `src/main/scala/scaffold/`
2. Replace `clemniem.framework` package declarations with `scaffold`
3. Update `Instruction.scala` and `JsPDF.scala` with the richer versions
4. Add `PdfLayout.scala`, `PdfLayoutConfig.scala`, `PdfPreviewRenderer.scala`
5. Update `NotesScreen` to use `Gallery` + `Loadable`
6. Update `setup.sh` to handle new optional modules (Canvas, Loadable)
7. Update docs (README, cursor rules)
8. Verify: `sbt compile` + `sbt test` in the scaffold
9. Verify: run `setup.sh` with all module combinations

**Effort:** Medium (file copying + package renaming + wizard updates)

**Risk:** Low — scaffold has no users yet, so breaking changes are fine.

---

## Recommended Order

| Priority | Step | Depends on | Effort |
|----------|------|------------|--------|
| 1 | Step 14 (error states) | None | Small-Medium |
| 2 | Step 13 (CmdUtils) | None | Small-Medium |
| 3 | Step 12 (renderers) | None | Medium |
| 4 | Step 11 (packages) | Steps 12-14 done first is cleaner | Medium |
| 5 | Step 16 (scaffold sync) | Step 11 | Medium |

Step 15 is already complete. Steps 12, 13, 14 are independent and can be done in parallel. Step 11 (package separation) is best done last since it's a large mechanical change that's easier after the other refactorings settle. Step 16 (scaffold sync) is the final step — it depends on Step 11 being complete so the framework/app boundary is well-defined.
