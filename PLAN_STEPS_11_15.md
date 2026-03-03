# Plan: Steps 11-15 — Minor Refactorings

These steps are lower-priority improvements to be done after MS10. Each is independent and can be done in any order.

---

## Step 11. Separate framework/app packages (m4)

**Goal:** Extract reusable Tyrian SPA framework code into a `framework` package so a new app can be built without touching domain code.

**Current state:** Everything is under `clemniem`. Framework code (`Screen`, `ActiveScreen`, `ScreenRegistry`, `CmdUtils`, `LocalStorageUtils`, `CanvasUtils`, `Loadable`, `Pagination`, `Loadable`, `circe/*`, `nescss/*`) is mixed with domain code (`Color`, `BuildConfig`, `PixelPic`, `StoredEntities`, `PaletteUtils`, etc.).

**Proposed package layout:**

```
clemniem/
  framework/
    Screen.scala           (Screen trait, ActiveScreen, RootMsg, HandleScreenMsg)
    ScreenRegistry.scala
    common/
      CmdUtils.scala
      CanvasUtils.scala
      LocalStorageUtils.scala
      ImageUtils.scala
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
      image/Dithering.scala, ColorQuantizationService.scala, etc.
    screens/
      (all 14 screen files)
      PaletteStripView.scala (app-specific component)
```

**Steps:**
1. Create `framework` and `app` sub-packages
2. Move files (no logic changes, only `package` declarations and imports)
3. Verify: `sbt compile` + `sbt test` + manual smoke test
4. Verify: create a minimal "hello world" Tyrian app using only `framework` imports

**Effort:** Medium (many files to move, but no logic changes)

**Risk:** Low — purely mechanical. Git will track moves as renames.

---

## Step 12. Extract canvas renderers (m3)

**Goal:** Move domain-specific canvas drawing logic out of screen files into dedicated renderer objects.

**Current state:** 8 screens contain private canvas drawing methods (30-80 lines each). All use `CanvasUtils` helpers but contain domain-specific rendering logic (pixel art, grid overlays, build progress highlighting).

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

## Recommended Order

| Priority | Step | Depends on | Effort |
|----------|------|------------|--------|
| 1 | Step 14 (error states) | None | Small-Medium |
| 2 | Step 13 (CmdUtils) | None | Small-Medium |
| 3 | Step 12 (renderers) | None | Medium |
| 4 | Step 11 (packages) | Steps 12-14 done first is cleaner | Medium |

Step 15 is already complete. Steps 12, 13, 14 are independent and can be done in parallel. Step 11 (package separation) is best done last since it's a large mechanical change that's easier after the other refactorings settle.
