# LEARNINGS.md – Context for Future Agents and Developers

This document captures what was learned during development of the Pixel Mosaic Maker app so that future agents or developers can get up to speed quickly.

---

## 1. Project overview

- **Stack:** Scala 3, Scala.js, **Tyrian** (Elm-style SPA), **Circe** (JSON), **Cats Effect IO**, Parcel for dev server.
- **Entry:** `PixelMosaicMaker` in `PixelMosaicMaker.scala` is a `TyrianIOApp`; root model is `RootModel(registry, currentScreenId, currentModel: Any)`. Screens are type-erased; each screen has `Model`, `Msg`, `init`, `update`, `view`, and `wrapMsg`.
- **Navigation:** Screens emit `NavigateNext(screenId, Some(ScreenOutput))`. Root app calls the target screen’s `init(Some(output))` and replaces `currentModel`. No URL routing yet (router is `Routing.none`).
- **Persistence:** All lists (grid configs, palettes, images, build configs, builds) are in **LocalStorage** via `clemniem.common.LocalStorageUtils` (`loadList` / `saveList` with `StorageKeys` in `StoredEntities.scala`).

---

## 2. Key data types

| Type | Where | Purpose |
|------|--------|--------|
| **Pixel** | `Pixel.scala` | RGBA; used inside PixelPic palette. |
| **Color** | `Color.scala` | RGB + hex; UI and StoredPalette. |
| **PixelPic** | `PixelPic.scala` | Width, height, palette (Vector[Pixel]), pixels (indices), name. Has `crop`, `setPalette`, `toImageData`. |
| **GridPart** | `GridConfig.scala` | x, y, width, height (one plate). |
| **GridConfig** | `GridConfig.scala` | cols, rows, parts: Array[GridPart]. |
| **BuildConfig** | `Screen.scala` | grid, imageRef, paletteRef, offsetX, offsetY (stateless template). |
| **StoredBuildConfig** | `StoredEntities.scala` | id, name, config, savedStepIndex (optional; legacy). |
| **StoredBuild** | `StoredEntities.scala` | id, name, **buildConfigRef** (id of StoredBuildConfig), **savedStepIndex**. A “build run” in the builds list. |
| **StoredImage** | `StoredEntities.scala` | id, name, pixelPic. |
| **StoredPalette** | `StoredEntities.scala` | id, name, colors: Vector[Color]. |

- **BuildConfig** = template (which grid, image, palette, offset). **StoredBuild** = one run: points at a config and stores current step; saved to `StorageKeys.builds`. Save step on Build screen updates/creates a StoredBuild in that list.

---

## 3. Backward compatibility (LocalStorage / Circe)

- Old JSON may lack new fields. Use **custom decoders** with `withX.or(withoutX)` (e.g. `StoredPalette` without `colors`, `StoredImage` without `pixelPic`, `StoredBuildConfig` without `savedStepIndex`, `StoredBuild` without `buildConfigRef`/`savedStepIndex`). Provide sensible defaults (e.g. default palette, placeholder PixelPic, empty string / None).
- **StoredBuild** legacy: old format was `(id, name)`; decoder maps to `buildConfigRef = ""`, `savedStepIndex = None`. Builds gallery filters these out (`buildConfigRef.nonEmpty`) so they don’t get a Resume button.

---

## 4. Canvas drawing

- **clemniem.common.CanvasUtils:** Use `drawAfterViewReady(id, maxRetries, delayMs)(draw)` or `drawAfterViewReadyDelayed` for canvases in lists (extra frames so DOM is ready). Never draw before the canvas is in the DOM.
- **Drawing a PixelPic:** Use `CanvasUtils.drawPixelPic(canvas, ctx, pic, targetWidth, targetHeight)` – fills ImageData from pic, draws via offscreen buffer, sets `imageSmoothingEnabled = false`. Used in BuildConfig, BuildConfigGallery, ImagesGallery, ImageUpload, Build screens.
- **Applying a palette to a PixelPic:** `PaletteUtils.applyPaletteToPixelPic(pic, storedPalette)` – maps StoredPalette colors to Pixel (alpha 255), pads/trims to pic’s palette size, then `pic.setPalette(...)`.

---

## 5. Gallery patterns

- **Empty state:** Shared `GalleryEmptyState(emptyText, buttonLabel, createMsg)` in `screens/GalleryEmptyState.scala`. Same styling everywhere.
- **Delete:** Many galleries use “Delete” → set `pendingDeleteId` → show “Delete «name»?” with Yes/Cancel; on Confirm delete from list and save to LocalStorage.
- **List + Create button:** Load list in `init`; view shows list of entry cards plus a “+ Create …” or “Upload” button. Create often navigates to an editor with `None`; Edit navigates with `Some(EditX(stored))`.

---

## 6. Build vs BuildConfig

- **BuildConfig** = template (grid + image + palette + offset). Stored in `buildConfigs`; edited in BuildConfigScreen; chosen when **starting a new build** from Builds gallery (dropdown only after “+ Start new build”).
- **Build** = one run: **StoredBuild** in `builds` with `buildConfigRef` and `savedStepIndex`. Build screen can be entered with:
  - **StartBuild(storedBuildConfig):** New run; no StoredBuild yet. First “Save step” creates a StoredBuild and appends to `builds`.
  - **ResumeBuild(storedBuild):** Load config by `buildConfigRef`, restore step; “Save step” updates that StoredBuild in `builds`.
- Offset in BuildConfig is clamped so the grid stays inside the image (BuildConfigScreen: `maxOffsets`, `clampOffsets`).

---

## 7. Build screen steps

- Steps = 16×16 patches: for each plate (GridPart), for each 16×16 cell in that plate (integer division), one step = (imageX, imageY) with imageX = offsetX + part.x + cx*16, imageY = offsetY + part.y + cy*16.
- Overview canvas shows **grid region only** (cropped image at offset), not the full image. Current 16×16 patch is highlighted in green.
- Preview canvas shows only the current 16×16 patch (scaled up for display).

---

## 8. Tyrian / UI quirks

- **Disabled attribute:** Tyrian’s HTML helpers may not support `disabled := true` on buttons in the same way; use conditional **style** (e.g. `cursor: not-allowed; opacity: 0.6`) instead of disabled when needed.
- **Number inputs:** Use `min := "0"`, `max := value.toString` for range; `value := model.value.toString` and `onInput` that parses and dispatches.
- **Reserved names:** Use backticks for HTML attributes that are Scala keywords, e.g. `` `type` := "number" ``.
- **onLoad:** `onLoad` is an **attribute** that takes a single message (e.g. to trigger a draw). It does **not** take a child; put it on the parent: `div(onLoad(DrawMsg))(canvas()())`, not `onLoad(DrawMsg)(canvas()())` (the latter fails with “onLoad does not take more parameters”).

---

## 9. Linting (Scalafix)

- **No `return`:** Use if/else or pattern match instead of early return.
- **No `null`:** Use `Option` and `.filter` / `.getOrElse`.
- **No `var` / `while`:** Prefer immutable and for-comprehensions / `.fold` / `.forall`.
- **No default arguments** in some rules; pass explicitly at call sites if required.
- Run `sbt compile` (Scalafix runs as part of compile); fix any reported issues before committing.

---

## 10. Where things live

| Concern | Location |
|--------|----------|
| Screen IDs, ScreenOutput, BuildConfig | `Screen.scala` |
| Stored entities, StorageKeys, Circe decoders | `StoredEntities.scala` |
| Canvas helpers, drawPixelPic | `common/CanvasUtils.scala` |
| Image load, scale detection, downscale | `common/ImageUtils.scala` |
| LocalStorage load/save list | `common/LocalStorageUtils.scala` |
| Apply StoredPalette to PixelPic | `PaletteUtils.scala` |
| Gallery empty state component | `screens/GalleryEmptyState.scala` |
| Root app, screen registry | `PixelMosaicMaker.scala` |
| Grid logic (parts, row/column defs) | `GridConfig.scala` |
| Pixel image (crop, setPalette, toImageData) | `PixelPic.scala` |

---

## 11. Testing

- Unit tests in `src/test/scala/clemniem/`: e.g. `ResizeSpec` (scale detection, downscale), `PixelPicTests`, `GridConfigSpec`, `LocalStorageUtilsSpec`. Run with `sbt test`.
- No browser/E2E tests in this repo; manual check in browser after `yarn start` and `fastLinkJS`.

---

## 12. Quick checklist for new features

1. **New screen:** Add `ScreenId`, implement `Screen` (init, update, view), register in `PixelMosaicMaker`’s `ScreenRegistry`.
2. **New stored entity:** Add case class in `StoredEntities.scala`, Circe encoder + backward-compatible decoder if needed, add `StorageKeys` key if it’s a new list.
3. **New navigation payload:** Add a `ScreenOutput` case class; pass it in `NavigateNext(screenId, Some(output))` and handle it in the target screen’s `init`.
4. **Canvas:** Use `CanvasUtils.drawAfterViewReady` or `drawAfterViewReadyDelayed`; use `drawPixelPic` for PixelPic; set canvas size in draw code when it depends on content (e.g. smaller image → smaller canvas).
5. **Gallery empty state:** Use `GalleryEmptyState(emptyText, buttonLabel, msg)`.
6. After changes: `sbt compile` and `sbt test`; fix Scalafix and test failures.

Using this file together with **FLOW.md** and **README.md** should give enough context to work on the project efficiently.
