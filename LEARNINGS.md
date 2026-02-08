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

## 8. NES.css and gallery styling

- **NES.css:** The app uses **NES.css** for a consistent retro look. Central class-name strings live in **`clemniem.common.nescss.NesCss`** (e.g. `NesCss.container`, `NesCss.containerRounded`, `NesCss.btn`, `NesCss.btnPrimary`, `NesCss.btnError`, `NesCss.text`, `NesCss.input`). Shared layout and gallery-specific classes are in **`css/style.css`** (e.g. `screen-container`, `screen-header`, `screen-title`, `flex-row`, `flex-col`, `gallery-card`, `gallery-card-body`, `gallery-card-title`, `gallery-card-meta`, `gallery-preview-canvas`, `gallery-actions`, `gallery-delete-confirm`, `palette-strip`, `palette-strip-swatch`, `progress-bar`, `progress-bar-fill`, `dropdown-panel`, `dropdown-panel-title`).
- **Gallery pattern (all galleries):** Root = `NesCss.container` + `NesCss.containerRounded` + `screen-container`. Header = `screen-header`, `screen-title`, back button = `NesCss.btn`.
- **Editor/creation screens:** Use **`ScreenHeader`** (`screens/ScreenHeader.scala`) for a consistent header: title, button row, optional name row (full-width input via `nameRowInput`). BuildConfig, GridConfig, Palette, ImageUpload, PrintInstructions, Build all use it where applicable. Primary action (Create/Upload) = `NesCss.btnPrimary`. Entry cards = `nes-container` + `gallery-card` (use `gallery-card--start` for Builds); body = `gallery-card-body`, title = `gallery-card-title`, meta = `gallery-card-meta nes-text`. Delete confirm block = `gallery-delete-confirm`, text = `delete-confirm-text nes-text`, Yes = `NesCss.btnError`, Cancel = `NesCss.btn`. Actions row = `gallery-actions`; Edit = `NesCss.btn`, Delete = `NesCss.btnError`; Resume (Builds) = `NesCss.btnPrimary`. Only **dynamic** values stay inline (e.g. palette/color `background`, progress bar `width` %).
- **Empty state:** `GalleryEmptyState` uses NES + `empty-state`; its main button uses `NesCss.btnPrimary`.

---

## 9. Tyrian / UI quirks

- **Disabled attribute:** Tyrian’s HTML helpers may not support `disabled := true` on buttons in the same way; use conditional **style** (e.g. `cursor: not-allowed; opacity: 0.6`) instead of disabled when needed.
- **Number inputs:** Use `min := "0"`, `max := value.toString` for range; `value := model.value.toString` and `onInput` that parses and dispatches.
- **Reserved names:** Use backticks for HTML attributes that are Scala keywords: `` `class` := "..." ``, `` `type` := "number" ``. NES + gallery views use `` `class` := s"${NesCss.container} ..." ``.
- **onLoad:** `onLoad` is an **attribute** that takes a single message (e.g. to trigger a draw). It does **not** take a child; put it on the parent: `div(onLoad(DrawMsg))(canvas()())`, not `onLoad(DrawMsg)(canvas()())` (the latter fails with “onLoad does not take more parameters”).

---

## 10. PDF (jsPDF)

- **Providing jsPDF to the app:** **Local dev:** Use the **npm** package and an **export wrapper**. In `export-wrapper.js`: `import { jsPDF } from "jspdf"; window.jspdf = { jsPDF };` then import and launch the Scala.js app. `index.html` loads `export-wrapper.js` as the module entry so the bundler includes jsPDF. **Production (GitHub Pages):** The deploy workflow builds its own `index.html` that loads jsPDF from a CDN (e.g. unpkg `jspdf.umd.min.js`) and sets `window.jspdf = { jsPDF: window.jsPDF }` before loading the app; see **Deployment (GitHub Pages)** below.
- **Never reference the global `jsPDF` from Scala:** Using `js.Dynamic.global.selectDynamic("jsPDF")` can make the Scala.js compiler emit a direct reference to a global variable named `jsPDF`. That causes `ReferenceError: jsPDF is not defined` because we only set `window.jspdf` (lowercase). Only ever use `selectDynamic("jspdf")` on the global; then get the constructor from that object using a **dynamic key** (e.g. `val ctorKey = "js" + "PDF"` and `jspdfObj.selectDynamic(ctorKey)`) so the compiler does not emit a global `jsPDF` access.
- **Calling a JavaScript constructor:** Use **`js.Dynamic.newInstance(ctor)(arg1, arg2, ...)`**, not `ctor.newInstance(args)`. The latter is not a function; `newInstance` is on the `js.Dynamic` object, not on the constructor value.
- **Scala.js global scope:** Do not assign `js.Dynamic.global` to a variable (e.g. `val g = js.Dynamic.global`); the compiler disallows “loading the global scope as a value”. Use `js.Dynamic.global.selectDynamic("...")` directly.
- **Abstraction:** PDF behaviour is described as **`Instruction`**s in `common/pdf/Instruction.scala`. **`JsPDF.run(instructions)`** in `common/pdf/JsPDF.scala` runs them; all use of the jsPDF API is confined there. High-level logic lives in **`PdfUtils.scala`**: it builds instruction lists and calls `JsPDF.run`. Entry point for the book is **`PdfUtils.printBookPdf(PrintBookRequest(title, mosaicPicAndGridOpt))`** (used by both Print PDF buttons).
- **Instruction set:** `PageSize(w, h)`, `AddPage`, **`AddPageWithSize(w, h)`** (for custom page size, e.g. 20×20 cm), `FontSize`, `Text`, `DrawPixelGrid` (pixel or block grid with flat RGB), `DrawStrokeRects` (e.g. plate grid or 4×4 overlay), `FillRect` (e.g. color swatches), `Save(filename)`.
- **Book layout:** See **`print_instructions_layout.md`** for the full spec. In short: (1) Cover (A4, title); (2) Overview (A4, full mosaic + red plate grid); (3) Chapter 1 – Plate overview (A4: small mosaic with current plate in blue, plate image, color swatches + counts); (4) Layer patch pages (20×20 cm, 4 patches per page in 2×2). Each patch is a **cumulative color layer** at **16×16 block** resolution (colors added least-used first); layer background is light grey (configurable later); each patch has a 4×4 stroke grid overlay and “Layer N” label.

---

## 11. Linting (Scalafix)


- **No `return`:** Use if/else or pattern match instead of early return.
- **No `null`:** Use `Option` and `.filter` / `.getOrElse`.
- **No `var` / `while`:** Prefer immutable and for-comprehensions / `.fold` / `.forall`.
- **No default arguments** in some rules; pass explicitly at call sites if required.
- Run `sbt compile` (Scalafix runs as part of compile); fix any reported issues before committing.

---

## 12. Where things live

| Concern | Location |
|--------|----------|
| Screen IDs, ScreenOutput, BuildConfig | `Screen.scala` |
| Stored entities, StorageKeys, Circe decoders | `StoredEntities.scala` |
| Canvas helpers, drawPixelPic | `common/CanvasUtils.scala` |
| Image load, scale detection, downscale | `common/ImageUtils.scala` |
| LocalStorage load/save list | `common/LocalStorageUtils.scala` |
| NES.css class names (container, btn, btnPrimary, …) | `common/nescss/NesCss.scala` |
| Screen layout, flex, gallery cards, progress bar, dropdown | `css/style.css` |
| Apply StoredPalette to PixelPic | `PaletteUtils.scala` |
| PDF instructions (PageSize, AddPageWithSize, DrawPixelGrid, …) | `common/pdf/Instruction.scala` |
| PDF runner (jsPDF behind the scenes) | `common/pdf/JsPDF.scala` |
| High-level PDF book (printBookPdf, chapter/layer logic) | `common/PdfUtils.scala` |
| Print book layout spec (cover, overview, chapter, layers) | `print_instructions_layout.md` |
| App entry: sets window.jspdf, then launches app | `export-wrapper.js` |
| GitHub Pages deploy (Scala.js build + upload-pages-artifact + deploy-pages) | `.github/workflows/deploy.yml` |
| Gallery empty state component | `screens/GalleryEmptyState.scala` |
| Shared screen header (title, buttons, optional name row) | `screens/ScreenHeader.scala` |
| Root app, screen registry | `PixelMosaicMaker.scala` |
| Grid logic (parts, row/column defs) | `GridConfig.scala` |
| Pixel image (crop, setPalette, toImageData) | `PixelPic.scala` |

---

## 13. Deployment (GitHub Pages)

- **Publishing source:** In the repo **Settings → Pages**, set **Source** to **GitHub Actions** (not “Deploy from a branch”). No need to create or maintain a `gh-pages` branch manually.
- **Official deployment flow:** When Source is “GitHub Actions”, GitHub serves from the **artifact** deployed by the workflow. Use **`actions/upload-pages-artifact`** then **`actions/deploy-pages`**. Do **not** use `peaceiris/actions-gh-pages` (which pushes to the `gh-pages` branch); with “GitHub Actions” as source, that branch is not what Pages serves, so you get 404.
- **Workflow requirements:** The deploy job needs **permissions** `contents: read`, `pages: write`, `id-token: write`, and an **environment** `github-pages` (GitHub creates it if missing). Use **concurrency** (e.g. `group: "pages"`) to avoid overlapping deploys.
- **Project site URL:** The site is at **`https://<owner>.github.io/<repo>/`** (e.g. `https://clemniem.github.io/pixel_mosaic_maker/`). The repo name is required; `https://<owner>.github.io/` alone returns 404 for a project site.
- **Scala.js build:** Run **`sbt fullLinkJS`**; output is under **`target/scala-<ver>/pixel_mosaic_maker-opt/`** (`main.js`, `main.js.map`). The workflow assembles a **`dist/`** directory: copy `main.js` (and optional `.map`), add a **production loader** (e.g. `loader.js` that does `import { TyrianApp } from './main.js'; TyrianApp.launch("myapp");`), and an **index.html** that loads **jsPDF from a CDN** (e.g. `jspdf.umd.min.js` from unpkg), sets `window.jspdf = { jsPDF: window.jsPDF }`, then loads the loader as `type="module"`. Local dev continues to use `index.html` + `export-wrapper.js` (target path, npm jspdf); the workflow builds its own dist and never uses `target/` in the deployed artifact.
- **Module kind must be ESModule:** The production loader uses **`import { TyrianApp } from './main.js'`**, which the browser only understands if `main.js` is an **ES module** (real `export` statements). In **`build.sbt`** use **`ModuleKind.ESModule`**, not `CommonJSModule`. With CommonJS, the linker emits `module.exports`, so the browser reports *“The requested module './main.js' does not provide an export named 'TyrianApp'”*; with ESModule, `@JSExportTopLevel("TyrianApp")` becomes a proper `export` and the import works.
- **Docs:** See **`docs/CI_CD_GITHUB_PAGES_ARCHITECTURE.md`** for the high-level pipeline; the actual workflow file is **`.github/workflows/deploy.yml`**.

---

## 14. Testing

- Unit tests in `src/test/scala/clemniem/`: e.g. `ResizeSpec` (scale detection, downscale), `PixelPicTests`, `GridConfigSpec`, `LocalStorageUtilsSpec`. Run with `sbt test`.
- No browser/E2E tests in this repo; manual check in browser after `yarn start` and `fastLinkJS`.

---

## 15. Quick checklist for new features

1. **New screen:** Add `ScreenId`, implement `Screen` (init, update, view), register in `PixelMosaicMaker`’s `ScreenRegistry`.
2. **New stored entity:** Add case class in `StoredEntities.scala`, Circe encoder + backward-compatible decoder if needed, add `StorageKeys` key if it’s a new list.
3. **New navigation payload:** Add a `ScreenOutput` case class; pass it in `NavigateNext(screenId, Some(output))` and handle it in the target screen’s `init`.
4. **Canvas:** Use `CanvasUtils.drawAfterViewReady` or `drawAfterViewReadyDelayed`; use `drawPixelPic` for PixelPic; set canvas size in draw code when it depends on content (e.g. smaller image → smaller canvas).
5. **Gallery empty state:** Use `GalleryEmptyState(emptyText, buttonLabel, msg)`.
6. **Gallery / NES styling:** Use `NesCss` + classes from `css/style.css` as in **§8** (screen-container, gallery-card, gallery-actions, etc.); keep only dynamic values (e.g. color, progress %) as inline styles.
7. After changes: `sbt compile` and `sbt test`; fix Scalafix and test failures.

Using this file together with **FLOW.md** and **README.md** should give enough context to work on the project efficiently.

---

## 16. UI / layout learnings (shared components, build screen, scrollbar)

### 16.1 Shared ScreenHeader and name row

- **ScreenHeader** (`screens/ScreenHeader.scala`): Shared header for editor/creation screens. Use `ScreenHeader(title, buttonRow, nameRow, shortHeader)` where `nameRow` is `Option[Html[Msg]]` (use `ScreenHeader.nameRowInput(...)` for the entity name). No default arguments (Scalafix); pass `None` and `false` when no name row or short header.
- **Name row:** `ScreenHeader.nameRowInput(nameValue, setMsg, inputId, extraRowClass)` produces a full-width name input (CSS `screen-header-name-input`: width 100%). Use for BuildConfig, GridConfig, Palette, ImageUpload (when image loaded). Palette uses `inputId = Some("palette-name")`, `extraRowClass = "palette-name-row"` for a11y and spacing.

### 16.2 Build screen layout

- **Row 0:** Save step + Back. **Row 1:** Previous | Next | [current step number input] | from [total]. No “Go to:” label; the number input is the current step (1-based).
- **Row 2:** Overview canvas + Patch background (color picker only, no hex text input) side by side (`.build-overview-row`).
- **Row 3:** “Current patch by color” title + preview canvas (`.build-preview-row`, `.build-preview-inner`). Canvas displays at **natural size** (no JS scaling); parent has `overflow: auto` so long content scrolls. Do not scale the preview canvas in JS; it caused layout/sizing issues.
- **Patch background default:** When the build config’s palette is loaded, if the user hasn’t changed the background yet (`patchBackgroundColorHex == defaultPatchBackground`), set it to a **palette-derived pastel**: `pastelBackgroundFromPalette(palette.colors)` in BuildScreen. Pastel is light, muted, with a tint opposite to the palette average; clamped to **185–215** per RGB channel so it contrasts clearly with both white and black. Applied in `LoadedBuildConfigs` and `LoadedPalettes` only when still at default.

### 16.3 Number inputs and pixel font

- **Readable number inputs:** With the “Press Start 2P” pixel font, number inputs need explicit sizing. In **`css/style.css`**, a global rule for `input[type="number"]` sets `font-size: 0.95rem`, `line-height: 1.3`, `padding-left/right: 6px`, `box-sizing: border-box` so digits are readable. Grid editor row keeps its own overrides for tight layout.

### 16.4 Consistent outer width and scrollbar

- **Fixed app width:** To prevent the outer container from changing size when switching screens (e.g. Overview vs Palettes) or when the title length varies, use a **fixed width** for the app root and screen container: **`#myapp`** has `width: 42rem; max-width: 100%`. **`.screen-container`** (and `--narrow` / `--wide`) all use `width: 42rem; max-width: 100%`. So the main column is always 42rem when the viewport allows.
- **Scrollbar layout jump:** When content becomes long (e.g. Palettes gallery), the browser shows a vertical scrollbar and the viewport width shrinks by the scrollbar width, causing a visible layout jump. Fix: **`scrollbar-gutter: stable`** on **`html`** so the browser reserves space for the scrollbar from the start. Layout then stays stable when navigating between short and long screens.
