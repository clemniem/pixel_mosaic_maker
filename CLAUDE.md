# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Local Development
```sh
yarn install   # install JS deps (first time)
yarn start     # start Parcel dev server at http://localhost:1234
```
In a separate terminal:
```sh
sbt fastLinkJS   # compile Scala → JS (dev); rerun after code changes
```
The browser hot-reloads after each `fastLinkJS`.

### Compile, Test, Production
```sh
sbt compile      # compile + run Scalafix lints
sbt test         # run unit tests (munit)
sbt fullLinkJS   # optimized production build → target/scalajs-opt/
```
After any change: run `sbt compile` then `sbt test` before committing. Scalafix runs automatically on compile.

## Architecture

Scala 3 + Scala.js + **Tyrian** (Elm-style SPA) + **Circe** (JSON) + **Cats Effect IO**. Parcel is the local dev server; production deploys to GitHub Pages via GitHub Actions.

### Entry Point & Navigation

`PixelMosaicMaker` in `PixelMosaicMaker.scala` is a `TyrianIOApp`. Root model: `RootModel(registry, currentScreenId, currentModel: Any)`. Screens are type-erased; each implements `Screen` with `Model`, `Msg`, `init`, `update`, `view`, `wrapMsg`.

Navigation: screens emit `NavigateNext(screenId, Some(ScreenOutput))`. The root calls the target screen's `init(Some(output))` and replaces `currentModel`. No URL routing (`Routing.none`).

### Persistence

All lists (grid configs, palettes, images, build configs, builds) use **LocalStorage** via `LocalStorageUtils` (`loadList`/`saveList`). Keys are in `StorageKeys` inside `StoredEntities.scala`. All entities use Circe `deriveEncoder`/`deriveDecoder`. For backward compatibility with old JSON formats, use `withX.or(withoutX)` custom decoders.

### Key Files

| Concern | Location |
|---------|----------|
| Screen IDs, ScreenOutput, BuildConfig | `Screen.scala` |
| Stored entities, StorageKeys, Circe codecs | `StoredEntities.scala` |
| Canvas helpers, `drawPixelPic` | `common/CanvasUtils.scala` |
| NES.css class name constants | `common/nescss/NesCss.scala` |
| PDF instruction set | `common/pdf/Instruction.scala` |
| PDF runner (jsPDF wrapper) | `common/pdf/JsPDF.scala` |
| High-level PDF book logic | `common/PdfUtils.scala` |
| Gallery empty state component | `screens/GalleryEmptyState.scala` |
| Shared screen header | `screens/ScreenHeader.scala` |
| Root app + screen registry | `PixelMosaicMaker.scala` |

### BuildConfig vs StoredBuild

**BuildConfig** = template (grid + image + palette + offset), stored in `buildConfigs`. **StoredBuild** = one run pointing at a `buildConfigRef` with a `savedStepIndex`, stored in `builds`. First "Save step" creates a `StoredBuild`; subsequent saves update it.

## Scalafix Rules (enforced on `sbt compile`)

- **No `return`** — use if/else or pattern match.
- **No `null`** — use `Option`.
- **No `var` / `while`** — use immutable vals, for-comprehensions, `.fold`.
- **No default arguments** — pass all args explicitly at every call site.
- **No `@unused`** — remove unused parameters or make them `Option`.
- **Varargs (Scala 3.4+)** — use `x*` not `x: _*`. Build the list in a variable first: `val attrs = List(...); input(attrs*)`.

## Tyrian / Scala.js Patterns

- **`disabled`**: not reliable on buttons — use conditional style (`cursor: not-allowed; opacity: 0.6`) or a CSS class instead.
- **Reserved keywords**: use backticks — `` `class` := "..." ``, `` `type` := "number" ``.
- **`onLoad`**: takes a single message; must go on the **parent** element: `div(onLoad(DrawMsg))(canvas()())`, not on the canvas itself.
- **Canvas drawing**: always use `CanvasUtils.drawAfterViewReady` or `drawAfterViewReadyDelayed`; never draw before the canvas is in the DOM. `drawPixelPic(canvas, ctx, pic, targetWidth, targetHeight, dx, dy)` has no default args — always pass `dx` and `dy` explicitly (`0, 0` when no offset needed).
- **NES.css radio buttons**: use `input.nes-radio` + an **immediate sibling `span`** (NES.css draws the dot via `input:checked + span::before`). Use `onClick` (not `onInput`) to update the model. Prefer radios over checkboxes — Press Start 2P lacks a checkmark glyph.

## jsPDF Access Pattern

Never reference the global `jsPDF` (uppercase) from Scala. Use `selectDynamic("jspdf")` (lowercase) on `js.Dynamic.global`, then get the constructor via a dynamic key (`val ctorKey = "js" + "PDF"`). Use `js.Dynamic.newInstance(ctor)(args)` for construction. Do not assign `js.Dynamic.global` to a variable.

Local dev: jsPDF is bundled via npm + `export-wrapper.js`. Production: loaded from CDN in the workflow-generated `dist/index.html`.

## NES.css & Styling

Always use constants from `NesCss` (e.g. `NesCss.btn`, `NesCss.btnPrimary`, `NesCss.btnError`) — never raw strings. For custom clickable elements use `cursor: var(--nes-pointer)` (not `cursor: pointer`). Only dynamic values (e.g. color `background`, progress bar `width %`) belong as inline styles; everything else uses CSS classes.

## Deployment

`sbt fullLinkJS` outputs to `target/scalajs-opt/`. The GitHub Actions workflow assembles `dist/` and deploys to GitHub Pages. The workflow **generates** `dist/index.html` via heredoc — it does **not** copy the repo's `index.html`. Any change to the local `index.html` (new scripts, link tags) must be replicated in `.github/workflows/deploy.yml`.

GitHub Pages source must be set to **GitHub Actions** (not "Deploy from a branch") in repo Settings → Pages. The service worker cache name in `sw.js` contains a `__BUILD_TS__` placeholder replaced by the workflow on each deploy — no manual version bumps needed.

## New Screen Checklist

1. Add `ScreenId`, implement `Screen` (init, update, view), register in `ScreenRegistry`.
2. New stored entity → case class in `StoredEntities.scala` + Circe encoder/decoder + `StorageKeys` key.
3. Navigation payload → `ScreenOutput` case class, pass via `NavigateNext(screenId, Some(output))`.
4. Canvas: use `CanvasUtils.drawAfterViewReady` / `drawAfterViewReadyDelayed`; use `drawPixelPic` for `PixelPic`.
5. Gallery list: use `GalleryLayout.listWithAddActionAndPagination`; model needs `currentPage: Int` + `PreviousPage`/`NextPage` msgs.
6. Empty state: use `GalleryEmptyState(emptyText, buttonLabel, msg)`.
7. Editor/creation screen header: use `ScreenHeader(title, buttonRow, nameRow, shortHeader)` with no default args.
