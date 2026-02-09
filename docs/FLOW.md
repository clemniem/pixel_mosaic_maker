# Pixel Mosaic Maker – App Flow

This document describes the single-page app structure. The app is built with **Tyrian** (Elm-style) and **Scala.js**. Each step is a **Screen** with its own model, messages, and view. Data flows between steps via **ScreenOutput** and **NavigateNext**.

Inspired by [gbcamutil](https://github.com/your-repo/gbcamutil); this app is a rewrite with a clearer step-by-step flow.

---

## Entry: Overview (Home)

**Purpose:** Hub to reach all galleries.

- Links to five galleries: **GridConfigs**, **Palettes**, **Images**, **BuildConfigs**, **Builds**.
- No persistent data; navigation only.

---

## 1. GridConfig

**Purpose:** Define a grid of **plates** (Lego-style plates that together form the whole image).

- **Gallery:** `GridConfigGalleryScreen` – list of saved grid configs; Create / Edit / Delete.
- **Editor:** `GridConfigScreen` – define grid by rows or columns (heights, cell widths/heights). Grid is normalized (rectangular); optional enlarge-cells vs add-plates when saving.
- The grid is the subdivision of the final mosaic into plates. Each plate has an internal resolution; the **Build** step uses **16×16** cells per plate.
- **Output:** Saved as `StoredGridConfig` in LocalStorage under `StorageKeys.gridConfigs`. Edit uses `EditGridConfig(stored)`.

---

## 2. Images

**Purpose:** Upload pixel art and prepare it for the mosaic pipeline.

- **Gallery:** `ImagesGalleryScreen` – list of saved images with preview and palette row; click palette to save as palette. Create (→ Upload) / Delete.
- **Upload:** `ImageUploadScreen` – pick file; image is decoded, nearest-neighbor scale detected and downscaled if needed, max 400×400 px. Stored as `StoredImage` (contains `PixelPic`) under `StorageKeys.images`.
- **Output:** `StoredImage` with `PixelPic` (width, height, palette, pixels). No direct ScreenOutput for “chosen image”; selection happens in **BuildConfig**.

---

## 3. Palettes

**Purpose:** Define **palette combinations** (which colors map to which slots).

- **Gallery:** `PalettesGalleryScreen` – list of saved palettes (name + swatches); Create / Edit / Delete.
- **Editor:** `PaletteScreen` – add/remove colors (1–16), hex + color picker per color. Stored as `StoredPalette` under `StorageKeys.palettes`.
- **Output:** Selection happens in **BuildConfig**. From Images gallery, “save as palette” uses `NewPaletteFromImage(name, colors)` to open Palette editor with pre-filled colors.

---

## 4. BuildConfig

**Purpose:** Define **what** to build – stateless configuration (template).

- **Gallery:** `BuildConfigGalleryScreen` – list of saved build configs with small preview canvas; Create / Edit / Delete.
- **Editor:** `BuildConfigScreen` – choose one **GridConfig**, one **Image**, one **Palette**, and **offset (X, Y)**. Offset is clamped so the grid stays inside the image. Two canvases: **Overview** (full image + grid + current region) and **Preview** (grid region only). Stored as `StoredBuildConfig` under `StorageKeys.buildConfigs`. Optional `savedStepIndex` on config is legacy; step is now stored per **StoredBuild**.
- **BuildConfig** = Grid + ImageRef + PaletteRef + OffsetX + OffsetY (no step state).
- **Output:** Saved config; “Start Build” is done from **Builds** gallery, not from here.

---

## 5. Builds (list of build runs)

**Purpose:** Manage multiple **build runs** and start new ones.

- **Screen:** `BuildsGalleryScreen`.
- **Data:** Loads **builds** from `StorageKeys.builds` and **build configs** from `StorageKeys.buildConfigs`.
- **List:** Each item is a **StoredBuild**: `id`, `name`, `buildConfigRef` (id of a BuildConfig), `savedStepIndex`. Display: name, config name, “step N” if saved, and **Resume** button.
- **Start new build:** Button “+ Start new build” reveals a **dropdown** of build configs (no dropdown until this button is clicked). User picks a config and clicks **Start** → navigates to **Build** screen with `StartBuild(storedBuildConfig)`.
- **Resume:** Click **Resume** on a list item → `ResumeBuild(storedBuild)` → **Build** screen loads the referenced config and restores `savedStepIndex`.

---

## 6. Build (step-by-step runner)

**Purpose:** Run one build instance: iterate plates, then 16×16 cells per plate; show current patch; save step to the **build** (in the builds list).

- **Entry:** Either `StartBuild(storedBuildConfig)` (new run; no StoredBuild yet) or `ResumeBuild(storedBuild)` (load config by `buildConfigRef`, restore step).
- **State:** Current **StoredBuildConfig** (or resolved from StoredBuild), optional **currentBuild** (StoredBuild), **stepIndex**, images/palettes loaded for drawing.
- **Steps:** For each plate in the grid, for each 16×16 cell in that plate, one step. Order: plate-by-plate, row-by-row within plate. Step = (imageX, imageY) of the 16×16 patch.
- **UI:** Overview canvas (grid region only, current 16×16 patch highlighted in green); Preview canvas (current 16×16 patch only); step counter; Previous / Next; “Go to” step input; **Save step** (writes to `StorageKeys.builds`: create or update **StoredBuild** with current step); Back → Builds gallery.
- **Save step:** If there is no `currentBuild`, creates a new StoredBuild (id, name from config, `buildConfigRef`, `savedStepIndex`) and appends to the builds list. If there is a `currentBuild`, updates that build in the list. After save, model holds the updated `currentBuild` so further saves update the same build.

---

## 7. Print Instructions (future)

**Purpose:** Produce a PDF of the full instruction set (same config as Build, non-interactive).

- **Screen:** `PrintInstructionsId` exists; implementation is future.
- **Output:** PDF generated (download / open).

---

## Flow summary

```
                    ┌─────────────┐
                    │  Overview   │
                    └──────┬──────┘
         ┌─────────────────┼─────────────────┐
         ▼                 ▼                 ▼
   GridConfigs        Palettes          Images
         │                 │                 │
    [Create/Edit]    [Create/Edit]    [Upload]
         │                 │                 │
         └─────────────────┼─────────────────┘
                           ▼
                    BuildConfigs
                    [Create/Edit]
                           │
                           ▼
                      Builds (list)
                    [Resume] or [+ Start new build → dropdown → Start]
                           │
                           ▼
                       Build (runner)
                    [Step nav, Save step]
                           │
                           ▼
                 Print Instructions (future)
```

- **Galleries** list stored entities (LocalStorage); **editors** create/update them. **BuildConfig** is a template; **Builds** list **StoredBuild** instances (each references a config + saved step). **Build** screen runs one instance and persists step to the builds list.
