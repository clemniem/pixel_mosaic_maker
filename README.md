# Pixel Mosaic Maker

This project was **entirely written with [Cursor](https://cursor.com)** (AI-assisted editor).

---

Pixel Mosaic Maker is a single-page web app that turns **pixel art** (e.g. Game Boy Camera photos or other low-res images) into **printable mosaic instructions**—step-by-step guides you can follow to recreate the image with physical tiles or bricks (Lego-style plates, perler beads, etc.).

**What it does:**

1. You define a **grid** that splits the image into plates (regions).
2. You **upload** your pixel art; the app detects scale and keeps it crisp.
3. You create **palettes** (color sets) and **build configs** that tie together: grid + image + palette + offset.
4. You run a **build** to generate the mosaic layout.
5. You get **print instructions**: a PDF book with a cover, overview, per-plate chapters (with color swatches and piece counts), and layer-by-layer patch pages showing which colors to place where.

Everything runs in the browser. Data (grids, palettes, images, build configs, builds) is stored in **LocalStorage**, so no server or account is required. The app is built with **[Tyrian](https://github.com/PurpleKingdomGames/tyrian)** (Elm-style architecture) and **Scala.js**, and uses **[NES.css](https://nostalgic-css.github.io/NES.css/)** for the retro pixel-UI look.

See **[docs/FLOW.md](docs/FLOW.md)** for the full six-step app flow: GridConfig → ImageUpload → Palettes → BuildConfig → Build → Print Instructions.

## Run locally

**Requirements:** [Node](https://nodejs.org/) (for yarn/npm), [sbt](https://www.scala-sbt.org/).

```sh
yarn install
yarn start
```

In another terminal, from the project root:

```sh
sbt
sbt:...> fastLinkJS
```

Open [http://localhost:1234/](http://localhost:1234/). After code changes, run `fastLinkJS` again and the browser will hot-reload.

## Libraries

| Library | Purpose |
|--------|--------|
| [Tyrian](https://github.com/PurpleKingdomGames/tyrian) | Elm-style UI framework for Scala.js (model / update / view) |
| [NES.css](https://nostalgic-css.github.io/NES.css/) | Retro pixel-art CSS framework for the UI look |
| [Scala.js](https://www.scala-js.org/) | Scala compiled to JavaScript |
| [Parcel](https://parceljs.org/) | Dev server and bundler |

## Feedback

Feedback is welcome via [GitHub issues](https://github.com/clemniem/pixel_mosaic_maker/issues) or pull requests.