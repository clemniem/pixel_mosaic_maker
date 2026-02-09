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

## Setup instructions

To run the program in a browser you will need to have yarn (or npm) installed.

Before your first run and for your tests to work, **you must** install the node dependencies with:

```sh
yarn install
```

This example uses Parcel.js as our bundler and dev server, there are lots of other options you might prefer like Webpack, scalajs-bunder, or even just vanilla JavaScript.

We recommend you have two terminal tabs open in the directory containing this README file.

In the first, we'll run sbt.

```sh
sbt
```

From now on, we can recompile the app with `fastLinkJS` or `fullLinkJS` _**but please note** that the `tyrianapp.js` file in the root is expecting the output from `fastLinkJS`_.

Run `fastLinkJS` now to get an initial build in place.

Then start your dev server, with:

```sh
yarn start
```

Now navigate to [http://localhost:1234/](http://localhost:1234/) to see your site running.

If you leave parcel's dev server running, all you have to do is another `fastLinkJS` or `fullLinkJS` and your app running in the browser should hot-reload the new code.

## Libraries

| Library | Purpose |
|--------|--------|
| [Tyrian](https://github.com/PurpleKingdomGames/tyrian) | Elm-style UI framework for Scala.js (model / update / view) |
| [NES.css](https://nostalgic-css.github.io/NES.css/) | Retro pixel-art CSS framework for the UI look |
| [Scala.js](https://www.scala-js.org/) | Scala compiled to JavaScript |
| [Parcel](https://parceljs.org/) | Dev server and bundler |