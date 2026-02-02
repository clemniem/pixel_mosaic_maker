# Pixel Mosaic Maker

Single-page app to create mosaic instructions from pixel art (e.g. Game Boy Camera photos → Lego-style instructions). Built with **Tyrian** and **Scala.js**.

See **[FLOW.md](FLOW.md)** for the six-step app flow: GridConfig → ImageUpload → Palettes → BuildConfig → Build → Print Instructions.

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