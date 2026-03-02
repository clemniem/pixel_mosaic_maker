/**
 * Pixel Mosaic Maker — sample data seed
 *
 * Populates all six galleries with a small set of demo items so you can
 * explore the full workflow without uploading your own images.
 *
 * Usage (browser dev tools console, on any page of the running app):
 *   copy the contents of this file and paste into the console, then press Enter.
 *   Reload the page after running it.
 *
 * Two demos are included:
 *   • Smiley Face   — 32×32 pixel-art face, single-panel layout, step size 16/32
 *   • Quilt Pattern — 32×32 geometric quilt, 2×2 layout, step size 16
 */
(function () {

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  function countColors(pixels) {
    const counts = {};
    pixels.forEach(function (p) { counts[p] = (counts[p] || 0) + 1; });
    return counts;
  }

  // ---------------------------------------------------------------------------
  // Pixel art generators
  // ---------------------------------------------------------------------------

  /**
   * 32×32 smiley face.
   * Palette indices:
   *   0 = white background
   *   1 = golden yellow face
   *   2 = dark brown (eyes & mouth)
   *   3 = pink (cheeks)
   */
  function makeSmilePixels() {
    var W = 32, H = 32;
    var cx = 15.5, cy = 15.5, r2 = 12.5 * 12.5;
    var pixels = [];
    for (var y = 0; y < H; y++) {
      for (var x = 0; x < W; x++) {
        var dx = x - cx, dy = y - cy;
        var inFace = (dx * dx + dy * dy) <= r2;

        // Eyes — two 2×2 dark blocks
        var leftEye  = (x >= 10 && x <= 11) && (y >= 11 && y <= 12);
        var rightEye = (x >= 20 && x <= 21) && (y >= 11 && y <= 12);

        // Cheeks — two 4×2 pink blocks (clipped to face circle)
        var leftCheek  = inFace && (x >= 6  && x <= 9)  && (y >= 17 && y <= 18);
        var rightCheek = inFace && (x >= 22 && x <= 25) && (y >= 17 && y <= 18);

        // Smile — U-shaped dark curve
        var smile = inFace && (
          ((x === 9  || x === 10) && (y >= 20 && y <= 21)) ||
          ((x === 21 || x === 22) && (y >= 20 && y <= 21)) ||
          ((x >= 11 && x <= 20)   && (y >= 22 && y <= 23))
        );

        if (!inFace)                       pixels.push(0);
        else if (leftEye || rightEye)      pixels.push(2);
        else if (smile)                    pixels.push(2);
        else if (leftCheek || rightCheek)  pixels.push(3);
        else                               pixels.push(1);
      }
    }
    return pixels;
  }

  /**
   * 32×32 quilt: 4×4 tiles of 8×8px each, rotated color palette per tile.
   * A 1px white grid line separates every tile (x%8===0 or y%8===0).
   * Palette indices:
   *   0 = white (grid lines)
   *   1 = red
   *   2 = blue
   *   3 = green
   *   4 = yellow/gold
   */
  function makeQuiltPixels() {
    var tc = [[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]];
    var pixels = [];
    for (var y = 0; y < 32; y++) {
      for (var x = 0; x < 32; x++) {
        if (x % 8 === 0 || y % 8 === 0) {
          pixels.push(0);
        } else {
          pixels.push(tc[Math.floor(y / 8)][Math.floor(x / 8)]);
        }
      }
    }
    return pixels;
  }

  // ---------------------------------------------------------------------------
  // Build data
  // ---------------------------------------------------------------------------

  var smilePixels = makeSmilePixels();
  var quiltPixels = makeQuiltPixels();

  // ---- Palettes ----

  var PALETTES = [
    {
      id: 'sample-palette-smiley',
      name: 'Sample: Smiley Colors',
      colors: [
        { r: 255, g: 255, b: 255 }, // 0 → background (white)
        { r: 255, g: 210, b: 30  }, // 1 → face (golden yellow)
        { r: 45,  g: 25,  b: 5   }, // 2 → features (dark brown)
        { r: 255, g: 150, b: 150 }  // 3 → cheeks (pink)
      ]
    },
    {
      id: 'sample-palette-quilt',
      name: 'Sample: Quilt Colors',
      colors: [
        { r: 255, g: 255, b: 255 }, // 0 → grid lines (white)
        { r: 200, g: 45,  b: 45  }, // 1 → red
        { r: 45,  g: 95,  b: 200 }, // 2 → blue
        { r: 45,  g: 165, b: 95  }, // 3 → green
        { r: 255, g: 200, b: 0   }  // 4 → yellow/gold
      ]
    }
  ];

  // ---- Images ----

  var IMAGES = [
    {
      id: 'sample-image-smiley',
      name: 'Sample: Smiley Face',
      pixelPic: {
        width:         32,
        height:        32,
        paletteLookup: [
          { r: 255, g: 255, b: 255, a: 255 },
          { r: 255, g: 210, b: 30,  a: 255 },
          { r: 45,  g: 25,  b: 5,   a: 255 },
          { r: 255, g: 150, b: 150, a: 255 }
        ],
        pixels:  smilePixels,
        palette: countColors(smilePixels),
        name:    'Sample: Smiley Face'
      }
    },
    {
      id: 'sample-image-quilt',
      name: 'Sample: Quilt Pattern',
      pixelPic: {
        width:         32,
        height:        32,
        paletteLookup: [
          { r: 255, g: 255, b: 255, a: 255 },
          { r: 200, g: 45,  b: 45,  a: 255 },
          { r: 45,  g: 95,  b: 200, a: 255 },
          { r: 45,  g: 165, b: 95,  a: 255 },
          { r: 255, g: 200, b: 0,   a: 255 }
        ],
        pixels:  quiltPixels,
        palette: countColors(quiltPixels),
        name:    'Sample: Quilt Pattern'
      }
    }
  ];

  // ---- Layouts ----
  // "Single Panel" — whole 32×32 image as one section (step sizes 16, 32 available)
  // "2×2 Grid"     — four 16×16 quadrants (step size 16 available)

  var LAYOUT_SINGLE = { cols: 1, rows: 1, parts: [
    { x: 0, y: 0, width: 32, height: 32 }
  ]};

  var LAYOUT_2X2 = { cols: 2, rows: 2, parts: [
    { x: 0,  y: 0,  width: 16, height: 16 },
    { x: 16, y: 0,  width: 16, height: 16 },
    { x: 0,  y: 16, width: 16, height: 16 },
    { x: 16, y: 16, width: 16, height: 16 }
  ]};

  var LAYOUTS = [
    { id: 'sample-layout-single', name: 'Sample: Single Panel', config: LAYOUT_SINGLE },
    { id: 'sample-layout-2x2',   name: 'Sample: 2×2 Grid',     config: LAYOUT_2X2   }
  ];

  // ---- Build Configs ----

  var BUILD_CONFIGS = [
    {
      id:       'sample-buildconfig-smiley',
      name:     'Sample: Smiley Mosaic',
      config: {
        grid:      LAYOUT_SINGLE,
        imageRef:  'sample-image-smiley',
        paletteRef:'sample-palette-smiley',
        offsetX:   0,
        offsetY:   0
      }
    },
    {
      id:       'sample-buildconfig-quilt',
      name:     'Sample: Quilt Mosaic',
      config: {
        grid:      LAYOUT_2X2,
        imageRef:  'sample-image-quilt',
        paletteRef:'sample-palette-quilt',
        offsetX:   0,
        offsetY:   0
      }
    }
  ];

  // ---- Builds ----

  var BUILDS = [
    { id: 'sample-build-smiley', name: 'Sample: Smiley Build', buildConfigRef: 'sample-buildconfig-smiley' },
    { id: 'sample-build-quilt',  name: 'Sample: Quilt Build',  buildConfigRef: 'sample-buildconfig-quilt'  }
  ];

  // ---- Print Configs ----

  var PRINT_CONFIGS = [
    {
      id:                     'sample-printconfig-quilt',
      name:                   'Sample: Quilt Print',
      selectedBuildId:        'sample-build-quilt',
      selectedBuildConfigId:  'sample-buildconfig-quilt',
      title:                  'My First Quilt Mosaic',
      stepSizePx:             16,
      pageBackgroundColorHex: '#fdfbe6',
      patchBackgroundColorHex:'#dcdcdc',
      stacked:                true,
      printerMarginMm:        3,
      contentTopOffsetMm:     2
    }
  ];

  // ---------------------------------------------------------------------------
  // Write to localStorage (prepend; skip items whose id already exists)
  // ---------------------------------------------------------------------------

  function seed(key, items) {
    var existing = JSON.parse(localStorage.getItem(key) || '[]');
    var existingIds = {};
    existing.forEach(function (x) { existingIds[x.id] = true; });
    var toAdd = items.filter(function (x) { return !existingIds[x.id]; });
    localStorage.setItem(key, JSON.stringify(toAdd.concat(existing)));
    console.log('Seeded ' + toAdd.length + ' item(s) → ' + key);
  }

  seed('palettes',     PALETTES);
  seed('images',       IMAGES);
  seed('gridConfigs',  LAYOUTS);
  seed('buildConfigs', BUILD_CONFIGS);
  seed('builds',       BUILDS);
  seed('printConfigs', PRINT_CONFIGS);

  console.log('✓ Sample data loaded. Reload the page to see all galleries populated.');

}());
