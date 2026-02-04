# CI/CD and GitHub Pages – Architecture

This document describes how the Pixel Mosaic Maker app is built, packaged, and deployed to GitHub Pages via GitHub Actions. Use it as a reference to implement the workflow and to configure the repository on GitHub.

---

## 1. Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           GitHub repository                                  │
│  main branch                                                                 │
│    ├── .github/workflows/deploy.yml   ← workflow definition                  │
│    ├── index.html                     ← app shell (loaded in browser)        │
│    ├── tyrianapp.js                   ← dev loader (imports from target/…)    │
│    ├── build.sbt, src/ …                                                      │
└─────────────────────────────────────────────────────────────────────────────┘
                                        │
                                        │ push to main
                                        ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        GitHub Actions (runner)                                │
│  1. Checkout code                                                            │
│  2. Set up Java (e.g. Temurin 21)                                             │
│  3. Build: sbt fullLinkJS                                                     │
│     → writes target/scala-<ver>/<project>-opt/main.js (and .map)             │
│  4. Assemble deployable root:                                                 │
│     • index.html (copy)                                                       │
│     • main.js (copy from fullLinkJS output)                                   │
│     • loader (e.g. tyrianapp.js) that imports './main.js' and launches       │
│  5. Deploy: peaceiris/actions-gh-pages                                       │
│     → publish_dir = assembled directory                                       │
└─────────────────────────────────────────────────────────────────────────────┘
                                        │
                                        │ push to gh-pages (or GITHUB_TOKEN)
                                        ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        GitHub Pages                                          │
│  Serves the contents of the deployed directory as a static site.             │
│  URL: https://<owner>.github.io/<repo>/   (or custom domain if set)           │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 2. Build output (Scala.js)

- **Command**: `sbt fullLinkJS` (production; use `fullLinkJS` for smaller, optimized JS).
- **Output directory**: `target/scala-<scalaVersion>/<projectId>-opt/`
  - For this project: `target/scala-3.6.4/pixel_mosaic_maker-opt/`
  - Contains: `main.js` (and usually `main.js.map`).
- **Important**: The app entry in the repo is `tyrianapp.js`, which imports from a **path under `target/`** (e.g. `./target/scala-3.6.4/pixel_mosaic_maker-fastopt/main.js`). That path is for **local development** only. `target/` is not committed and is not present on GitHub Pages.

So for deployment we must **assemble a separate, self-contained root** that does not depend on `target/`:

- `index.html` (unchanged) loads a script (e.g. a “production” loader).
- That loader imports from a **fixed path** in the deployed root, e.g. `./main.js`.
- `main.js` in the deployed root is a **copy** of the file produced by `fullLinkJS` (from `target/.../pixel_mosaic_maker-opt/main.js`).

---

## 3. Deployable directory layout

What the workflow must produce and pass to `peaceiris/actions-gh-pages` as `publish_dir`:

```
publish_dir/
├── index.html          # copy from repo root (no change)
├── tyrianapp.js        # production loader: import from './main.js'; TyrianApp.launch("myapp");
└── main.js             # copy from target/scala-3.6.4/pixel_mosaic_maker-opt/main.js
```

Optional: copy `main.js.map` for source maps in production.

- **index.html**: already references `./tyrianapp.js`; no change needed.
- **tyrianapp.js (production)**: must not reference `target/`. It should:
  - `import { TyrianApp } from './main.js';`
  - `TyrianApp.launch("myapp");`
- **main.js**: the single file (or the main bundle) produced by `sbt fullLinkJS`.

The workflow will create this directory in the runner (e.g. `./dist` or `./.gh-pages-root`), then set `publish_dir` to that path.

---

## 4. GitHub Actions workflow (high level)

| Step | Purpose |
|------|--------|
| **Checkout** | `actions/checkout@v4` |
| **Set up Java** | `actions/setup-java@v4`, distribution: `temurin`, Java 21 (or version aligned with your build) |
| **Build** | `sbt fullLinkJS` (not `fastLinkJS` for production) |
| **Assemble** | Create `publish_dir`; copy `index.html`; write production `tyrianapp.js`; copy `target/scala-*/pixel_mosaic_maker-opt/main.js` → `publish_dir/main.js` |
| **Deploy** | `peaceiris/actions-gh-pages@v4` with `publish_dir: <assembled dir>`, `github_token: ${{ secrets.GITHUB_TOKEN }}` |

Required permission for the job that deploys: `contents: write` (so the action can push to the `gh-pages` branch or the branch used by Pages).

Trigger: e.g. `on: push: branches: [ main ]` so every push to `main` builds and deploys.

---

## 5. GitHub repository settings (what you need to change)

### 5.1 Enable GitHub Pages and set source to “GitHub Actions”

1. Repo → **Settings** → **Pages** (in the “Code and automation” section).
2. Under **Build and deployment**:
   - **Source**: choose **“GitHub Actions”** (not “Deploy from a branch”).
3. Save.

No need to create a `gh-pages` branch manually; the `peaceiris/actions-gh-pages` action will create/update the branch it uses when you use the default settings.

### 5.2 No extra secrets

- `GITHUB_TOKEN` is provided automatically; the workflow uses `secrets.GITHUB_TOKEN` so the action can push the built site. No extra secrets are required for a standard public repo.

### 5.3 Optional: custom domain

- If you add a custom domain later: same **Settings → Pages** screen, **Custom domain**.
- If you use a project site (e.g. `https://<owner>.github.io/<repo>/`), the app is served at the **root** of that URL; no need for a `<base href>` unless you change the base path later.

---

## 6. Where to see your site

After the first successful deploy:

1. Open your repo on GitHub → **Settings** → **Pages** (under “Code and automation”).
2. At the top you’ll see: **“Your site is live at …”** with the real URL.
3. That URL is always: **`https://<owner>.github.io/<repo>/`**  
   Replace `<owner>` with your GitHub username and `<repo>` with the repository name (e.g. `https://clemniem.github.io/pixel_mosaic_maker/`).

After the workflow finishes, allow **1–2 minutes** for the site to update. The **first time** you enable Pages or switch the source to GitHub Actions, it can take **up to 5–10 minutes** before the site is available. Use the link from **Settings → Pages** to avoid typos.

---

## 7. If you see 404 or a blank page

| Symptom | What to check |
|--------|----------------|
| **“There isn’t a GitHub Pages site here”** or **404** | **Settings → Pages** → **Source** must be **“GitHub Actions”**. After saving, wait **5–10 minutes** (first time can be slow). Then confirm the **Actions** tab shows a successful deploy. |
| **404** | **Actions** tab → last “Deploy to GitHub Pages” run → confirm it completed successfully (green). If it failed, fix the workflow and push again. |
| **Blank white page** | Open the same URL, then **Developer tools (F12) → Console**. Look for red errors (e.g. script failed to load or module not found). Share the error message to debug further. |

---

## 8. Summary checklist

| Item | Where / What |
|------|----------------|
| Workflow file | `.github/workflows/deploy.yml` |
| Build command | `sbt fullLinkJS` |
| Assemble step | Copy `index.html`; write production loader that imports `./main.js`; copy `target/scala-*/pixel_mosaic_maker-opt/main.js` into `publish_dir` |
| Deploy action | `peaceiris/actions-gh-pages@v4`, `publish_dir` = assembled directory |
| Repo setting | **Settings → Pages → Source = GitHub Actions** |
| **Live URL** | **Settings → Pages** shows: **`https://<owner>.github.io/<repo>/`** (replace owner and repo with your username and repo name). |

Once this is in place, every push to `main` will build the optimized JS, assemble the deployable root, and publish it to GitHub Pages.
