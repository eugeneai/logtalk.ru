# AGENTS.md

## Workspace layout

- This directory is the `logtalk` home/deploy directory, not a single git repo. There are two independent checkouts:
  - `src/` — Gatsby 5 site for the Russian Logtalk course. Remote `origin` is `eugeneai/logtalk.ru`, branch `main`; remote `py` tracks the original `programming-24` material template.
  - `api/` — FastAPI + SQLAlchemy auth/course API in its own repo (`eugeneai/mooc-backend`).
- Run site commands in `src/` and API commands in `api/`; the root has no build.
- `public` is a symlink to `src/public`. Production nginx serves that directory and proxies `/api`, `/oauth`, `/openapi.json`, and `/api-docs` to FastAPI on `127.0.0.1:8000` (see `/etc/nginx/sites-available/logtalk.ru`).
- Do **not** try to update the platform by pulling from the `mooc`/`py` template remotes. They contain the old Helsinki Python MOOC platform; platform changes are made directly in `src/` and its dependencies.

## Website (`src/`)

- Node.js **>= 22.12** is required (Gatsby 5 and its `sanitize-html` dependency). CI and `src/Dockerfile` use Node 22 LTS. A local Node 20 can install with `yarn install --ignore-engines`, but that is only a workaround.
- Install with `corepack enable && yarn install --frozen-lockfile`. `src/yarn.lock` is the authoritative lockfile. Do not use `npm ci`: `package-lock.json` is not tracked.
- Develop: `yarn develop`. Build: `yarn build` → `src/public` (gitignored).
- Verification: `yarn build` is the real check; there is no test suite (`yarn test` always exits 1). On a low-memory host use `GATSBY_TELEMETRY_DISABLED=1 GATSBY_CPU_COUNT=1 NODE_OPTIONS=--max-old-space-size=1024 yarn build`; on a machine with more memory use `--max-old-space-size=4096`.
- Current known build blocker: Gatsby 5 reaches static HTML generation but fails on `/support-and-assistance/` in `rehype-react` v6 → `property-information` v5 with React 18. See `TODO.md` for the exact next step. Do not assume a successful production build until this is fixed.
- Format check: `yarn prettier-check`; auto-fix: `yarn format`. Prettier only covers `src/**/*.js`; it does not cover `gatsby-config.js`, `gatsby-ssr.js`, or `plugins/**/*.js`, so check those separately with `npx prettier --check ...`. The pre-commit hook `src/bin/check-code-style.sh` runs the configured check.
- Deploy: production uses the local build. After updating: `cd src && git pull origin main && yarn install --frozen-lockfile && yarn build`. GitHub Actions builds on pushes to `main` and deploys `public` to `gh-pages`.

### Gatsby 5 / platform notes

- Gatsby is 5.16.x, React is 18, GraphQL is 16, and Material-UI is still v4 (JSS-based).
- With GraphQL 16, constructors must be called with `new`, e.g. `new GraphQLList(...)`.
- `gatsby-link` has no default export in Gatsby 5; use `import { Link } from "gatsby-link"`.
- Do not add `gatsby-plugin-material-ui`: it targets MUI v5/Emotion, while this site uses MUI v4/JSS. SSR styles are handled in `gatsby-ssr.js` via `ServerStyleSheets`.
- The site has a single banner source, `src/banner.svg`; `src/images/banner.svg` was removed.

## Content

- Content is in `src/data/*.md`; `src/course-settings.js` is the authoritative course config (Russian Logtalk, site URL, TMC/quizzes IDs). The README and some `data/` files are inherited Finnish/Python MOOC text; do not assume they match the actual course.
- When authoring or rewriting course material, use this source priority: (1) the Logtalk course topic and existing course structure, (2) `https://logtalk.org/` Handbook/APIs, (3) `https://haskell.mooc.fi/` for functional-programming pedagogy and comparisons, (4) the original Helsinki MOOC Python material as a last-resort structural template. Do not copy Python exercises into the Logtalk course.
- Required frontmatter is `path` and `title`; `src/data/frontmatter-guide.md` (Finnish) documents the rest. Custom tags such as `<quiz id=...>` and `<programming-exercise ...>` map to `src/partials/index.js`; exercise tags are also parsed by `src/plugins/gatsby-transformer-moocfi-exercises/gatsby-node.js`, so a new exercise tag needs both.

## Exercises

- `src/exercises/` holds Logtalk exercise sets and tests. Run a set from `src/exercises`, e.g. `swilgt -l Set1TestsLoader.lgt -g halt`. `swilgt`/Logtalk is not installed here (only SWI-Prolog); use the Logtalk toolchain image in `src/docker/`. `src/docker/` is a vendored Logtalk image, not the site deployment.
- The exercise harness is `src/exercises/testlib/testing.lgt` plus `studyunit.lgt`. Tests use `test(Name, Outcome, Options, Goal)` with `true`/`fail`/`all(...)`/`none(...)` outcomes and options such as `condition/1`, `explain/1`, `each_*`; exceptions in test goals are reported as failures. Use `Set5Tests.lgt` as the current reference example; `Set5Solution.lgt` is the working reference solution.
- The old advanced exercise set was renamed to `SetSearch*.lgt`; do not confuse it with the current course `Set6`.
- Ignore `.orig` files (`src/i18n.js.orig`, `src/partials/SampleData.js.orig`, `src/services/*.orig`) and `updates*`; they are stale template artifacts.

## API (`api/`)

- FastAPI app is `main.py`; SQLAlchemy models/session logic is `db.py`.
- Run from `api/` with the pyenv virtualenv named in `.python-version` (`logtalk`): `pyenv activate logtalk && uvicorn main:app --reload`.
- `db.py` connects at import time and calls `create_missing_tables()`; the connection string is hardcoded as `postgresql+psycopg://mooc:mooc312@ktulhu/mooc`. The API will not import if that Postgres host/database is unreachable.
- Endpoints are `/oauth/token`, `/api/v8/users...`, `/api/v8/org/...`; nginx forwards them to port 8000 without rewriting the path.
- The worktree has uncommitted edits and an Emacs autosave file `api/#db.py#`; inspect `git status` before changing or committing.

## Git

- `src` and `api` are separate git histories; commit only in the intended repo and never commit/push unless asked.
- If a commit hook fails only because the local Node version is below 22.12, run the checks explicitly and use `git commit --no-verify`; do not lower the documented Node requirement.
