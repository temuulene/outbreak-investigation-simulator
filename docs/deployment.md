# Running and hosting Fieldnotes

## Local setup

Use R 4.6.1 for the reviewed lock. Run `Rscript setup.R`, then `Rscript run.R`.
Dependencies restore to the ignored `artifacts/library`; no global R library is
changed. `renv.lock` pins app, test, and deployment dependencies. Run tests with
that library on `.libPaths()`. The launcher defaults to `127.0.0.1:3874`.

On Debian/Ubuntu, source package installation requires development headers for
curl, OpenSSL, XML, ICU, and zlib. The verification workflow installs
`libcurl4-openssl-dev libssl-dev libxml2-dev libicu-dev zlib1g-dev` before restore.

## Optional dialogue and chat

Set environment variables before starting R (or in private hosting secrets):

- `FIELDNOTES_DIALOGUE_PROVIDER=scripted` (default), `gemini`, or `ollama`.
- `FIELDNOTES_DIALOGUE_MODEL`: an available model for the selected provider.
- For Gemini: `GOOGLE_API_KEY` or `GEMINI_API_KEY`.
- For Ollama: start an OpenAI-compatible local endpoint and configure the base URL
  using `OLLAMA_BASE_URL` (default `http://127.0.0.1:11434`); install the selected
  model on that service first.
- `FIELDNOTES_CHAT_UI=shinychat` enables the optional conversation widget.

Only question text and the allowed topic list are sent to the provider. Never
enter personal or patient information. Responses are validated topic selections
and optional reviewed connective phrases, then scenario-authored factual replies.
The backend has a 30-request session cap, 2-second spacing and 20-second timeout;
provider failures use authored fallback. The session ignores replies made stale
by a restored session or an intervening investigation change. Provider usage may
incur charges. No provider is required for the complete investigation.

`.env.example` documents variables; it is not loaded automatically. Never commit
keys, `.Renviron`, or deployment tokens. Session files hold learner writing; store
and share them deliberately. Facilitator overrides are local review notes, not
access-controlled grades.

## Posit Connect Cloud

The GitHub deployment entry point is `app.R` on `main`. Connect Cloud requires
the committed `manifest.json`; it does not restore directly from `renv.lock`.
Its reviewed runtime range ends at R 4.6.0. The manifest is generated under that
version with the exact package versions in the existing lockfile. Local R 4.6.1
and container settings can remain unchanged.

Publish from [Connect Cloud](https://connect.posit.cloud/): select Shiny, this
repository, branch `main`, and primary file `app.R`. Configure private Variables
before enabling assisted dialogue:

```text
FIELDNOTES_DIALOGUE_PROVIDER=gemini
FIELDNOTES_DIALOGUE_MODEL=gemini-3.5-flash-lite
GOOGLE_API_KEY=<enter the existing key privately in the hosting settings>
```

Use the host's secret variable controls, not a committed `.Renviron`. The
scripted investigation also works without any variables. After publishing,
verify the generated public URL, two separate sessions, save/resume, exports,
and a successful assisted interview. The Free plan has a monthly usage allowance;
Google promotional credits fund eligible Gemini usage separately from hosting.

To regenerate the manifest, use R 4.6.0 and the restored project library:

```text
Rscript setup.R
Rscript scripts/write-connect-manifest.R
```

The generator includes only the app, dependency files, R code, static assets,
scenario definitions, and starter analysis. Git history, tests, documents,
artifacts, saved learner sessions, and local keys are excluded.

GitHub Actions tests R 4.6.0 and 4.6.1, initializes the Shiny app, regenerates the
manifest under R 4.6.0, and checks for drift. If code or dependencies change,
download `connect-cloud-manifest` from that workflow run and review/commit the
updated `manifest.json`. The artifact is uploaded before the drift check so it
remains available when the committed manifest needs refreshing. Installation
timestamps are omitted from package metadata so identical locked dependencies
produce the same committed manifest.

Sources: [R runtime and dependency requirements](https://docs.posit.co/connect-cloud/user/platform/r.html),
[GitHub publishing](https://docs.posit.co/connect-cloud/user/publish/github.html),
[private variables](https://docs.posit.co/connect-cloud/user/manage/content_settings.html),
and [plans](https://connect.posit.cloud/plans).

## Container

Build with `docker build -t fieldnotes .`, then run
`docker run --rm -p 3874:3838 fieldnotes`. The container runs as an unprivileged user
and restores the lockfile. Configure your host's TLS and WebSocket support before
public use. Pass provider settings through runtime secrets. The explicit copy
manifest excludes Git, artifacts, session files, and local secrets.

## shinyapps.io

Authenticate `rsconnect` locally using the host's documented account setup. Set
`FIELDNOTES_SHINYAPPS_ACCOUNT` to that account, then run
`Rscript scripts/deploy-shinyapps.R` from the project root. The script includes only
app entry point, package manifest, lockfile, R code, static assets, reviewed
scenarios, and starter analysis files. This script deploys the scripted experience.
The installed rsconnect adapter supports secret environment-variable forwarding
only for Posit Connect; this script does not forward provider keys to shinyapps.io.
Use a container with runtime secrets or a separately configured Connect deployment
for provider-assisted dialogue. Never add keys to appFiles. Verify your plan's usage
limits and session timeout. Closing/refreshing may discard unsaved work: learners
should save a resumable session.

## Release checks

Run all tests, seed review, and a browser walkthrough: intro and intermediate,
resume after refresh, media reply, repeat collection, exports, and debrief. For an
assisted deployment, check one successful live provider request and an outage
fallback, plus two simultaneous browser sessions. Verify the public URL on a
separate client and record the actual host, release commit, and date.

Hosting destination and credentials are not configured in the repository.
The local Docker engine was unavailable during verification, so a container build
was not run. Local Gemini calls have been verified; public deployment, hosted
provider calls, and human pilot validation must be recorded separately. Tests and
manifest generation do not establish those outcomes.
