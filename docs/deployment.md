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
The local Docker engine was stopped during verification, so a container build was
not run. Public deployment, live provider calls, and human pilot validation
must be recorded separately; local tests do not establish those outcomes.
