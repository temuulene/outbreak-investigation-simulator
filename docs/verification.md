# Verification — 2026-10-03

## Completed software checks

- `Rscript tests/run.R`: 222 passing expectations, with no failures or warnings.
  Coverage includes both scenarios, bounded seed screening, time-ordered events,
  repeated collection, stratified denominators, media evidence references,
  formative assessment, facilitator overrides, and guided navigation.
- Save/resume tests reconstruct an identical investigation, preserve frozen
  snapshots and future task results, and reject malformed types, references,
  chronology, contact histories, and oversized files before replacing a session.
  The server restores the latest snapshot and refreshes debrief review controls.
- Optional dialogue tests cover structured output validation, authored factual
  replies, separate sessions, request/input/rate limits, stale responses, errors,
  and timeout fallback. Chat-widget regression tests cover its list-shaped text
  input and reject unsupported attachment payloads.
- Local Gemini verification on 2026-10-03 used `gemini-3.5-flash-lite`. The
  provider returned approved menu topics, the engine recorded an `assisted`
  interview, and the browser displayed the authored answer with a five-minute
  game-time advance. An initial live test exposed ellmer's enum arrays being
  represented as R factors; the adapter now validates their character labels.
  Regression tests cover valid and empty factors plus unknown, missing, and
  duplicate values. The test runner temporarily disables live providers and keys
  so ordinary regression tests cannot spend configured API credits.
- Both seed-review scripts regenerate their committed instructor reviews.
  The intermediate reference seed shows the apparent crude coleslaw association
  attenuating in chicken-salad strata. These are synthetic teaching examples;
  screening conditions the generated seeds rather than estimating real-world
  outbreak frequencies.
- The reviewed lockfile restored successfully into the project-local ignored
  library. The deployment manifest resolves 73 dependency records with rsconnect.
  Application and container copy manifests exclude artifacts and local secrets.
- Connect Cloud compatibility passed under Linux R 4.6.0 and R 4.6.1 in
  [run 37106741570](https://github.com/temuulene/outbreak-investigation-simulator/actions/runs/37106741570).
  Each runtime restored the pinned dependencies, passed all 222 expectations,
  regenerated both scenario reviews, and passed the documentation drift check.
  `manifest.json` was generated with rsconnect under R 4.6.0: 22 runtime files,
  73 packages matching `renv.lock`, and no references to local secrets or learner
  saves. The source file checksums were also verified. No application or package
  version changes were needed for the older supported R runtime.

## Browser checks

- The default experience retains one main activity, three short reflection
  prompts, optional tools, and a single opening action. Intro interviews and the
  intermediate challenge were exercised in the browser.
- Scheduled late reports and a media inquiry were viewed, an evidence-linked
  media response was saved, and a resumable JSON file was downloaded. That actual
  downloaded file restored successfully through the R importer. Browser automation
  could not open the upload chooser; the server upload/restore path is covered by
  tests, but a manual browser upload remains a release walkthrough item.
- The optional shinychat interface sent an open question, displayed the authored
  menu reply, cleared its pending state, and advanced game time by five minutes.
  Switching to the cook showed a separate conversation. No browser console errors
  or Shiny output errors were observed in those checks.
- The welcome screen at a 390 px viewport had no horizontal overflow. The final
  desktop preview shows the completed guided interface.
- Earlier browser checks covered guest-list retrieval, team collection, frozen
  snapshots and Excel export. Server tests also exercise the full investigation,
  recommendation, debrief, retry, reveal, and draft-preserving navigation.

## Release and external checks

The owner published the public pilot on Posit Connect Cloud on 2026-10-03:
[live simulator](https://temuulen-outbreak-investigation-simulator.share.connect.posit.cloud/).
A fresh browser visit loaded the welcome screen. The three opening reflection
screens saved and advanced to the interview step. Asking Pat about the menu
returned scenario-authored food names, displayed `Reply added · assisted`, and
advanced game time from Monday 09:00 to 09:05. A second browser tab opened at the
untouched welcome screen while the first retained its interview. This confirms
basic hosted interaction and separate starting sessions; it is not a load test or
a complete two-session assisted investigation.

The repository baseline during that check was `5d74a05`, which passed
[run 37107329943](https://github.com/temuulene/outbreak-investigation-simulator/actions/runs/37107329943)
under Linux R 4.6.0 and R 4.6.1, including Shiny initialization, all 222 expectations,
scenario-review drift checks, manifest validation, and deterministic manifest
regeneration. The public UI does not expose the hosted revision SHA, runtime
version, or provider/model settings, so these were not independently read from
the host. The manifest targets R 4.6.0; the hosted UI confirms an accepted assisted
reply rather than exposing provider credentials.

The expanded implementation at `ba1961a` passed
[GitHub Actions run 37103565970](https://github.com/temuulene/outbreak-investigation-simulator/actions/runs/37103565970).
The Ubuntu runner installed system prerequisites, restored the lockfile, passed
the full suite, regenerated both seed reviews, and passed the documentation drift
check. The first expanded run exposed missing Linux development headers; the
workflow and container manifest now include those prerequisites. The original
main release also passed its earlier workflow.

The local Docker daemon was unavailable, so a container build was not run. Hosted
save/resume and workbook downloads, live Ollama requests, forced live-provider
outage checks, and two simultaneous assisted investigations remain unverified.
Successful assisted replies do not establish promotional credit deduction. The
scripted investigation needs no external provider.

No real pilot participants have tested the app. Educational effectiveness,
facilitator scoring reliability, and a full accessibility audit remain unverified.
Use the pilot guide and observation template to record those human checks.

## Local environment

The local runtime was R 4.6.1 on Windows. The inherited `C.UTF-8` locale is not
recognised by Windows R, so tests used `LC_ALL=English_United States.utf8`.
`run.R` corrects the character locale when needed. App source is UTF-8, and the
Sass disk cache is disabled. Dependencies restore inside `artifacts/`; no global
library is changed.

The original build passed 72 expectations and the first guided revision passed
96 expectations on 2026-10-01. Those historical results are superseded by the
expanded suite above.
