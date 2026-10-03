# Verification — 2026-10-02

## Completed software checks

- `Rscript tests/run.R`: 215 passing expectations, with no failures or warnings.
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
- Both seed-review scripts regenerate their committed instructor reviews.
  The intermediate reference seed shows the apparent crude coleslaw association
  attenuating in chicken-salad strata. These are synthetic teaching examples;
  screening conditions the generated seeds rather than estimating real-world
  outbreak frequencies.
- The reviewed lockfile restored successfully into the project-local ignored
  library. The deployment manifest resolves 73 dependency records with rsconnect.
  Application and container copy manifests exclude artifacts and local secrets.

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

The expanded implementation at `ba1961a` passed
[GitHub Actions run 37103565970](https://github.com/temuulene/outbreak-investigation-simulator/actions/runs/37103565970).
The Ubuntu runner installed system prerequisites, restored the lockfile, passed
the full suite, regenerated both seed reviews, and passed the documentation drift
check. The first expanded run exposed missing Linux development headers; the
workflow and container manifest now include those prerequisites. The original
main release also passed its earlier workflow.

Hosting destination and credentials have not been configured. The local Docker
daemon was stopped, so a container build was not run. Public hosting, live Gemini
or Ollama requests, live-provider outage checks, and simultaneous assisted browser
sessions remain unverified. The scripted investigation needs no external provider.

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
