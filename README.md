# Fieldnotes · Outbreak investigation lab

A working R Shiny pilot based on the **Outbreak Simulator Design Sketch**.
Investigate a fictional community potluck through interviews, case definition,
targeted collection, analysis, proportionate action, and a structured debrief.

The interface guides learners through one activity at a time. Evidence, delegated
tasks, and protective actions stay a click away. Short checkpoint prompts preserve
drafts when going back; advanced analysis and settings are optional disclosures.

**[Open the live simulator](https://temuulen-outbreak-investigation-simulator.share.connect.posit.cloud/)**
— hosted on Posit Connect Cloud. Allow about 80 minutes for an investigation.
Use **Help & session → Save resumable session** to keep your progress before
closing or refreshing the browser. Resume that file from the welcome screen.

## Run locally

Use R 4.6.1 for the reviewed dependency lock. In this project directory:

```r
source("setup.R") # first-time dependency restore using renv
source("run.R")
```

Or use `Rscript run.R`, then visit <http://127.0.0.1:3874>.
For an existing R installation with dependencies already available,
`shiny::runApp()` also works. No API key is required. The server binds to localhost by default. Set `FIELDNOTES_HOST` and `PORT` when hosting; see [deployment](docs/deployment.md).

## Included

- Seeded, separate truth / reported / collected data and six scripted contacts.
- Free-text topic detection, explicit topic fallback, and independent histories.
- Preservation deadline, queued results, late reports, and contemporaneous actions.
- Structured case definitions with live counts and revision reasons.
- Questionnaire coverage preview; unasked fields remain missing.
- Frozen CSV and Excel exports, starter R script, RR/attack-rate checks, and curve.
- Three reasoning checkpoints, five-domain review, optional truth reveal, and retry.
- JSON session export containing learner-visible evidence, not hidden guest truth.

Choose introductory or intermediate challenges, with optional reviewed random
variations. Media requests, stratified comparisons, repeat collection, and a
transparent facilitator review are available without adding steps to the main flow.
Save a resumable JSON session from Help & session; resume it on the welcome screen.
The evidence-only export remains available separately. Resume files contain local
learner work and are not authenticated assessment records.

Authored dialogue is the default and needs no credentials. Optional Gemini or
local Ollama assistance maps questions to reviewed topics; all factual responses
remain authored from the scenario. Enable the optional chat interface with
`FIELDNOTES_CHAT_UI=shinychat`. See [deployment](docs/deployment.md) for provider
configuration and limits. A fresh browser connection starts a new run; nothing is
saved on the hosting server between sessions.

## Verify

```sh
Rscript tests/run.R
Rscript scripts/review-seed.R
Rscript scripts/review-scenarios.R
```

Tests cover data isolation, classification, missingness, task timing, preservation,
contact caps, action conditions, frozen evidence, workbook output, pilot teaching
signal, and the complete Shiny server workflow. See [the pilot guide](docs/pilot-guide.md)
for facilitation and limitations and [scenario review](docs/scenario-review.md) for
complete-follow-up reference results and the confounding comparison. This is a training prototype, not a
validated assessment instrument.

## Structure

`app.R` loads the UI/server and pure engine in `R/`. Instructor configuration lives
in `scenarios/*/scenario.yaml`. Learner support is in `starter/`;
test and implementation notes are in `tests/` and `docs/`. The Excel workbook is
generated from each learner's frozen snapshot, so its rows match their collection.

The YAML is trusted local instructor configuration. Character prose and the
supported case templates still live in R; adding a scenario requires review.
