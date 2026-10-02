# Fieldnotes · Outbreak investigation lab

A working R Shiny pilot based on the **Outbreak Simulator Design Sketch**.
Investigate a fictional community potluck through interviews, case definition,
targeted collection, analysis, proportionate action, and a structured debrief.

The interface guides learners through one activity at a time. Evidence, delegated
tasks, and protective actions stay a click away. Short checkpoint prompts preserve
drafts when going back; advanced analysis and settings are optional disclosures.

## Run locally

Requires R 4.3 or later. In this project directory:

```r
source("setup.R") # first-time dependency restore using renv
source("run.R")
```

Or use `Rscript run.R`, then visit <http://127.0.0.1:3874>.
For an existing R installation with dependencies already available,
`shiny::runApp()` also works. No API key is required. The server binds only to
localhost. This repository does not publish or deploy the app.

## Included

- Seeded, separate truth / reported / collected data and six scripted contacts.
- Free-text topic detection, explicit topic fallback, and independent histories.
- Preservation deadline, queued results, late reports, and contemporaneous actions.
- Structured case definitions with live counts and revision reasons.
- Questionnaire coverage preview; unasked fields remain missing.
- Frozen CSV and Excel exports, starter R script, RR/attack-rate checks, and curve.
- Three reasoning checkpoints, five-domain review, optional truth reveal, and retry.
- JSON session export containing learner-visible evidence, not hidden guest truth.

This implements the sketch's scripted pilot milestone. LLM dialogue remains a
post-pilot step. There is no cloud persistence: refreshing or closing loses the
session, so export the record first. A fresh browser connection starts a new run.

## Verify

```sh
Rscript tests/run.R
Rscript scripts/review-seed.R
```

Tests cover data isolation, classification, missingness, task timing, preservation,
contact caps, action conditions, frozen evidence, workbook output, pilot teaching
signal, and the complete Shiny server workflow. See [the pilot guide](docs/pilot-guide.md)
for facilitation and limitations and [seed review](docs/seed-review.md) for the
learner-visible reference-plan results. This is a training prototype, not a
validated assessment instrument.

## Structure

`app.R` loads the UI/server and pure engine in `R/`. Instructor configuration lives
in `scenarios/potluck-01/scenario.yaml`. Learner support is in `starter/`;
test and implementation notes are in `tests/` and `docs/`. The Excel workbook is
generated from each learner's frozen snapshot, so its rows match their collection.

The YAML is trusted local instructor configuration. Character prose and the
supported case templates still live in R; adding a scenario requires review.
