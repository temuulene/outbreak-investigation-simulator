# Implementation decisions — 2026-10-01

The supplied design sketch is the product specification. Its staged build order
supports implementing the scripted pilot before adding generative dialogue.
The working title is Fieldnotes. The app uses R Shiny and bslib with a quiet,
paper-and-forest-green investigation desk. It is local-only by default.

Pure R functions implement generation, collection, classification, scheduling,
interviews, analysis, and review. Shiny owns a separate reactive state per session.
The private truth/reported tables are never included in learner exports.
Case definition edits do not alter frozen analysis snapshots. Checkpoints and
actions retain the evidence available at the time. The queue runs chronologically
when time advances; reading and opening hints never advance it.

The interface uses a single guided column on desktop and phones. A short welcome
leads into nine steps: initial reasoning, interviews, interview reasoning, case
definition, collection, analysis, analysis reasoning, recommendation, and retry.
Each reasoning checkpoint has three short prompts. Progress represents the current
step, not competence. Notebook, tasks/actions, and session tools are closed by
default; opening one closes the other tool panels. These remain available throughout.
The collection screen handles requesting/receiving the list and waiting for the
team directly. Navigation does not move game time. Draft checkpoint answers are
preserved by stage and earlier visited steps remain accessible. Numeric analysis,
raw tables, definition settings, and debrief detail are progressively disclosed.
Keyboard focus moves to the main content after navigation, and motion respects
reduced-motion settings. Manual inspection is distinct from a full accessibility audit.

Dependency APIs checked against the official [Shiny download documentation](https://shiny.posit.co/r/reference/shiny/latest/downloadhandler.html)
and [bslib theme documentation](https://rstudio.github.io/bslib/reference/bs_theme.html).
All data are fictional; authored scenario probabilities were supplied in the sketch.

## Expansion — 2026-10-02

The optional welcome settings select either potluck scenario and a fixed or
screened random seed. The intermediate scenario correlates two food exposures;
stratified comparisons use collected records in a frozen snapshot. Scheduled media
requests and evidence-linked learner responses are retained in the audit record.
Repeated collection can add missing fields after an earlier questionnaire returns.

Optional ellmer assistance selects permitted topic identifiers and reviewed
connective text. The engine supplies all factual replies, including exact numbers
and onset times. This conservative adapter avoids free-form factual generation.
Requests have session, spacing, input-length, and deadline limits. Errors and
invalid output fall back to scripted replies; stale asynchronous replies cannot
overwrite changed investigation state. An optional shinychat widget uses the
same engine boundary and contact-specific histories.

Versioned JSON save/resume reconstructs private records locally from the scenario
and seed. Imports are bounded and validated before replacing current work; the
latest frozen snapshot is restored. Learner exports include media and facilitator
review records and exclude hidden guest tables.

The five-domain formative profile displays transparent process indicators and
contemporaneous reasoning/evidence. Calculation accuracy is checked automatically;
nuanced reasoning receives facilitator review with an auditable rationale. It does
not claim a validated automated competence score. A human pilot observation sheet
supports the testing milestone in the sketch.

Deployment packaging includes an explicit container copy inventory and a
shinyapps.io deployment script, plus a verified R 4.6.0 manifest for Connect Cloud.
The [public pilot](https://temuulen-outbreak-investigation-simulator.share.connect.posit.cloud/)
is hosted on Posit Connect Cloud. A hosted assisted interview and a separate fresh
session were checked on 2026-10-03; the remaining hosted release checks are listed
in [verification.md](verification.md).
