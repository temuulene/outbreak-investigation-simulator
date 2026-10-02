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

Deferred per the sketch: live LLM dialogue and shinychat, random seeds, potluck-02
confounding, media simulation, public deployment, and automated reasoning scores.
