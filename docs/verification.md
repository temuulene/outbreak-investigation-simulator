# Verification — 2026-10-01

## Guided UX revision

- Replaced the three-column dashboard with a single guided activity and optional
  notebook, task/action, and session disclosures. The opening screen has one CTA.
- All 96 expectations pass after the revision. Server tests exercise guided
  checkpoints in order, direct guest-list retrieval, waiting for team results,
  analysis, recommendation, retry, and reveal. Invalid checkpoint submission stays
  on the current step. Back/revisit preserves drafts without moving game time.
- Browser checked Start, all three initial reasoning prompts, automatic progression
  to interviews, restored answers when returning, and exclusive optional tools.
  The corrected navigation handler focuses the main landmark. A 390 px viewport
  check found no horizontal overflow and retained a usable single-column form.
- The redesigned app is previewed on port 3875 to leave the existing port 3874
  session untouched. `Rscript run.R` continues to use the documented default port.
- These checks verify implementation, not a measured improvement in learner
  engagement. The first learner pilot is still outstanding.

## Initial build

- `Rscript tests/run.R`: 72 passing expectations across the engine and Shiny
  session tests; no test warnings or failures in that run.
- `Rscript scripts/review-seed.R`: generated the reference-plan seed review.
  The simulated vehicle retains the largest observed association (RR 5.59).
  Collection is evaluated at the time the team returns; a later-onset guest can
  still be symptom-free then. This is intentional, not a changed hidden dataset.
- Actual browser: opened briefing, interviewed Pat, received the guest list,
  sent the team, viewed 60 records, froze a snapshot, and downloaded an Excel
  workbook. Inspected the responsive layout at 390 px and normal panel width.
- Added fixed links to evidence and actions at narrower widths. Fresh final
  session renders the briefing and Monday 09:00 clock.
- Workbook generation tests inspect its three sheets. Automated server tests
  cover three checkpoints, recommendation, debrief rendering, retry, and reveal.
- `renv.lock` records installed runtime and testing dependencies. A clean-machine
  restore was not performed. The GitHub Actions workflow is provided but has not
  run remotely; this directory started as an empty, uncommitted repository.

The R executable used locally was R 4.6.1. The host's inherited `C.UTF-8` locale
is not recognised by Windows R; tests ran with `LC_ALL=English_United States.utf8`.
`run.R` corrects the character locale if needed. App source is explicitly UTF-8,
and the Sass disk cache is disabled to avoid relying on a writable global cache.

No real pilot participants have tested the app yet. Educational effectiveness,
facilitator scoring reliability, a full accessibility audit, and live LLM safety
remain unverified. No deployment or outbound API integration was performed.
