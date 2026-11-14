# Scenario engine and review

The welcome settings select either potluck exercise. A supplied seed replays its
fictional records exactly; random selection tries up to 100 seeds and rejects
weak or unsuitable teaching signals. The reference review uses complete reported
follow-up at 72 game hours. Earlier, selective, or incomplete collection may produce
different associations. See [the generated scenario review](scenario-review.md).

The intermediate scenario generates correlated chicken-salad and coleslaw
exposures. Its screening requires an apparent crude coleslaw association that
attenuates after stratifying by chicken salad, and a vehicle association that
persists within coleslaw strata. Learners choose both foods themselves. Missing
stratifier values are excluded and counted; inadequate denominators remain
not estimable. The comparison reports the crude relative risk, each stratum, and
a Mantel-Haenszel pooled relative risk with a Greenland-Robins interval.

Foods appear in `menu_order` everywhere: questionnaires, tables, downloads, and
selectors. Keep the vehicle out of first place to avoid anchoring. A case
definition limited to RSVP guests defines the cohort; other guests are
classified `Excluded` and leave both denominators. The interview roster holds
the first guests who had reported illness at the start plus well guests, and
does not change as later reports arrive.

Scheduled report, media, and evidence events come from YAML and run in time order
with queued tasks. A media response stores the learner's wording and a copy of
the cited evidence available then. It does not send a real public statement.
Repeated questionnaires add or update only the requested fields and preserve
earlier frozen analysis snapshots.

The five-domain profile displays observable process indicators, written reasoning,
action conditions, and evidence references. Each rule is available for review.
Facilitator judgments include a rationale in the audit log. Automated calculation
accuracy, instructor judgment of reasoning, and the optional truth reveal remain
separate. Nuanced causal reasoning receives instructor review; these indicators
are not a validated educational score.

Resume files use a versioned typed JSON format. The private scenario and truth and
reported tables are reconstructed from the reviewed local scenario and seed.
Only learner state is saved. Imports are limited to 5 MB and checked for allowed
types, shapes, identifiers, references, and chronology before replacing the active
session. They never evaluate R code. The latest frozen snapshot is restored.
Files remain editable local study records, so they do not establish authenticated
grades or prove who wrote the answers. A changed scenario file may require a new
session rather than reproduce an older version; retain the release used for a pilot.
