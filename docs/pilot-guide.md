# Facilitator pilot guide

This is a fictional, scripted training exercise, not a clinical decision system.
Plan roughly 80 minutes: briefing 5, assessment 10, interviews and planning 20,
analysis 20, recommendation 10, debrief and retry 15. These are proposed timings,
not findings from a user test.

## Before the session

Start a new browser session. Give pairs permission to make mistakes, ask for
hints, and switch interviewer/evidence-keeper roles. Explain that the clock is
game time, starting Monday at 09:00; it does not track real elapsed time.
The meal occurred Saturday at 18:00 (39 hours before the clock starts).

## A reference route (not a required answer)

1. Select Start investigating and answer the three short initial prompts.
   Consider low-burden guest advice through Tasks & actions.
2. Interview Pat about the menu, list completeness, and leftovers. Request
   the RSVP list and preserve leftovers before Tuesday 09:00.
3. Interview one ill guest, one well guest, and Lou. An environmental health
   review provides an alternative route to the cooling evidence after 24 hours.
4. Continue from interviews to save the interview checkpoint. Define cases and preview a plan including
   well guests, symptoms, onset, and every food. Send the team.
5. Delegate appropriate lab/environmental tasks and advance to due results.
6. Freeze a line-list snapshot; download CSV plus R script or Excel workbook.
   Check attack rates and RR. Interpret uncertainty, incomplete data, and recall.
7. Save the analysis checkpoint and recommendation. Review the profile and
   contemporaneous evidence; retry one decision. Export the session record.

## Review rubric

| Domain | Needs discussion | Defensible reasoning |
| --- | --- | --- |
| Planning | Ill-only sampling, unasked alternative foods | Comparison guests and broad exposure ascertainment; limitations explained |
| Classification | Implicit criteria or unexplained revisions | Reproducible person, place, time, clinical criteria; unknowns recognised |
| Analysis | Wrong denominators or unknowns treated as well | Correct observed denominators, missingness and uncertainty acknowledged |
| Synthesis | One interview treated as proof | Epidemiological, preparation, and laboratory evidence distinguished |
| Action | Wait for certainty or assume ongoing spread | Proportionate action justified from what was known then |

Calculation checks are automated. Nuanced reasoning requires instructor review.
Agreement with simulated truth is not a pass/fail result. The app gives rule-based
prompts, not a validated competence score. The guide places each checkpoint at
its intended stage. Revisited checkpoints are
logged as new entries, allowing facilitators to identify later revisions.

## Observe, do not assume

Record whether learners anchor on the first food, omit well guests, miss walk-ins,
confuse onset/report time, overlook preservation, or overreact. Record confusing
questions and classifier errors; the topic menu is always available. Ask for a
short transfer example and a five-minute retry. Two or three testers can expose
usability problems, but cannot establish educational effectiveness.

## Known pilot boundaries

- One reviewed seed and one point-source scenario. No secondary transmission.
- Scripted topic detection; no live LLM, provider credential, or external request.
- Six available contacts, per-topic time cost, a single delegated questionnaire.
  A revised questionnaire requires a new session; individual interviews can add
  fields after delegation. Case definitions can be revised throughout.
- Lab sampling is simplified to up to three already reported ill guests; only
  those records receive positive stool status. Unsampled guests remain unknown.
- No patient identifiers, real guest records, authentication, cloud hosting, or
  resumable sessions. Export before closing; refresh creates a fresh session.
- Scenario YAML configures seed, foods, risks, symptoms, tasks, and action rules.
  Character prose and the supported case-definition templates are authored in R;
  this is not yet a general scenario authoring platform.

## Future dialogue adapter

Keep the scripted path as the baseline. A later ellmer adapter should classify
topics into a constrained schema, receive only allowed facts for that character,
preserve numeric/time strings, reject unsupported pathogen knowledge, and fall
back to the authored answer after two failed checks. Never give an LLM the state
object or truth table. Validate with the first pilot's actual question corpus
before enabling it. Do not collect API keys in the learner UI.
