# Facilitator pilot guide

This is a fictional training exercise with scripted dialogue by default.
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
   Describe the cases first: symptoms, incubation, and the onset curve. Then
   check attack rates and RR. Interpret uncertainty, incomplete data, and recall.
7. When a media request arrives, draft a short response distinguishing known
   findings, uncertainty, and actions. Cite the notebook entries you used.
8. Save the analysis checkpoint and recommendation. Review the profile and
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

## Intermediate exercise

Choose the intermediate scenario in the optional welcome settings. Compare crude
food associations, then choose two foods in the optional stratified analysis.
The app reports the crude relative risk, each stratum, and a Mantel-Haenszel
adjusted relative risk with an approximate interval. Discuss whether an
association persists within strata, why the adjusted value moves, the size of
each comparison group, missing exposure histories, and the limits of
observational evidence.
Do not announce the simulated vehicle before learners form their own explanation.

## Advanced exercise

Choose the advanced scenario. The agent differs: learners should notice the
shorter incubation and the near absence of fever before they look at foods, and
say which agents fit. A community post blames one food early on. Two foods then
show strong crude associations. Ask learners to weigh timing of the rumour,
stratified results, the cook's account, and laboratory findings. The reveal
shows the rumoured food's true and recalled tables.

Save a resumable JSON file before closing. It restores a local session; it is not
an authenticated assessment submission. A facilitator can add a reasoned review
judgment, retained alongside the learner's evidence and decisions.

Use [the observation sheet](pilot-observation-template.md) to record usability
findings. Randomized runs are screened against a reference collection plan;
that screening does not guarantee a useful result for every learner's choices.

## What the simulated data include

- Unrelated mild illness in a few guests the outbreak spared, some of it before
  the meal. A broad symptom criterion gains sensitivity and loses specificity;
  the onset window removes illness that could not come from the meal. The
  [scenario review](scenario-review.md) lists sensitivity and specificity for
  each criterion.
- Not every case phones in. A plan limited to guests who reported misses the
  rest and has no comparison group.
- Some guests do not answer the team. The result names how many, and they are
  absent from the line list. Ask learners who is missing and how that could bias
  the attack rates.
- Recall error on every food. In the advanced scenario a rumour makes ill guests
  over-report one food, which creates an association the reveal then takes apart.
- Stool culture misses some cases, and a food test can come back negative.
  Requiring a positive stool result leaves culture-negative cases as non-cases.
- A guest interviewed before Pat gives the menu names only what they remember.
  Foods they do not mention stay unknown, not "no".
- Onset and report are date-times in the exports. Learners work out incubation
  from the meal time. The on-screen case description does this for them; hide it
  if you want them to do it by hand.

## Known pilot boundaries

- Three point-source scenarios, fixed or screened random seeds. No secondary transmission.
- Scripted topic detection by default. Optional provider-assisted topic selection
  requires server configuration; factual replies remain authored by the engine.
- Six available contacts and per-topic time cost. A revised delegated questionnaire
  can add missing fields after an earlier collection returns. Individual interviews
  also add fields. Case definitions can be revised throughout.
- Lab sampling covers up to three guests who had reported illness when samples
  were requested. Unsampled guests remain unknown. The leftover result reports a
  match only when stool isolates exist.
- An RSVP-only definition removes walk-ins from the cohort. They show as
  Excluded and count in neither denominator.
- Response and reporting are fixed per guest. A second visit does not reach a
  guest who did not answer the first.
- Action feedback checks one recorded condition per action: two or more illness
  reports, a known preparation failure, leftovers known to have gone home, or
  continuing exposure at the hall (never true in these scenarios). It does not
  judge the written rationale.
- The calculation check accepts attack rates within 0.015 and a relative risk
  within 0.05 of the exact value, or of the ratio of the learner's own correctly
  rounded attack rates. It then shows the unrounded value.
- No real guest records or learner authentication. Refresh creates a fresh session;
  upload a previously saved resume file to continue. Hosting is configured separately.
- Scenario YAML configures the agent, foods, risks, the cook's account, events,
  tasks, action rules, and realism settings. The cast, the potluck frame, and the
  case-definition templates are fixed in the app.

## Optional dialogue adapter

Keep the scripted path as the baseline. The ellmer adapter classifies topics into
a constrained schema and selects an approved introductory phrase. It receives
the current question and allowed topic vocabulary; the engine supplies all facts.
Invalid output, timeout, and provider errors use authored fallback responses.
See [dialogue configuration and limits](dialogue.md). Validate with the first
pilot's actual question corpus before enabling it. Keys belong on the server.
