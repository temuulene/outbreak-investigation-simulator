# Scenario reference review

Instructor reference; contains scenario spoilers. Each authored seed is evaluated
at 72 hours of game time with every attendee and all symptom, onset, and food fields.
This complete follow-up reference differs from an earlier or incomplete learner collection:
learners reach only guests who respond, and see only cases that have phoned in or been interviewed.

## Trouble after the community potluck

Introductory | Salmonella Enteritidis | Seed: 4172 | Attendees: 60 | Teaching signal accepted: TRUE

Simulated cases: 22. Unrelated illness: 3. Cases who phone in: 19. Guests who answer the team: 58. Cases a stool culture would detect: 19. Leftover test detects the vehicle: TRUE.

With perfect recall the vehicle table is 21 / 37 against 1 / 23 (RR 13.05).
Recall error and unrelated illness account for the gap from the crude RR below.

| Exposure | Exposed cases / total | Unexposed cases / total | Crude RR |
| --- | ---: | ---: | ---: |
| cake | 16 / 38 | 6 / 22 | 1.54 |
| punch | 14 / 37 | 8 / 23 | 1.09 |
| fried_rice | 12 / 27 | 10 / 33 | 1.47 |
| coleslaw | 7 / 17 | 15 / 43 | 1.18 |
| chicken_salad | 20 / 37 | 2 / 23 | 6.22 |

Clinical criteria against simulated truth, any onset time:

| Symptoms criterion | Sensitivity | Specificity |
| --- | ---: | ---: |
| diarrhea | 0.91 | 0.97 |
| standard | 0.95 | 0.97 |
| any | 1.00 | 0.92 |

## Two foods, one shared story

Intermediate | Salmonella Enteritidis | Seed: 4172 | Attendees: 180 | Teaching signal accepted: TRUE

Simulated cases: 65. Unrelated illness: 12. Cases who phone in: 51. Guests who answer the team: 156. Cases a stool culture would detect: 51. Leftover test detects the vehicle: TRUE.

With perfect recall the vehicle table is 59 / 98 against 6 / 82 (RR 8.23).
Recall error and unrelated illness account for the gap from the crude RR below.

| Exposure | Exposed cases / total | Unexposed cases / total | Crude RR |
| --- | ---: | ---: | ---: |
| cake | 54 / 126 | 15 / 54 | 1.54 |
| punch | 42 / 109 | 27 / 71 | 1.01 |
| fried_rice | 34 / 100 | 35 / 80 | 0.78 |
| coleslaw | 43 / 89 | 26 / 91 | 1.69 |
| chicken_salad | 61 / 99 | 8 / 81 | 6.24 |

Clinical criteria against simulated truth, any onset time:

| Symptoms criterion | Sensitivity | Specificity |
| --- | ---: | ---: |
| diarrhea | 0.95 | 0.97 |
| standard | 1.00 | 0.94 |
| any | 1.00 | 0.90 |

Association for coleslaw within levels of chicken_salad :

| Stratum | Exposed cases / total | Unexposed cases / total | RR |
| --- | ---: | ---: | ---: |
| chicken_salad unexposed | 1 / 17 | 7 / 64 | 0.54 |
| chicken_salad exposed | 42 / 72 | 19 / 27 | 0.83 |

coleslaw adjusted for chicken_salad: crude RR 1.69; Mantel-Haenszel RR 0.80 (approx. 95% CI 0.58 to 1.11).

chicken_salad adjusted for coleslaw: crude RR 6.24; Mantel-Haenszel RR 7.41 (approx. 95% CI 3.35 to 16.40).

## A faster illness, and a rumour

Advanced | Clostridium perfringens | Seed: 4172 | Attendees: 120 | Teaching signal accepted: TRUE

Simulated cases: 36. Unrelated illness: 7. Cases who phone in: 21. Guests who answer the team: 99. Cases a stool culture would detect: 20. Leftover test detects the vehicle: TRUE.

With perfect recall the vehicle table is 34 / 70 against 2 / 50 (RR 12.14).
Recall error and unrelated illness account for the gap from the crude RR below.

| Exposure | Exposed cases / total | Unexposed cases / total | Crude RR |
| --- | ---: | ---: | ---: |
| apple_pie | 16 / 60 | 24 / 60 | 0.67 |
| lemonade | 29 / 74 | 11 / 46 | 1.64 |
| bread_rolls | 20 / 62 | 20 / 58 | 0.94 |
| garden_salad | 32 / 63 | 8 / 57 | 3.62 |
| steamed_rice | 26 / 61 | 14 / 59 | 1.80 |
| beef_stew | 34 / 66 | 6 / 54 | 4.64 |

Rumoured food garden_salad: truly 20 / 54 against 16 / 66 (RR 1.53); as recalled 30 / 63 against 6 / 57 (RR 4.52). The association comes from differential recall.

Clinical criteria against simulated truth, any onset time:

| Symptoms criterion | Sensitivity | Specificity |
| --- | ---: | ---: |
| diarrhea | 1.00 | 0.93 |
| standard | 1.00 | 0.93 |
| any | 1.00 | 0.92 |

Random selection checks at most 100 seeds. It requires adequate comparison
groups, a vehicle RR of at least 3 and the largest crude association, at least two
initial ill contacts, and the scheduled number of late reporters. Scenarios with a
stratifier also require an apparent association for that food that attenuates within
vehicle strata, while the vehicle association remains within its strata.

Selection deliberately favours a readable lesson. It is not a sample for estimating
real-world outbreak frequencies or effects. Small cells, non-response, and recall errors still
limit inference; a food association and a preparation failure do not alone establish cause.
