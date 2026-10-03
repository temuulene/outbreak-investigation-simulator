# Natural assisted interviews

The learner should be able to ask naturally, follow up without repeating the subject,
and receive varied replies that respect each contact's knowledge. Existing topic
selection and preset openings do not provide that experience.

Use a three-stage asynchronous pipeline: scoped topic classification, grounded
rewriting, and a fresh semantic review. Supply at most four recent exchanges with
the selected contact; send only the current canonical answer to the writing step.
An explicit topic skips classification but remains eligible for natural wording.
Keep authored interviews available when assistance is disabled, limited, unavailable,
late or rejected. Use concise conversational language by default.

The engine is authoritative for evidence, collections, timing and scenario facts.
Accepted prose is a chat presentation change, with its canonical answer retained for
audit and resume. Deterministic checks protect numeric tokens, reply shape and size;
the separate review checks factual support, completeness, uncertainty and role.
Semantic review remains probabilistic. No answer key, other character records or
full investigation state are sent to the provider.

A single deadline and revision token cover all stages. Keep the existing 30-turn
session budget, now costing at most three provider requests per assisted turn.
Explain automatic assistance and fallback in the UI. Preserve current controls,
draft edits, stale-result rejection and latest-answer scrolling.

Verify with tests written before behavior changes, mocked errors and malicious
drafts, same-contact context isolation, unchanged engine state, save/resume,
live fictional Gemini conversations and a public browser check after deployment.
