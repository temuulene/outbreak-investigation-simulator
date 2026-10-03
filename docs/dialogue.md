# Optional assisted interviews

Scripted interviews are the default and need no credentials. With Gemini or
Ollama configured, assistance runs automatically for written questions and explicit
topic choices. No separate creative mode or special prompt is needed.

Written questions first receive a structured topic selection, using up to four
recent exchanges with the same contact to interpret follow-ups. The engine then
constructs the canonical answer from the selected topics. A second provider request
rephrases that answer in concise, natural first-person language. A fresh provider
chat checks that the draft is supported, complete and in character before it is
shown. Explicit topic choices skip classification and use the same writing/review
steps. "Reply added · Gemini-assisted reply" means reviewed wording was accepted.
"Reply added · scenario answer" means authored wording was used instead.

Open questions such as "Tell me about the potluck" receive an authored event
overview from Pat, Lou, or a guest. Event timing is separate from illness onset.
The overview does not unlock the complete menu, walk-in contacts, preparation
findings, or individual health histories; ask focused follow-ups for those.
The scripted matcher recognizes common phrasings and whole words, including
"Who came?", "What did you serve?", "left over", and "throw up". It avoids
incidental matches such as "will" as illness or "a list of dishes" as a guest list.
Unmatched questions receive a character-specific example of a useful question.

Questions such as "Who brought which food?", "Who made the chicken salad?", and
"Who is Lou?" use a separate contributor topic. Pat explains that Lou brought
the chicken salad sandwiches and that the other contributors are not recorded;
the dialogue does not invent a complete contributor list. A menu-only answer
lists the dishes without singling out Lou. Contributor questions do not collect
guest food histories or reveal preparation methods unless those are also asked
for. The topic menu includes "Who brought the food" for each contact.

A selected topic supplements the written question. Both scripted and assisted
paths retain locally recognized topics, including when a valid provider response
omits them. Successful replies reset the topic to the written-question option and
clear the submitted text; rejected submissions keep the draft. A draft or topic
edited while a reply is pending, and another contact's controls, are preserved.
Topic recognition is bounded, so the topic menu remains available for wording
the matcher misses.

The standard conversation panel brings the latest answer into view after Shiny
has replaced its contents, including delayed assisted and fallback replies.
Reading earlier messages does not itself trigger scrolling. Very long answers
show their beginning so the remaining text can be read by scrolling.

Set `FIELDNOTES_DIALOGUE_PROVIDER` to `gemini` or `ollama` and set
`FIELDNOTES_DIALOGUE_MODEL` to an explicitly chosen, available model. Gemini uses
ellmer's `GOOGLE_API_KEY` environment variable. Ollama uses `OLLAMA_BASE_URL`
(default `http://127.0.0.1:11434`) and its OpenAI-compatible `/v1` endpoint. Run and
install the chosen Ollama model separately. Keep credentials in deployment secrets
or an ignored local `.Renviron`; never include them in saved investigations.

The implementation uses ellmer's `chat_structured_async()` with an enum schema.
Each stage creates a fresh provider chat; there is no shared provider chat object,
cross-character memory or cross-session memory. Classification receives the question,
role/topic vocabulary and bounded same-contact history. History includes only question,
displayed reply and topic IDs, truncated to 600 and 1,600 characters respectively.
Writing also receives the canonical answer for the current question; review receives
only that answer, the draft and contact identity. State, reported tables, other
contacts, evidence and the answer key stay in R. Only the current character's
requested facts, available at the current simulated time, reach the writing step.
Learners should enter fictional investigation questions only; hosted providers
receive those questions.

Classification must contain exactly `topics` and `intro`, restricted to the contact's
allowed identifiers. Writing must contain exactly `reply`; review must contain exactly
three true booleans: `supported`, `complete`, and `in_character`. Blank, oversized,
HTML/URL-containing and numerically changed drafts are rejected deterministically.
The fresh review checks names, food identities, roles, uncertainty, negations,
omissions and unsupported claims. This semantic review is probabilistic, rather than
the earlier structural guarantee that all prose was authored. It reduces factual
drift but cannot prove every paraphrase correct. The notebook evidence, event log,
collected records and clock always use the canonical engine answer; accepted chat
entries retain `authored_reply` for audit and save/resume. Apply-time validation also
requires that the canonical answer still matches the current engine answer.

The writer receives no facts beyond the current answer and cannot generate extra
scenario facts through tools. Prompts treat all questions and history as untrusted
data. Persona wording is based on contact role, never illness or the hidden outcome.
Malformed classification falls back to the local matcher. Rewriting/review failures
retain successfully recognized topics and use authored wording. Simple "tell me more"
follow-ups can also retain the previous topic during an outage.

Each Shiny session owns a `dialogue_session()` environment. Defaults permit at most
30 assisted turns, at least two seconds between turns, a 1,200-character
question, and one pending request. A written turn can make up to three provider
requests (classification, writing, review); an explicit topic uses at most two.
Thus the cap permits at most 90 requests per browser session. Scripted replies spend
no allowance. A single 20-second application deadline covers the whole pipeline
and resolves to an authored reply; late results are ignored. Review is not started
if writing finishes after cancellation or the deadline. The underlying HTTP request may still finish at the
provider and incur usage: this deadline is not transport cancellation. Provider
failures and diagnostics are never displayed or stored in learner state.

The Ask button shows "Preparing reply…" and prevents duplicate clicks while a reply
is pending; the learner can keep reading or editing their next question.
The UI must call `dialogue_cancel()` when replacing an investigation or disconnecting,
and accept a result only when `dialogue_result_current()` is true and the captured
state revision still matches. Apply `dialogue_apply()` once to that valid state.
This avoids replacing newer learner actions with a stale asynchronous snapshot.
Request budgets should persist across resets within the same browser session.

Mocked tests cover malformed and hostile output, exact authored evidence, session
isolation, bounded same-contact context, separate writer/reviewer payloads, natural
wording, explicit-topic assistance, save/resume, request/rate/input limits, errors,
cancellation and timeout fallback.
On 2026-10-03, a local `gemini-3.5-flash-lite` request and a browser interview
both recorded assisted replies with authored facts. ellmer enum arrays return R
factors; their labels are normalized before strict validation. Keys remain in an
ignored local environment file. The regression runner disables real providers and
credentials temporarily. On the same date, the
[public Connect Cloud pilot](https://temuulen-outbreak-investigation-simulator.share.connect.posit.cloud/)
returned an authored menu answer with `Reply added · assisted`. A second session
started at the untouched welcome screen. The public UI does not expose the
provider/model settings. Live Ollama, an unrelated live question, a forced
live-service failure, and two simultaneous assisted investigations remain
separate deployment checks. Promotional credit deduction must be
confirmed in the billing account; a successful API reply does not establish it.
