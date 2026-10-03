# Optional assisted interviews

Scripted interviews are the default and need no credentials. Explicit topic buttons
always use the authored engine. Free-text questions can optionally use a configured
provider to recognize topics and choose a short connective from an approved list.

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

Set `FIELDNOTES_DIALOGUE_PROVIDER` to `gemini` or `ollama` and set
`FIELDNOTES_DIALOGUE_MODEL` to an explicitly chosen, available model. Gemini uses
ellmer's `GOOGLE_API_KEY` environment variable. Ollama uses `OLLAMA_BASE_URL`
(default `http://127.0.0.1:11434`) and its OpenAI-compatible `/v1` endpoint. Run and
install the chosen Ollama model separately. Keep credentials in deployment secrets
or an ignored local `.Renviron`; never include them in saved investigations.

The implementation uses ellmer's `chat_structured_async()` with an enum schema.
Each request creates a fresh provider chat: there is no shared conversation history,
no cross-character memory, and no cross-session chat object. Only the learner's
question, character role/topic vocabulary, and classification instructions go to
the provider. State, reported records, evidence, and the answer key stay in R.
Learners should enter fictional investigation questions only; hosted providers
receive those questions.

The response must contain exactly `topics` and `intro`. Topics are restricted to
the selected character's permitted domains. Intro values select authored connective
text; arbitrary provider prose, HTML, quantities, diagnoses, causes and extra fields
are rejected. All factual sentences and numeric values are inserted by the existing
interview engine from reported information available at the current simulated time.
This is structural fact validation: the provider cannot supply a changed number or
unsupported factual sentence. Temperament is associated with character identity,
never illness, exposure, or the hidden outcome. Unknown or malformed responses use
the authored topic matcher. This constrained design intentionally does not produce
unrestricted generative dialogue.

Each Shiny session owns a `dialogue_session()` environment. Defaults permit at most
30 provider attempts, at least two seconds between attempts, a 1,200-character
question, and one pending request. Explicit topic and scripted replies spend no
provider allowance. A 20-second application deadline resolves to an authored reply;
late results are ignored. The underlying HTTP request may still finish at the
provider and incur usage: this deadline is not transport cancellation. Provider
failures and diagnostics are never displayed or stored in learner state.

The UI must call `dialogue_cancel()` when replacing an investigation or disconnecting,
and accept a result only when `dialogue_result_current()` is true and the captured
state revision still matches. Apply `dialogue_apply()` once to that valid state.
This avoids replacing newer learner actions with a stale asynchronous snapshot.
Request budgets should persist across resets within the same browser session.

Mocked tests cover malformed and hostile output, exact authored evidence, session
isolation, request/rate/input limits, errors, cancellation, and timeout fallback.
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
