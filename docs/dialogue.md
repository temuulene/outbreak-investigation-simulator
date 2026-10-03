# Optional assisted interviews

Scripted interviews are the default and need no credentials. Explicit topic buttons
always use the authored engine. Free-text questions can optionally use a configured
provider to recognize topics and choose a short connective from an approved list.

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
credentials temporarily. Live Ollama, hosted integration, an unrelated live
question, a forced live-service failure, and two simultaneous assisted browser
sessions remain separate deployment checks. Promotional credit deduction must be
confirmed in the billing account; a successful API reply does not establish it.
