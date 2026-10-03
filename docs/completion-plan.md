# Simulator implementation status

Maintain the guided interface and the separation of simulated truth, reported facts,
and collected evidence. Preserve existing learner decisions and transparent uncertainty.

- [x] Configurable scenario selection and bounded random-seed teaching-signal checks.
- [x] Intermediate potluck scenario with correlated exposures and stratified analysis.
- [x] Media events, evidence-based responses, and logged communication decisions.
- [x] Grounded optional ellmer dialogue with structured topics, reply validation,
      per-session request limits, and authored fallback; shinychat integration.
- [x] Transparent formative reasoning feedback with evidence references and instructor review.
- [x] Safe session save/resume, repeat collection, and generalised scenario configuration.
- [x] Deployment packaging, provider configuration, secret exclusions, and local launch checks.
- [x] Full regression tests, browser checks, documentation, dependency lock, and GitHub CI.
- [x] Record external prerequisites and pilot observation workflow.

## External release checks

- [ ] Deploy to a configured host and verify the URL from another client.
- [ ] Build and run the container with a running Docker daemon.
- [ ] Verify live-provider success, outage fallback, and independent sessions with
      configured Gemini credentials or a local Ollama model.
- [ ] Manually confirm browser upload/resume; server import/restore tests pass.
- [ ] Observe real pilot participants and record usability and learning findings.

Local software verification is recorded in [verification.md](verification.md).
Public deployment requires a hosting destination and its account credentials;
live-provider verification additionally requires a configured Gemini key or a
running Ollama service with the selected model. Container execution needs a running
Docker daemon. See [deployment.md](deployment.md) for the reviewed manifests and
commands. No hosting account or provider service is configured by the repository.

Real pilot participation and educational-effectiveness validation require human
testers. Provide a pilot observation workflow; do not label those activities completed
by software tests.
