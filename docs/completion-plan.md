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

- [x] Publish to Posit Connect Cloud and verify the public URL in a fresh browser session.
- [x] Verify a hosted assisted reply and a separate session's untouched welcome screen.
- [ ] Build and run the container with a running Docker daemon.
- [ ] Verify forced live-provider outage fallback and two simultaneous assisted
      investigations; live Ollama remains optional and unverified.
- [ ] Complete the hosted save/resume and export walkthrough.
- [ ] Manually confirm browser upload/resume; server import/restore tests pass.
- [ ] Observe real pilot participants and record usability and learning findings.

Software and hosted smoke verification are recorded in [verification.md](verification.md).
The [public pilot](https://temuulen-outbreak-investigation-simulator.share.connect.posit.cloud/)
is hosted on Posit Connect Cloud. Hosting credentials and provider keys remain
private and outside the repository. Container execution needs a running Docker
daemon. See [deployment.md](deployment.md) for manifests, publishing settings,
and update instructions.

Real pilot participation and educational-effectiveness validation require human
testers. Provide a pilot observation workflow; do not label those activities completed
by software tests.
