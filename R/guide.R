# Each checkpoint has a fixed place in the learning journey. Optional tools do
# not change the current step; revisiting never changes the simulated clock.
guide_steps <- function() {
  data.frame(
    page = c("briefing", "decision", "interview", "decision", "define", "collect", "analysis", "decision", "recommend", "debrief"),
    label = c("Welcome", "Your first thoughts", "Talk to people", "Review your interviews", "Define a case", "Collect histories", "Explore the data", "Weigh the evidence", "Recommend an action", "Reflect and retry"),
    checkpoint = c(NA, "After initial assessment", NA, "After interviews", NA, NA, NA, "After analysis", NA, NA))
}

checkpoint_fields <- function() c("suspect", "confidence", "supporting", "against", "change", "action_now")
