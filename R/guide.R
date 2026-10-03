# Each checkpoint has a fixed place in the learning journey. Optional tools do
# not change the current step; revisiting never changes the simulated clock.
guide_steps <- function() {
  data.frame(
    page = c("briefing", "decision", "interview", "decision", "define", "collect", "analysis", "decision", "recommend", "debrief"),
    label = c("Welcome", "Your first thoughts", "Talk to people", "Review your interviews", "Define a case", "Collect histories", "Explore the data", "Weigh the evidence", "Recommend an action", "Reflect and retry"),
    checkpoint = c(NA, "After initial assessment", NA, "After interviews", NA, NA, NA, "After analysis", NA, NA))
}

checkpoint_fields <- function() c("suspect", "confidence", "supporting", "against", "change", "action_now")


validate_guide_restore <- function(g, count = nrow(guide_steps())) {
  if (is.null(g)) return(list(step = 1L, furthest = 1L, drafts = list()))
  if (!is.list(g)) stop("Invalid saved navigation.")
  index <- function(x, default) {
    if (is.null(x)) return(default)
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != floor(x) || x < 1 || x > count) stop("Invalid saved step.")
    as.integer(x)
  }
  text_fields <- function(x) {
    if (is.null(x)) return(list())
    if (!is.list(x) || is.null(names(x)) || any(!names(x) %in% checkpoint_fields())) stop("Invalid saved fields.")
    for (value in x) if (!is.null(value) && (!is.character(value) || length(value) != 1L || is.na(value) || nchar(value) > 20000)) stop("Invalid saved draft.")
    x
  }
  g$step <- index(g$step, 1L)
  g$furthest <- index(g$furthest, g$step)
  if (g$furthest < g$step) stop("Invalid navigation order.")
  g$current_fields <- text_fields(g$current_fields)
  if (is.null(g$drafts)) g$drafts <- list()
  if (!is.list(g$drafts) || (length(g$drafts) && (is.null(names(g$drafts)) || any(!names(g$drafts) %in% as.character(seq_len(count)))))) stop("Invalid saved drafts.")
  g$drafts <- lapply(g$drafts, text_fields)
  if (!is.null(g$recommendation_draft) && (!is.character(g$recommendation_draft) || length(g$recommendation_draft) != 1L || is.na(g$recommendation_draft) || nchar(g$recommendation_draft) > 20000)) stop("Invalid recommendation draft.")
  g
}
