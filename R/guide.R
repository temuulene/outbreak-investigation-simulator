# Each checkpoint has a fixed place in the learning journey. Optional tools do
# not change the current step; revisiting never changes the simulated clock.
# Code refers to steps by id, so a step can be added without renumbering.
guide_steps <- function() {
  data.frame(
    id = c(
      "welcome", "first_thoughts", "interview", "interview_review", "define", "collect", "analysis",
      "weigh", "recommend", "debrief"
    ),
    page = c(
      "briefing", "decision", "interview", "decision", "define", "collect", "analysis", "decision",
      "recommend", "debrief"
    ),
    label = c(
      "Welcome", "Your first thoughts", "Talk to people", "Review your interviews", "Define a case",
      "Collect histories", "Explore the data", "Weigh the evidence", "Recommend an action", "Reflect and retry"
    ),
    checkpoint = c(NA, "After initial assessment", NA, "After interviews", NA, NA, NA, "After analysis", NA, NA),
    stringsAsFactors = FALSE
  )
}

step_index <- function(id, steps = guide_steps()) {
  index <- match(id, steps$id)
  if (is.na(index)) stop("Unknown guide step: ", id)
  index
}

checkpoint_fields <- function() c("suspect", "confidence", "supporting", "against", "change", "action_now")

# One check for saved navigation, used when a file is read and again before the
# server applies it. Returns the guide with defaults filled in.
validate_guide <- function(g, steps = guide_steps()) {
  if (is.null(g)) return(list(step = 1L, furthest = 1L, drafts = list()))
  allowed <- c("step", "furthest", "drafts", "current_fields", "recommendation_draft")
  if (!is.list(g) || is.data.frame(g) || any(!names(g) %in% allowed)) stop("Invalid saved guide.")
  count <- nrow(steps)
  index <- function(x, default) {
    if (is.null(x)) return(default)
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != floor(x) || x < 1 || x > count) {
      stop("Invalid saved guide step.")
    }
    as.integer(x)
  }
  text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nchar(x) <= max_text_length()
  fields <- function(x) {
    if (is.null(x)) return(list())
    named <- is.list(x) && (!length(x) || (!is.null(names(x)) && all(names(x) %in% checkpoint_fields())))
    if (!named || !all(vapply(x, function(value) is.null(value) || text(value), logical(1)))) {
      stop("Invalid saved guide draft.")
    }
    x
  }
  g$step <- index(g$step, 1L)
  g$furthest <- index(g$furthest, g$step)
  if (g$furthest < g$step) stop("Invalid saved guide step order.")
  g$current_fields <- fields(g$current_fields)
  if (is.null(g$drafts)) g$drafts <- list()
  decisions <- as.character(which(steps$page == "decision"))
  keyed <- is.list(g$drafts) && (!length(g$drafts) || (!is.null(names(g$drafts)) && all(names(g$drafts) %in% decisions)))
  if (!keyed) stop("Invalid saved guide draft.")
  g$drafts <- lapply(g$drafts, fields)
  if (!is.null(g$recommendation_draft) && !text(g$recommendation_draft)) stop("Invalid saved guide draft.")
  g
}
