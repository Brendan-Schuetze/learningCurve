test <- function() {
  x <- generateCurve(engine_type = "logistic", engine_pars = list(slope = 1, floor = 0, shift = 0),
                     limiter_type = "simplegoal", limiter_pars = list(goal = 0.85),
                     time_end = 10)
  print(x)
}