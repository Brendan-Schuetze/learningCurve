# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# TODO:
# Add in Self-regulated / goal-seeking behavior
# Need to think about how to best incorporate this into the library and where
#

library(psych)
library(ggplot2)

clamp <- function(x) {
  return(pmin(1, pmax(0, x)))
}

checkNames <- function(pars, names) {
  if(!all(names %in% names(pars))) {
    stop("Names of pars inputs do not match expected names for this engine")
  }
}

limiterSimpleGoal <- function(raw_output_perf, pars) {

  # TODO: Rename rawoutput after export?

  checkNames(pars = pars, names = c("goal"))

  goal <- as.numeric(pars["goal"])

  for(i in 1:length(raw_output_perf)) {

    # If we reach goal, stop studying
    if(raw_output_perf[i] >= goal) {

      raw_output_perf[i:length(raw_output_perf)] <- raw_output_perf[i]

      break
    }
  }

  return(raw_output_perf)

}

engineLinear <- function(t, pars) {

  # TODO: STILL NEED TO ADD SHIFT
  # TODO: Need to add bounds

  checkNames(pars = pars, names = c("slope", "floor", "shift"))

  slope <- as.numeric(pars["slope"])
  floor <- as.numeric(pars["floor"])
  shift <- as.numeric(pars["shift"])

  output_perf <- t * slope + floor

  # Clamp between 0 and 1
  output_perf <- clamp(output_perf)

  return(output_perf)
}

engineLogistic <- function(time_seq, pars) {

  checkNames(pars = pars, names = c("slope", "floor", "shift"))

  slope <- as.numeric(pars["slope"])
  floor <- as.numeric(pars["floor"])
  shift <- as.numeric(pars["shift"])

  output_perf <- psych::logistic(x = time_seq,
                                 a = slope,
                                 c = floor,
                                 d = shift)

  return(output_perf)
}

setEngine <- function(engine_type) {
  if(engine_type == "logistic") {
    return(engineLogistic)
  }
  if(engine_type == "linear") {
    return(engineLinear)
  }

  # Throw error if we can't set engine
  stop(paste("No engine of this name <", engine_type, "> found."))
}

setLimiter <- function(limiter_type) {
  if(limiter_type == "simplegoal") {
    return(limiterSimpleGoal)
  }

  # Throw error if we can't set engine
  stop(paste("No limiter of this name <", limiter_type, "> found."))
}

print.learningcurve <- function(x, ...) {
  # Ensure x is a learningcurve object
  if (!inherits(x, "learningcurve")) {
    stop("Object is not of class 'learningcurve'")
  }

  # Print the engine type
  print(paste("Engine Type:", x$engine_type))
  print(paste("Time Seq:", min(x$values$t), "-", max(x$values$t)))

}

plot.learningcurve <- function(x, ...) {
  # Ensure x is a learningcurve object
  if (!inherits(x, "learningcurve")) {
    stop("Object is not of class 'learningcurve'")
  }

  ggplot2::ggplot(data = x$values) +
    ggplot2::geom_line(ggplot2::aes(x = t, y = limited_performance), color = "blue", size = 1.1) +
    ggplot2::ylim(0, 1) + ggplot2::theme_classic() +
    ggplot2::xlab("Time") + ggplot2::ylab("Performance")

}

methods::setMethod("plot", "learningcurve", plot.learningcurve)
methods::setMethod("print", "learningcurve", print.learningcurve)

generateCurve <- function(engine_type, engine_pars,
                          limiter_type = NA, limiter_pars = NA,
                          time_end, time_start = 0, time_step = 0.01) {

  ############################
  # CONFIGURE LEARNING CURVE #
  ############################

  # Set Learning Curve Accumulator (Engine)
  engine <- setEngine(engine_type = engine_type)

  # Set Self-Regulation Module (Limiter)
  # A limiter is not required, so we check if set other than NA
  if(!is.na(limiter_type)) {
    limiter <- setLimiter(limiter_type = limiter_type)
  } else {
    limiter <- NA
  }

  ############################
  # CALCULATE PERFORMANCE    #
  ############################

  # This is simply the number and range of time steps to run the engine
  time_seq = seq(time_start, time_end, by = time_step)

  # Calculate raw output performance
  raw_output_perf <- engine(t = time_seq, pars = engine_pars)

  if(any(raw_output_perf > 1 | raw_output_perf < 0)) {
    warning("Check Engine Light: Data outside bounds of (0, 1)")
  }

  # Apply limiter
  if(!is.na(limiter)) {
    limited_output_perf <- limiter(raw_output_perf, limiter_pars)
  } else {
    limited_output_perf <- raw_output_perf
  }

  ############################
  # CREATE OBJECT            #
  ############################

  # Initialize the list
  output_obj <- list()

  # Assign elements to the list
  output_obj$engine_type <- engine_type
  output_obj$engine_pars <- engine_pars
  output_obj$values <- data.frame(t = time_seq,
                                  raw_performance = raw_output_perf,
                                  limited_performance = limited_output_perf)

  # Assign the class to the list
  class(output_obj) <- "learningcurve"

  return(output_obj)

}

test <- function() {
  x <- generateCurve(engine_type = "logistic", engine_pars = list(slope = 1, floor = 0, shift = 0),
                     limiter_type = "simplegoal", limiter_pars = list(goal = 0.85),
                     time_end = 10)
  print(x)
}
