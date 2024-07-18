limiterBayesDeriv <- function(raw_output_perf, pars) {

  # TODO: Implement actual logic

  checkNames(pars = pars, names = c("prior", "deriv"))

  prior <- as.numeric(pars["prior"])
  deriv <- as.numeric(pars["deriv"])

  for(i in 1:length(raw_output_perf)) {

    # If we reach goal, stop studying
    if(raw_output_perf[i] >= goal) {

      raw_output_perf[i:length(raw_output_perf)] <- raw_output_perf[i]

      break
    }
  }

  return(raw_output_perf)

}
