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
