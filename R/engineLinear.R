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
