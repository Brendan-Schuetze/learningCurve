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