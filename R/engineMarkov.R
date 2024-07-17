engineMarkov <- function(time_seq, pars) {
  
  # Check if all required parameters are present in 'pars'
  checkNames(pars = pars, names = c("prior", "learns", "guess", "slips", "n_items"))
  
  # Convert parameters from 'pars' to numeric values
  prior <- as.numeric(pars["prior"])
  learns <- as.numeric(pars["learns"])
  slips <- as.numeric(pars["slips"])
  guess <- as.numeric(pars["guess"])
  n_items <- as.numeric(pars["n_items"])
  
  # Loop over each item
  for(i in 1:n_items) {
    first <- TRUE  # Flag to handle the first time point separately
    
    correct_v <- c()  # Vector to store correctness of responses for current item
    learned_v <- c()  # Vector to store learning states for current item
    
    # Loop over each time point in the sequence
    for(j in time_seq) {
      if(first) {
        # Initialize learning state based on prior probability for the first time point
        learned <- rbinom(n = 1, size = 1, prob = prior)
        first <- FALSE  # Set flag to FALSE after the first iteration
      } else if (learned == 0) {
        # Update learning state if not learned yet, based on learning probability
        learned <- rbinom(n = 1, size = 1, prob = learns)
      }
      
      # Determine correctness based on current learning state
      if(learned == 1) {
        correct <- rbinom(n = 1, size = 1, prob = (1 - slips))  # If learned, use slip probability
      } else {
        correct <- rbinom(n = 1, size = 1, prob = guess)  # If not learned, use guess probability
      }
      
      # Append results to vectors
      correct_v <- c(correct_v, correct)
      learned_v <- c(learned_v, learned)
    }
    
    # Create or append to data frames for correctness and learning states
    if(i == 1) {
      correct_df <- data.frame(correct_v)
      learned_df <- data.frame(learned_v)
    } else {
      correct_df <- cbind(correct_df, correct_v)
      learned_df <- cbind(learned_df, learned_v)
    }
  }
  
  # Compute average performance by taking the row means of the correctness data frame
  output_perf <- rowMeans(correct_df)
  
  return(output_perf)  # Return the average performance output
}