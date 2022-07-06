# interface functions

library(stringr)

read_info <- function() {
  # Read in network states and clean
  states <- readline(prompt = "Enter Network States (separated by comma): ")
  states <- strsplit(states, ",")[[1]]
  states <- str_trim(states)
  
  # Create empty labelled rate vector
  n <- length(states)
  rate_vector <- numeric(n * n)
  
  # Input in weights for matrix
  counter <- 1
  for (fromState in states) {
    for (toState in states) {
      if (fromState != toState) {
        rate_vector[counter] <- as.numeric(readline(prompt = str_interp("What is the rate at which ${fromState} goes to ${toState}: ")))
      }
      counter <- counter + 1
    }
  }
  
  # create weight matrix with rate vector
  rate_matrix <- matrix(rate_vector, nrow = n, ncol = n, byrow = TRUE)
  rownames(rate_matrix) <- states
  colnames(rate_matrix) <- states
  
  return(rate_matrix)
}