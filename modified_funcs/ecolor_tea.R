# like color tea, but works for more than SIR states

ecolor_tea <- function(nd, old.var = "testatus", new.var = "ndtvcol", verbose = TRUE) {
  
  times <- 1:max(get.change.times(nd))
  ntwk_states <- find_ntwk_states(nd)
  for (at in times) {
    stat <- get.vertex.attribute.active(nd, old.var, at = at)
    
    # red, green, blue, yellow, pink, gray
    colors <- c(2, 3, 4, 7, 6, 8)
    counter <- 1
    for (state in ntwk_states) {
        temp <- which(stat == state)
        nd <- activate.vertex.attribute(nd, prefix = new.var, value = adjustcolor(colors[counter], 0.75),
                                        onset = at, terminus = Inf, v = temp)
        counter <- counter + 1
    }
    
    if (verbose == TRUE) {
      cat("\n", at, "/", max(times), "\t", sep = "")
    }
  }
  
  return(nd)
}

find_ntwk_states <- function(nd) {
  foundStates <- vector()
  for (i in nd$val) {
    for (j in i$testatus.active[[1]]) {
      if (!(j %in% foundStates)) {
        foundStates <- append(foundStates, j)
      }
    }
  }
  return(foundStates)
}


