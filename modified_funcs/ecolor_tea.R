# like color tea, but works for more than SIR states
# currently just up to 6 for "pretty colors"

ecolor_tea <- function(nd, old.var = "testatus", new.var = "ndtvcol", alpha = 0.75, verbose = TRUE) {
  
  times <- 1:max(get.change.times(nd))
  ntwk_states <- find_ntwk_states(nd)
  for (at in times) {
    stat <- get.vertex.attribute.active(nd, old.var, at = at)
    print(stat)
    # red, green, blue, yellow, pink, gray
    colors <- c(2, 3, 4, 7, 6, 8)
    counter <- 1
    for (state in ntwk_states) {
        temp <- which(stat == state)
        nd <- activate.vertex.attribute(nd, prefix = new.var, value = adjustcolor(colors[counter], alpha),
                                        onset = at, terminus = Inf, v = temp)
        # nd <- activate.vertex.attribute(nd, prefix = a_immunity, value = )
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


