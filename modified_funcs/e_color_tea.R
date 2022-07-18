# like color tea, but works for more than SIR states
# currently just up to 6 for "pretty colors"

e_color_tea <- function(nd, old.var = "testatus", new.var = "ndtvcol", alpha = 0.75, verbose = TRUE) {
  
  times <- 1:max(get.change.times(nd))
  ntwk_states <- n_find_ntwk_states(nd)
  for (at in times) {
    stat <- get.vertex.attribute.active(nd, old.var, at = at)
    # red, green, blue, yellow, pink, gray
    colors <- c(2, 3, 4, 7, 6, 8)
    counter <- 1
    for (state in ntwk_states) {
        temp <- which(stat == state)
        nd <- activate.vertex.attribute(nd, prefix = new.var, value = adjustcolor(colors[counter], alpha),
                                        onset = at, terminus = Inf, v = temp)
        counter <- counter + 1
    }
    
    if (verbose == TRUE) {
      cat("\n", at, "/", max(times), "\t", sep = "")
    }
  }
  
  return(nd)
}

n_size_tea <- function(nd, old.var, new.var = "ndtvcex", verbose = TRUE) {
  
  times <- 1:max(get.change.times(nd))
  for (at in times) {
    attr <- get.vertex.attribute.active(nd, old.var, at = at)
    
    nd <- activate.vertex.attribute(nd, prefix = new.var, value = 1 + (attr/3),
                                    onset = at, terminus = Inf)
    if (verbose == TRUE) {
      cat("\n", at, "/", max(times), "\t", sep = "")
    }
  }
  return(nd)
}


n_find_ntwk_states <- function(nd) {
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


