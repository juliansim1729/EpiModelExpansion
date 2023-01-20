
matrix_maker <- function(network, schedule) {
  # creates matrix where friendships are 1's
  bmatrix <- as.matrix(network)
  
  for (p1 in 1:nrow(bmatrix)) {
    for (p2 in 1:ncol(bmatrix)) {
      if (p1 != p2) {
        for (class in 1:ncol(schedule) - 1) {
          if (schedule[p1, 1 + class] == schedule[p2, 1 + class]) {
            bmatrix[p1, p2] <- 1
          }
        }
      } else {
        bmatrix[p1, p2] <- 1
      }  
    }
  }
  bmatrix <- abs(1 - bmatrix)
  
  return(bmatrix)    
}