# a more open version of discord_edgelist
e_discord_edgelist <- function(dat, at, network = 1, infstat = "i", susstat = "s") {
    
    status <- get_attr(dat, "status")
    active <- get_attr(dat, "active")
    
    el <- get_edgelist(dat, network)
    
    del <- NULL
    
    if (nrow(el) > 0) {
        el <- el[sample(seq_len(nrow(el))), , drop = FALSE]
        stat <- matrix(status[el], ncol = 2)
        
        isInf <- matrix(stat %in% infstat, ncol = 2)
        isSus <- matrix(stat %in% susstat, ncol = 2)
        
        SIpairs <- el[isSus[, 1] * isInf[, 2] == 1, , drop = FALSE]
        ISpairs <- el[isSus[, 2] * isInf[, 1] == 1, , drop = FALSE]
        
        pairs <- rbind(SIpairs, ISpairs[, 2:1])
        
        if (nrow(pairs) > 0) {
            sus <- pairs[, 1]
            inf <- pairs[, 2]
            del <- data.frame(at, sus, inf)
        
            # check for active status
            keep <- rowSums(matrix(c(active[del$sus], active[del$inf]),
                                   ncol = 2)) == 2
            del <- del[keep, ]
            if (nrow(del) < 1) {
                del <- NULL
            }
        }
    }
    
    return(del)
}