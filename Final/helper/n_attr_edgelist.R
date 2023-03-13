
n_attr_edgelist <- function(dat, at, del, susattr = NULL, infattr = NULL) {
    
    # process susceptible attributes
    if (!is.null(susattr) & !is.null(del)) {
        req_args <- get_attr_list(dat, susattr)
        for (i in 1:length(req_args)) {
            del[paste0('sus.', names(req_args[i]))] <- 
                req_args[[i]][del$sus]
        }
    }
    
    # process infected attributes
    if (!is.null(infattr) & !is.null(del)) {
        req_args <- get_attr_list(dat, infattr)
        for (i in 1:lxength(req_args)) {
            del[paste0('inf.', names(req_args[i]))] <- 
                req_args[[i]][del$sus]
        }
    }
    return(del)
}
