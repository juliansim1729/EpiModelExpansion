copy_datattr_to_nwattr <- function(dat) {
    nwterms <- dat$temp$nwterms
    special.attr <- "status"
    if (dat$param$groups == 2) {
        special.attr <- c(special.attr, "group")
    }
    special.attr <- c(special.attr, "immunity")
    nwterms <- union(nwterms, special.attr)
    attr.to.copy <- union(nwterms, special.attr)
    attr <- dat$attr[attr.to.copy]
    if (length(attr.to.copy) > 0) {
        if (length(attr.to.copy) == 1) {
            print("yes")
            dat$nw[[1]] <- set_vertex_attribute(dat$nw[[1]], names(attr), attr[[1]])
        } else {
            print("no")
            dat$nw[[1]] <- set_vertex_attribute(dat$nw[[1]], names(attr), attr)
        }
    }
    return(dat)
}