# https://github.com/EpiModel/EpiModel/blob/ebac1efd8d3e6555ff685c139754b9f3ec5ac71d/R/net.mod.init.R
# ISSUES: 
# can only initialize infected amounts
# groups can only operate within 2
# status is only tracked object

init_status2.net <- function(dat) {
  
  type <- get_control(dat, "type", override.null.error = TRUE)
  type <- if (is.null(type)) "None" else type
  
  nsteps <- get_control(dat, "nsteps")
  tergmLite <- get_control(dat, "tergmLite")
  vital <- get_param(dat, "vital")
  groups <- get_param(dat, "groups")
  status.vector <- get_init(dat, "status.vector", override.null.error = TRUE)
  if (type %in% c("SIS", "SIR")) {
    rec.rate <- get_param(dat, "rec.rate")
  }
  if (vital == TRUE) {
    di.rate <- get_param(dat, "di.rate")
  }
  
  # Variables ---------------------------------------------------------------
  i.num <- get_init(dat, "i.num", override.null.error = TRUE)
  if (type  == "SIR" && is.null(status.vector)) {
    r.num <- get_init(dat, "r.num")
  }
  
  num <- sum(get_attr(dat, "active") == 1)
  
  if (groups == 2) {
    group <- get_attr(dat, "group")
    if (!all(group %in% c(1, 2))) {
      stop(
        "When using the `group` attribute, the only authorized values",
        " are 1 and 2.\n",
        "The values found were: ", paste0(unique(group), collapse = ", ")
      )
    }
    
    i.num.g2 <- get_init(dat, "i.num.g2")
    if (type  == "SIR" && is.null(status.vector)) {
      r.num.g2 <- get_init(dat, "r.num.g2", override.null.error = TRUE)
    }
  } else {
    group <- rep(1, num)
  }
  
  statOnNw <- "status" %in% dat$temp$nwterms
  
  # Status ------------------------------------------------------------------
  
  ## Status passed on input network
  if (statOnNw == FALSE) {
    if (!is.null(status.vector)) {
      status <- status.vector
    } else {
      status <- rep("s", num)
      status[sample(which(group == 1), size = i.num)] <- "i"
      if (groups == 2) {
        status[sample(which(group == 2), size = i.num.g2)] <- "i"
      }
      if (type == "SIR") {
        status[sample(which(group == 1 & status == "s"), size = r.num)] <- "r"
        if (groups == 2) {
          status[sample(which(group == 2 & status == "s"),
                        size = r.num.g2)] <- "r"
        }
      }
    }
    dat <- set_attr(dat, "status", status)
  } else {
    status <- get_vertex_attribute(dat$nw[[1]], "status")
    dat <- set_attr(dat, "status", status)
  }
  
  
  ## Set up TEA status
  if (tergmLite == FALSE) {
    if (statOnNw == FALSE) {
      dat$nw[[1]] <- set_vertex_attribute(dat$nw[[1]], "status", status)
    }
    dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]],
                                             prefix = "testatus",
                                             value = status,
                                             onset = 1,
                                             terminus = Inf)
    dat$nw[[1]] <- activate.vertex.attribute(dat$nw[[1]],
                                             prefix = "teimmunity",
                                             value = immunity,
                                             onset = 1,
                                             terminus = Inf)
  }
  
  
  # Infection Time ----------------------------------------------------------
  ## Set up inf.time vector
  if (type == "None") {
    infTime <- rep(NA, num)
    idsInf <- idsInf <- which(status == "i")
    infTime[idsInf] <- 1
    dat <- set_attr(dat, "infTime", infTime)
  } else {
    idsInf <- which(status == "i")
    infTime <- rep(NA, length(status))
    infTime.vector <- get_init(dat, "infTime.vector",
                               override.null.error = TRUE)
    
    if (!is.null(infTime.vector)) {
      infTime <- infTime.vector
    } else {
      # If vital dynamics, infTime is a geometric draw over the duration of
      # infection
      if (vital == TRUE && di.rate > 0) {
        if (type == "SI") {
          infTime[idsInf] <- -rgeom(n = length(idsInf), prob = di.rate) + 2
        } else {
          infTime[idsInf] <- -rgeom(n = length(idsInf),
                                    prob = di.rate +
                                      (1 - di.rate) * mean(rec.rate)) + 2
        }
      } else {
        if (type == "SI" || mean(rec.rate) == 0) {
          # if no recovery, infTime a uniform draw over the number of sim time
          # steps
          infTime[idsInf] <- ssample(1:(-nsteps + 2),
                                     length(idsInf), replace = TRUE)
        } else {
          infTime[idsInf] <- -rgeom(n = length(idsInf),
                                    prob = mean(rec.rate)) + 2
        }
      }
    }
    
    dat <- set_attr(dat, "infTime", infTime)
  }
  
  return(dat)
}