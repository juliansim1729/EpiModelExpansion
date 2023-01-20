## 0: Environment setup
library("ergm")
library("tergm")
library("EpiModel")
library("ndtv")

data(faux.mesa.high)

## 1: Modules

### 1.1 Time

time_passing <- function(dat, at) {
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  immunity <- get_attr(dat, "immunity")
  age <- get_attr(dat, "age")

  imm.decay <- get_param(dat, "imm.decay")
  imm.nRecMod <- get_param(dat, "imm.nRecMod")
  act.rate <- get_param(dat, "act.rate")
  
  idsNotRec <- which(active == 1 & status != "r")
  idsRec <- which(active == 1 & status == "r")
  idsElig <- which(active == 1)
  nElig <- length(idsElig)

  if (nElig > 0) {
    # immunity
    immunity[idsNotRec] <- ifelse(immunity[idsNotRec] > 0, immunity[idsNotRec] - 
                                    imm.nRecMod*imm.decay, immunity[idsNotRec])
    immunity[idsRec] <- ifelse(immunity[idsRec] > 0, immunity[idsRec] - 
                                 imm.decay, immunity[idsRec])
    immunity[idsElig] <- ifelse(immunity[idsElig] > 0, immunity[idsElig], 0)
    
    # age
    age[idsElig] <- age[idsElig] + act.rate * 1 / 365
  }
  
  dat <- set_attr(dat, "immunity", immunity)
  dat <- set_attr(dat, "age", age)
  
  dat <- set_epi(dat, "meanImmunity", at, mean(immunity, na.rm = TRUE))
  dat <- set_epi(dat, "meanAge", at, mean(age, na.rm = TRUE))
  
  if (at == 3) {
    # dat <- set_epi(dat, "e.num", 1, 0)
    # dat <- set_epi(dat, "r.num", 1, 0)
  }
  
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
  
}

## 1.2: Infection - Infected causes Susceptible -> Exposed

infect_ise <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  infTime <- get_attr(dat, "infTime")
  
  act.rate <- get_param(dat, "act.rate")
  ise.prob <- get_param(dat, "ise.prob")
  
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  
  nElig <- length(idsInf)
  nInf <- 0
  
  if (nElig > 0 && nElig < nActive) {
    # get discord el with sus attributes
    del <- discord_edgelist(dat, at)
    del <- n_attr_edgelist(dat, at, del, susattr = "immunity")
    
    if (!(is.null(del))) {
      
      del$transProb <- ise.prob
      del$actRate <- act.rate
      del$adjProb <- 1 - (1 - del$transProb)^del$actRate
      del$finalProb <- del$adjProb^(1 + 2*del$sus.immunity)
      
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      idsNewInf <- unique(del$sus)
      nInf <- length(idsNewInf)
      
      if (nInf > 0) {
        status[idsNewInf] <- "e"
        infTime[idsNewInf] <- at
        
        dat <- set_attr(dat, "status", status)
        dat <- set_attr(dat, "infTime", infTime)
      }
    }
  }
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
  
  dat <- set_epi(dat, "se.flow", at, nInf)
  
  return(dat)
}

## 1.3: Progression of Exposed to Infected

progress_ei <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  
  ei.rate <- get_param(dat, "ei.rate")
  
  n.ei <- 0
  idsElig.ei <- which(active == 1 & status == "e")
  nElig.ei <- length(idsElig.ei)
  
  if (nElig.ei > 0) {
    vec.ei <- which(rbinom(nElig.ei, 1, ei.rate) == 1)
    if (length(vec.ei) > 0) {
      ids.ei <- idsElig.ei[vec.ei]
      n.ei <- length(ids.ei)
      status[ids.ei] <- "i"
    }
  }
  dat <- set_attr(dat, "status", status)
  
  dat <- set_epi(dat, "ei.flow", at, n.ei)
  
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
  
  return(dat)
}

## 1.3: Progression of Infected to Recovered

progress_ir <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  immunity <- get_attr(dat, "immunity")
  
  ir.rate <- get_param(dat, "ir.rate")
  imm.gain <- get_param(dat, "imm.gain")
  
  n.ir <- 0
  idsElig.ir <- which(active == 1 & status == "i")
  nElig.ir <- length(idsElig.ir)
  
  if (nElig.ir > 0) {
    vec.ir <- which(rbinom(nElig.ir, 1, ir.rate) == 1)
    if (length(vec.ir) > 0) {
      ids.ir <- idsElig.ir[vec.ir]
      n.ir <- length(ids.ir)
      status[ids.ir] <- "r"
      immunity[ids.ir] <- immunity[ids.ir] + imm.gain
    }
  }
  dat <- set_attr(dat, "immunity", immunity)
  dat <- set_attr(dat, "status", status)
  
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
  
  
  dat <- set_epi(dat, "ir.flow", at, n.ir)
  dat <- set_epi(dat, "meanImmunity", at, mean(immunity))

  
  return(dat)
}

## 1.4 Progression of Recovered to Susceptible

progress_rs <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  immunity <- get_attr(dat, "immunity")
  
  rs.rate <- get_param(dat, "ir.rate")
  
  n.rs <- 0
  idsElig.rs <- which(active == 1 & status == "r")
  nElig.rs <- length(idsElig.rs)
  
  if (nElig.rs > 0) {
    vec.rs <- which(rbinom(nElig.rs, 1, rs.rate^(1 + immunity[idsElig.rs])) == 1)
    if (length(vec.rs) > 0) {
      ids.rs <- idsElig.rs[vec.rs]
      n.rs <- length(ids.rs)
      status[ids.rs] <- "s"
    }
  }
  
  dat <- set_attr(dat, "status", status)
  
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
  
  dat <- set_epi(dat, "rs.flow", at, n.rs)

  return(dat)
}

## TODO: Rewrite Demographic

dfunc <- function(dat, at) {
  
  ## Attributes
  active <- get_attr(dat, "active")
  exitTime <- get_attr(dat, "exitTime")
  age <- get_attr(dat, "age")
  status <- get_attr(dat, "status")
  
  ## Parameters
  dep.rates <- get_param(dat, "departure.rates")
  dep.dis.mult <- get_param(dat, "departure.disease.mult")
  
  ## Query alive
  idsElig <- which(active == 1)
  nElig <- length(idsElig)
  
  ## Initialize trackers
  nDepts <- 0
  idsDepts <- NULL
  
  if (nElig > 0) {
    
    ## Calculate age-specific departure rates for each eligible node ##
    ## Everyone older than 85 gets the final mortality rate
    whole_ages_of_elig <- pmin(ceiling(age[idsElig]), 86)
    drates_of_elig <- dep.rates[whole_ages_of_elig]
    
    ## Multiply departure rates for diseased persons
    idsElig.inf <- which(status[idsElig] == "i")
    drates_of_elig[idsElig.inf] <- drates_of_elig[idsElig.inf] * 
      dep.dis.mult
    
    ## Simulate departure process
    vecDepts <- which(rbinom(nElig, 1, drates_of_elig) == 1)
    idsDepts <- idsElig[vecDepts]
    nDepts <- length(idsDepts)
    
    ## Update nodal attributes
    if (nDepts > 0) {
      active[idsDepts] <- 0
      exitTime[idsDepts] <- at
    }
  }
  
  ## Set updated attributes
  dat <- set_attr(dat, "active", active)
  dat <- set_attr(dat, "exitTime", exitTime)
  
  ## Summary statistics ##
  dat <- set_epi(dat, "total.deaths", at, nDepts)
  
  # covid deaths
  covid.deaths <- length(intersect(idsDepts, which(status == "i")))
  dat <- set_epi(dat, "covid.deaths", at, covid.deaths)
  
  return(dat)
}

afunc <- function(dat, at) {
  
  ## Parameters ##
  n <- get_epi(dat, "num", at - 1)
  a.rate <- get_param(dat, "arrival.rate")
  
  ## Process ##
  nArrivalsExp <- n * a.rate
  nArrivals <- rpois(1, nArrivalsExp)
  
  # Update attributes
  if (nArrivals > 0) {
    dat <- append_core_attr(dat, at = at, n.new = nArrivals)
    dat <- append_attr(dat, "status", "s", nArrivals)
    dat <- append_attr(dat, "infTime", NA, nArrivals)
    dat <- append_attr(dat, "age", 0, nArrivals)
  }
  
  ## Summary statistics ##
  dat <- set_epi(dat, "a.flow", at, nArrivals)
  
  return(dat)
}

## 2: Network Simulation

### 2.1: Network Initialization

nw <- faux.mesa.high

fmh_grades <- get.node.attr(faux.mesa.high, 'Grade')
fmh_schedule <- trad_classgen(fmh_grades, 12)

for (period in 1:9) {
  fmh_pdV <- paste0('pd', period)
  nw <- network::set.vertex.attribute(nw, fmh_pdV, fmh_schedule[, period + 1])
}

init <- init.net(i.num = 10)

### 2.2: ERGM Formula

ergm_formula <- ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
  nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") + 
  nodematch("pd8")

### 2.3: Calculate Target Statistics
# nw_size <- length(nw$val)
# 
# m <- matrix(1, nw_size, nw_size)
# m <- m - diag(nw_size)
# 
# nw_complete <- network(m, directed = FALSE)
# 
# for (period in 1:9) {
#   fmh_pdV <- paste0('pd', period)
#   nw_complete <- network::set.vertex.attribute(nw_complete, fmh_pdV, 
#                                                fmh_schedule[, period + 1])
# }
# 
# vals <- nw_complete ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
#   nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") + 
#   nodematch("pd8")
# 
# complete_graph_targets <- summary(vals)

## recalibrate these values
# target.stats <- 0.007 * complete_graph_targets
# target.stats[1] <- 0.01 * complete_graph_targets

ergm_mod <- ergm(nw ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
                   nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + 
                   nodematch("pd7") + nodematch("pd8"))

sim_nws <- simulate(ergm_mod, nsim = 4)
summary(sim_nws) # not sure how to pull values from here directly

target.stats <- c(196, 41, 49, 57, 53, 53, 34, 47, 56)


### 2.4: Network Dissolution
coef.diss <- dissolution_coefs(~offset(edges), 200)

### 2.5: Network Estimation
est <- netest(nw, formation = ergm_formula, target.stats = target.stats, 
              coef.diss = coef.diss)

### 2.6 Network Control
nsteps <- 50
control <- control.net(type = NULL, nsteps = nsteps, nsims = 1, 
                       infection.FUN = NULL, recovery.FUN = NULL, time_passing.FUN = time_passing,
                       initialize.FUN = e_initialize.net, infect_ise.FUN = infect_ise,
                       progress_ei.FUN = progress_ei, progress_ir.FUN = progress_ir,
                       progress_rs.FUN = progress_rs, nwupdate.FUN = e_nwupdate.net,
                       departures.FUN = dfunc, arrivals.FUN = afunc,
                       skip.check = TRUE, 
                       resimulate.network = FALSE, verbose.int = 0)

### 2.7 Network Parameters
dr_pc <- c(588.45, 24.8, 11.7, 14.55, 47.85, 88.2, 105.65, 127.2,
           154.3, 206.5, 309.3, 495.1, 736.85, 1051.15, 1483.45,
           2294.15, 3642.95, 6139.4, 13938.3) / 1e5 / 365
age_spans <- c(1, 4, rep(5, 16), 1)
dr_vec <- rep(dr_pc, times = age_spans)

param <- param.net(ise.prob = 0.9,
                   ei.rate = 0.4, ir.rate = 0.05, rs.rate = 0.015,
                   imm.gain = 2, imm.decay = 0.1, imm.nRecMod = 2,
                   departure.rates = dr_vec, departure.disease.mult = 100,
                   arrival.rate = 1/(365*85),
                   act.rate = 1)

### 2.8 Simulation
sim <- netsim(est, param, init, control)

## 3: Plots & Animations
ntwk <- get_network(sim)
ntwk <- n_size_tea(ntwk, "teimmunity")
ntwk <- e_color_tea(ntwk)
ntwk_light <- e_color_tea(ntwk, alpha = 0.15)

### 3.1: Plots
par(mfrow = c(1, 1))
plot(sim, y = c("s.num", "e.num", "i.num", "r.num"),
     mean.col = 1:4, qnts = 1, qnts.col = 1:4, legend = TRUE)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE))

# plot(network.collapse(ntwk, at = 1), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', 1),
#      vertex.cex = 1.5, edge.lwd = 2)
# plot(network.collapse(ntwk, at = floor(nsteps/5)), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', floor(nsteps/5)),
#      vertex.cex = 1.5, edge.lwd = 2)
# plot(network.collapse(ntwk, at = floor(2*nsteps/5)), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', floor(2*nsteps/5)),
#      vertex.cex = 1.5, edge.lwd = 2)
# plot(network.collapse(ntwk, at = floor(3*nsteps/5)), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', floor(3*nsteps/5)),
#      vertex.cex = 1.5, edge.lwd = 2)
# plot(network.collapse(ntwk, at = floor(4*nsteps/5)), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', floor(4*nsteps/5)),
#      vertex.cex = 1.5, edge.lwd = 2)
# plot(network.collapse(ntwk, at = nsteps), vertex.col='ndtvcol',
#      main = paste0('simulated network at t=', nsteps),
#      vertex.cex = 1.5, edge.lwd = 2)

layout(1)

### 3.2: Animations

# render an animation of the network
# render.par <- list(tween.frames=5,show.time=TRUE,
#                    show.stats=NULL )# ,extraPlotCmds = expression(
#                    #  legend(locator(1), legend = slice%v%'testatus')))
# plot.par <- list(mar = c(0, 0, 0, 0))
# 
# compute.animation(ntwk,animation.mode = 'MDSJ', chain.direction = 'reverse', verbose=FALSE)
# 
# # TODO: floating legend
# render.d3movie(
#   ntwk,
#   vertex.tooltip = function(slice){paste('name:',slice%v%'vertex.names','<br>',
#                                          'status:', slice%v%'testatus', '<br>',
#                                          'immunity:', round(slice%v%'teimmunity', 3), '<br>',
#                                          'age:', round(slice%v%'teage', 3), '<br>',
#                                          'sex:', slice%v%'Sex')},
#   d3.options=list(animationDuration=2000,enterExitAnimationFactor=0.5),
#   render.par = render.par, plot.par = plot.par,
#   vertex.cex = "ndtvcex", vertex.col = "ndtvcol", vertex.border = "lightgrey",
#   label.cex=0.8,label.col="black", verbose = FALSE,
#   main = "Simulated Spread of SEIR with Tracked Immunity on a Network",
#   displaylabels = TRUE)

