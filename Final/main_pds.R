

# Environment Setup ----
library("ergm")
library("tergm")
library("EpiModel")
library("ndtv")

data(faux.mesa.high)

# Params
p_numIter = 1

# Modules ----

mod_onStep <- function(dat, at) {
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  immunity <- get_attr(dat, "immunity")
  age <- get_attr(dat, "age")
  
  imm.decay <- get_param(dat, "imm.decay")
  imm.nRecMod <- get_param(dat, "imm.nRecMod")
  
  idsNotRec <- which(active == 1 & status != "r")
  idsRec <- which(active == 1 & status == "r")
  idsElig <- which(active == 1)
  
  nElig <- length(idsElig)
  
  if (nElig > 0) {
    # immunity
    immunity[idsElig] <- ifelse(immunity[idsElig] > 0, immunity[idsElig]/2, immunity[idsElig])
    immunity[idsElig] <- ifelse(immunity[idsElig] > 0.01, immunity[idsElig], 0)
    
    # age
    age[idsElig] <- age[idsElig] + ( 1 / 365 )
  }
  
  dat <- set_attr(dat, "immunity", immunity)
  dat <- set_attr(dat, "age", age)
  
}

mod_epiRecord <- function(dat, at) {
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")
  
  
  if (at == 2) {
    # TODO: edit init to automatically do this
    dat <- set_epi(dat, "e.num", 1, 0)
    dat <- set_epi(dat, "r.num", 1, 0)
  }
  
  dat <- set_epi(dat, "s.num", at, sum(active == 1 & status == "s"))
  dat <- set_epi(dat, "e.num", at, sum(active == 1 & status == "e"))
  dat <- set_epi(dat, "i.num", at, sum(active == 1 & status == "i"))
  dat <- set_epi(dat, "r.num", at, sum(active == 1 & status == "r"))
}

mod_infect_ise <- function(dat, at) {
  
  active <- get_attr(dat, "active")
  status <- get_attr(dat, "status")

  act.rate <- get_param(dat, "act.rate")
  ise.prob <- get_param(dat, "ise.prob")
  
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  
  nElig <- length(idsInf)
  nInf <- 0
  
  if (nElig > 0 && nElig < nActive) {
    # get discord el with vertex attributes
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
      }
    }
  }
  
  dat <- set_attr(dat, "status", status)

  return(dat)
}

mod_progress_ei <- function(dat, at) {
  
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

  return(dat)
}

mod_progress_ir <- function(dat, at) {
  
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

  return(dat)
}

mod_progress_rs <- function(dat, at) {
  
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

  return(dat)
}

# NetSim ----

nw <- faux.mesa.high

fmh_grades <- get.node.attr(faux.mesa.high, 'Grade')
fmh_schedule <- trad_classgen(fmh_grades, 20)

for (period in 1:9) {
  fmh_pdV <- paste0('pd', period)
  nw <- network::set.vertex.attribute(nw, fmh_pdV, fmh_schedule[, period + 1])
}

nw <- network::set.vertex.attribute(nw, "lunch", fmh_schedule[, 11])

nocontact_mat <- matrix_maker(faux.mesa.high, fmh_schedule)

init <- init.net(i.num = 10)

coef.diss <- dissolution_coefs(~offset(edges), 1)

ergm_formula <- ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") +
  nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") +
  nodematch("pd8") + nodematch("pd9") + nodematch("lunch") + 
  offset(edgecov(as.matrix(faux.mesa.high))) +
  offset(edgecov(nocontact_mat))

for (n in 1:p_numIter) {
  fmh_sim <- simulate(nw ~ edges + nodematch("pd1") + nodematch("pd2") +
                        nodematch("pd3") + nodematch("pd4") + nodematch("pd5") +
                        nodematch("pd6") + nodematch("pd7") + nodematch("pd8") +
                        nodematch("pd9") + nodematch("lunch") + edgecov(as.matrix(faux.mesa.high)) +
                        edgecov(nocontact_mat),
                      coef = c(-1.0986, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, -0.1, Inf, -Inf),
                      control = control.simulate(MCMC.burnin = 1000000, MCMC.interval = 100))
  
  target.stats <- attr(fmh_sim, "stats")[1:11]
  
  est <- netest(nw, formation = ergm_formula, target.stats = target.stats, 
                coef.diss = coef.diss, coef.form = c(Inf, -Inf))
  
  nsteps <- 270
  control <- control.net(type = NULL, nsteps = nsteps, nsims = 1, 
                         infection.FUN = NULL, recovery.FUN = NULL, 
                         onStep.FUN = mod_onStep, initialize.FUN = e_initialize.net, 
                         progress_ir.FUN = mod_progress_ir, progress_ei.FUN = mod_progress_ei, 
                         infect_ise.FUN = mod_infect_ise, progress_rs.FUN = mod_progress_rs, 
                         nwupdate.FUN = e_nwupdate.net, epiRecord.FUN = mod_epiRecord,
                         skip.check = TRUE, 
                         resimulate.network = FALSE, verbose.int = 0)
  
  ### 2.6: Network Parameters
  param <- param.net(ise.prob = 0.25,
                     ei.rate = 0.1, ir.rate = 0.05, rs.rate = 0.02,
                     imm.gain = 1, imm.decay = 0.1, imm.nRecMod = 2,
                     departure.disease.mult = 100,
                     arrival.rate = 1/(365*85),
                     act.rate = 1)
  
  sim <- netsim(est, param, init, control)
  
  if (n == 1) {
    print("Recording...")
    dflist_plot <- list(s.num = data.frame(sim1 = sim$epi$s.num), e.num = data.frame(sim1 = sim$epi$e.num), 
                        i.num = data.frame(sim1 = sim$epi$i.num), r.num = data.frame(sim1 = sim$epi$r.num),
                        num = data.frame(sim1 = sim$epi$num))
  }

  dflist_plot$s.num[paste("sim", n, sep = "")] = sim$epi$s.num
  dflist_plot$e.num[paste("sim", n, sep = "")] = sim$epi$e.num
  dflist_plot$i.num[paste("sim", n, sep = "")] = sim$epi$i.num
  dflist_plot$r.num[paste("sim", n, sep = "")] = sim$epi$r.num
  dflist_plot$num[paste("sim", n, sep = "")] = sim$epi$num
  
  print(paste(n, "/", p_numIter, " trials completed.", sep = ""))
}

sim1 <- sim

sim$epi$s.num <- dflist_plot$s.num
sim$epi$e.num <- dflist_plot$e.num
sim$epi$i.num <- dflist_plot$i.num
sim$epi$r.num <- dflist_plot$r.num
sim$epi$num <- dflist_plot$num
sim$control$nsims <- p_numIter

plot(sim)

sim <- sim1

## 3: Plots & Animations
ntwk <- get_network(sim)
ntwk <- n_size_tea(ntwk, "teimmunity")
ntwk <- e_color_tea(ntwk)
ntwk_light <- e_color_tea(ntwk, alpha = 0.15)

layout(matrix(c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE))
# 
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

plot(sim, type = "network", at = 1, sims = "mean", col.status = TRUE, main = "Prevalence at t = 1")
plot(sim, type = "network", at = 90, sims = "mean", col.status = TRUE, main = "Prevalence at t = 1")
plot(sim, type = "network", at = 180, sims = "mean", col.status = TRUE, main = "Prevalence at t = 1")
plot(sim, type = "network", at = 270, sims = "mean", col.status = TRUE, main = "Prevalence at t = 1")


### 3.2: Animations

# render an animation of the network
render.par <- list(tween.frames=5,show.time=TRUE,
                   show.stats=NULL )# ,extraPlotCmds = expression(
#  legend(locator(1), legend = slice%v%'testatus')))
plot.par <- list(mar = c(0, 0, 0, 0))

compute.animation(ntwk,animation.mode = 'MDSJ', chain.direction = 'reverse', verbose=FALSE)

# TODO: floating legend
render.d3movie(
  ntwk,
  vertex.tooltip = function(slice){paste('name:',slice%v%'vertex.names','<br>',
                                         'status:', slice%v%'testatus', '<br>',
                                         'immunity:', round(slice%v%'teimmunity', 3), '<br>',
                                         'age:', round(slice%v%'teage', 3), '<br>',
                                         'sex:', slice%v%'Sex')},
  d3.options=list(animationDuration=2000,enterExitAnimationFactor=0.5),
  render.par = render.par, plot.par = plot.par,
  vertex.cex = "ndtvcex", vertex.col = "ndtvcol", vertex.border = "lightgrey",
  label.cex=0.8,label.col="black", verbose = FALSE,
  main = "Simulated Spread of SEIR with Tracked Immunity on a Network",
  displaylabels = TRUE)
