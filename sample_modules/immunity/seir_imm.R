# Susceptible > Exposed > Infected > Recovered > Susceptible
# SEIR + Immunity

# when are S, E, I, R updated within testatus.active

### Library Calls

library("EpiModel")
library("ndtv")

### Attribute Modules

immunity <- function(dat, at) {
  nw <- dat$nw
  if (at == 2) {
    n <- sum(dat$attr$active == 1)
    
    imm_val <- rpois(n, 0.5) + 1
    dat$attr$immunity <- imm_val
    nw[[1]]%v%'immunity' <- imm_val
  } else {
    # immunity falls off over time, with the immunity gained from a successful
    # recovery disappearing over 50 time steps
    imm_val <- ifelse(dat$attr$immunity > 1, dat$attr$immunity - 
                          dat$param$imm.decay, dat$attr$immunity)
    dat$attr$immunity <- imm_val
    nw[[1]]%v%"immunity" <- imm_val
  }
  
  if (at == 2) {
    dat$epi$meanImmunity <- c(NA_real_, mean(dat$attr$immunity, na.rm = TRUE))
  } else {
    dat$epi$meanImmunity[at] <- mean(dat$attr$immunity, na.rm = TRUE)
  }
  dat$nw <- nw
  return(dat)
}

### Infection Modules

infect_ise <- function(dat, at) {
  
  active <- dat$attr$active
  status <- dat$attr$status
  nw <- dat$nw
  
  idsInf <- which(active == 1 & status == "i")
  nActive <- sum(active == 1)
  
  nElig <- length(idsInf)
  nInf <- 0
  
  if (nElig > 0 && nElig < nActive) {
    del <- discord_edgelist(dat, at)
    del <- n_attr_edgelist(dat, at, del, susattr = "immunity")
    
    if (!(is.null(del))) {
      
      del$transProb <- dat$param$ise.prob
      del$actRate <- dat$param$act.rate
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate
      del$finalProb <- del$finalProb^del$sus.immunity
      
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      idsNewInf <- unique(del$sus)
      nInf <- length(idsNewInf)
      if (nInf > 0) {
        dat$attr$status[idsNewInf] <- "e"
        dat$attr$infTime[idsNewInf] <- at
      }
    }
  }
  
  if (at == 2) {
    dat$epi$se.flow <- c(0, nInf)
  }
  else {
    dat$epi$se.flow[at] <- nInf
  }
  dat$nw <- nw
  return(dat)
}

infect_ese <- function(dat, at) {
  
  active <- dat$attr$active
  status <- dat$attr$status
  nw <- dat$nw
  
  idsInf <- which(active == 1 & status == "e")
  nActive <- sum(active == 1)
  
  nElig <- length(idsInf)
  nInf <- 0
  
  if (nElig > 0 && nElig < nActive) {
    del <- discord_edgelist(dat, at)
    del <- n_attr_edgelist(dat, at, del, susattr = "immunity")
    
    if (!(is.null(del))) {
      
      del$transProb <- dat$param$ese.prob
      del$actRate <- dat$param$act.rate
      del$finalProb <- 1 - (1 - del$transProb)^del$actRate
      del$finalProb <- del$finalProb^del$sus.immunity
      
      transmit <- rbinom(nrow(del), 1, del$finalProb)
      del <- del[which(transmit == 1), ]
      idsNewInf <- unique(del$sus)
      nInf <- length(idsNewInf)
      if (nInf > 0) {
        dat$attr$status[idsNewInf] <- "e"
        dat$attr$infTime[idsNewInf] <- at
      }
    }
  }
  
  if (at == 2) {
    dat$epi$se.flow <- c(0, nInf)
  }
  else {
    dat$epi$se.flow[at] <- nInf
  }
  dat$nw <- nw
  return(dat)
}

### Progression Modules

progress <- function(dat, at) {
  nw <- dat$nw
  
  active <- dat$attr$active
  status <- dat$attr$status
  immunity <- dat$attr$immunity
  
  ei.rate <- dat$param$ei.rate
  ir.rate <- dat$param$ir.rate
  rs.rate <- dat$param$rs.rate
  
  ## E to I progression
  nInf <- 0
  idsEligInf <- which(active == 1 & status == "e")
  nEligInf <- length(idsEligInf)
  
  if (nEligInf > 0) {
    vecInf <- which(rbinom(nEligInf, 1, ei.rate) == 1)
    if (length(vecInf) > 0) {
      idsInf <- idsEligInf[vecInf]
      nInf <- length(idsInf)
      status[idsInf] <- "i"
    }
  }
  
  ## I to R progression
  nRec <- 0
  idsEligRec <- which(active == 1 & status == "i")
  nEligRec <- length(idsEligRec)
  
  if (nEligRec > 0) {
    vecRec <- which(rbinom(nEligRec, 1, ir.rate) == 1)
    if (length(vecRec) > 0) {
      idsRec <- idsEligRec[vecRec]
      nRec <- length(idsRec)
      status[idsRec] <- "r"
    }
  }

  ## R to S progression
  nSus <- 0
  idsEligSus <- which(active == 1 & status == "r")
  nEligSus <- length(idsEligSus)
  
  if (nEligSus > 0) {
    vecSus <- which(rbinom(nEligSus, 1, rs.rate) == 1)
    if (length(vecSus) > 0) {
      idsSus <- idsEligSus[vecSus]
      nSus <- length(idsSus)
      status[idsSus] <- "s"
      imm_val <- immunity[idsSus] + 2
      immunity[idsSus] <- imm_val
      nw[[1]]%v%"immunity" <- imm_val
    }
  }
  
  dat$attr$status <- status
  status <- dat$attr$status
  immunity <- dat$attr$immunity
  
  if (at == 2) {
    dat$epi$ei.flow <- c(0, nInf)
    dat$epi$ir.flow <- c(0, nRec)
    dat$epi$rs.flow <- c(0, nSus)
    
    dat$epi$s.num <- c(0, sum(active == 1 & status == "s"))
    dat$epi$e.num <- c(0, sum(active == 1 & status == "e"))
    dat$epi$i.num <- c(0, sum(active == 1 & status == "i"))
    dat$epi$r.num <- c(0, sum(active == 1 & status == "r"))
  }
  else {
    dat$epi$ei.flow[at] <- nInf
    dat$epi$ir.flow[at] <- nRec
    dat$epi$rs.flow[at] <- nSus
    
    dat$epi$s.num[at] <- sum(active == 1 & status == "s")
    dat$epi$e.num[at] <- sum(active == 1 & status == "e")
    dat$epi$i.num[at] <- sum(active == 1 & status == "i")
    dat$epi$r.num[at] <- sum(active == 1 & status == "r")
  }
  
  dat$nw <- nw
  
  return(dat)
}

### Network Simulation

# Initialize the network and estimate the ERGM
nw <- network::network.initialize(100, directed = FALSE)
est <- netest(nw, formation = ~ edges, target.stats = 50,
              coef.diss = dissolution_coefs(~ offset(edges), 10))

# Epidemic model parameterization
param <- param.net(ise.prob = 0.5, ese.prob = 0.5,
                   ei.rate = 0.075, ir.rate = 0.05, rs.rate = 0.1,
                   act.rate = 1, imm.decay = 0.04)
init <- init.net(i.num = 10)
control <- control.net(nsteps = 50, nsims = 5, initialize.FUN = e_initialize.net, 
                       immunity.FUN = immunity,
                       infection.FUN = NULL, infection_ise.FUN = infect_ise,
                       infection_ese.FUN = infect_ese,
                       recovery.FUN = NULL, progress.FUN = progress,
                       skip.check = TRUE, 
                       resimulate.network = FALSE, verbose.int = 0)

# Simulate the epidemic model
sim <- netsim(est, param, init, control)

# Plot the results
par(mfrow = c(1, 1))
plot(sim, y = c("s.num", "e.num", "i.num", "r.num"),
     mean.col = 1:4, qnts = 1, qnts.col = 1:4, legend = TRUE)

### Animations
ntwk <- get_network(sim)
ntwk <- e_color_tea(ntwk)
ntwk_light <- e_color_tea(ntwk, alpha = 0.15)

timeline(ntwk)

# set up layout to draw plots under timeline
layout(matrix(c(1,1,1,2,3,4),nrow=2,ncol=3,byrow=TRUE))
# plot a proximity.timeline illustrating infection spread
proximity.timeline(ntwk_light, vertex.col = 'ndtvcol',
                   spline.style='color.attribute',
                   mode = 'sammon',default.dist=10,
                   chain.direction='reverse')
#plot 3 static cross-sectional networks 
# (beginning, middle and end) underneath for comparison
plot(network.collapse(ntwk,at=1),vertex.col='ndtvcol',
     main='simulated network at t=1', vertex.cex = 1.5, edge.lwd = 2)
plot(network.collapse(ntwk,at=100),vertex.col='ndtvcol',
     main='simulated network at=100', vertex.cex = 1.5, edge.lwd = 2)
plot(network.collapse(ntwk,at=200),vertex.col='ndtvcol',
     main='simulated network at t=200', vertex.cex = 1.5, edge.lwd = 2)
layout(1) # reset the layout


# render an animation of the network

render.par <- list(tween.frames=10,show.time=TRUE,
                 show.stats=NULL)
plot.par <- list(mar = c(0, 0, 0, 0))

# render.animation(ntwk, render.par = render.par, vertex.col='ndtvcol', displaylabels=FALSE)
# ani.replay()

compute.animation(ntwk,animation.mode = 'MDSJ', chain.direction = 'reverse', verbose=FALSE)

render.d3movie(
  ntwk,
  vertex.tooltip = function(slice){paste('name:',slice%v%'vertex.names','<br>',
                                         'status:', slice%v%'testatus', '<br>',
                                         'immunity:', slice%v%'teimmunity')},
  d3.options=list(animationDuration=2000,enterExitAnimationFactor=0.5),
  render.par = render.par,
  plot.par = plot.par,
  vertex.cex = 0.9,
  vertex.col = "ndtvcol",
  vertex.border = "lightgrey",
  displaylabels = FALSE)

