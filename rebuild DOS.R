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
    age[idsElig] <- age[idsElig] + ( 1 / 365 )
  }
  
  if (at == 2) {
    dat <- set_epi(dat, "e.num", 1, 0)
    dat <- set_epi(dat, "r.num", 1, 0)
  }
  
  dat <- set_attr(dat, "immunity", immunity)
  dat <- set_attr(dat, "age", age)
  
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
    
    # print(del)
    
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

## 2: Network Simulation

### 2.1: Network Initialization

nw <- faux.mesa.high

fmh_grades <- get.node.attr(faux.mesa.high, 'Grade')
fmh_schedule <- trad_classgen(fmh_grades, 12)

for (period in 1:9) {
  fmh_pdV <- paste0('pd', period)
  nw <- network::set.vertex.attribute(nw, fmh_pdV, fmh_schedule[, period + 1])
}

nocontact_mat <- matrix_maker(faux.mesa.high, fmh_schedule)

init <- init.net(i.num = 10)

### 2.2: Network Dissolution
coef.diss <- dissolution_coefs(~offset(edges), 1)

### 2.3: ERGM Formula

ergm_formula <- ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") +
  nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") +
  nodematch("pd8") + nodematch("pd9") + offset(edgecov(as.matrix(faux.mesa.high))) +
  offset(edgecov(nocontact_mat))

# ergm_formula <- ~ edges + nodematch("pd1") + 
#   edgecov(as.matrix(faux.mesa.high)) +
#   edgecov(nocontact_mat)

# TODO: Init Loop
# TODO: Adjust coefs based on some parameter -- e.g. measles vs generic

for (n in 1:1) {
  fmh_sim <- simulate(nw ~ edges + nodematch("pd1") + nodematch("pd2") +
                        nodematch("pd3") + nodematch("pd4") + nodematch("pd5") +
                        nodematch("pd6") + nodematch("pd7") + nodematch("pd8") +
                        nodematch("pd9") + edgecov(as.matrix(faux.mesa.high)) +
                        edgecov(nocontact_mat),
                      coef = c(0, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, Inf, -Inf),
                      control = control.simulate(MCMC.burnin = 1000000, MCMC.interval = 100))
  
  target.stats <- attr(fmh_sim, "stats")[1:10]
  
  est <- netest(nw, formation = ergm_formula, target.stats = target.stats, 
                coef.diss = coef.diss, coef.form = c(Inf, -Inf))
  
  nsteps <- 45
  control <- control.net(type = NULL, nsteps = nsteps, nsims = 5, 
                         infection.FUN = NULL, recovery.FUN = NULL, time_passing.FUN = time_passing,
                         initialize.FUN = e_initialize.net, infect_ise.FUN = infect_ise,
                         progress_ei.FUN = progress_ei, progress_ir.FUN = progress_ir,
                         progress_rs.FUN = progress_rs, nwupdate.FUN = e_nwupdate.net,
                         skip.check = TRUE, 
                         resimulate.network = FALSE, verbose.int = 0)
  
  ### 2.6: Network Parameters
  param <- param.net(ise.prob = 0.3,
                     ei.rate = 0.05, ir.rate = 0.03, rs.rate = 0.001,
                     imm.gain = 2, imm.decay = 0.1, imm.nRecMod = 2,
                     departure.disease.mult = 100,
                     arrival.rate = 1/(365*85),
                     act.rate = 1)
  
  sim <- netsim(est, param, init, control)
  
  if (n == 1) {
    print("Recording...")
    dflist_plot <- list()
  }
  
  temp_df = data.frame(sim$epi$s.num, sim$epi$e.num, sim$epi$i.num, r.num = sim$epi$r.num)
  colnames(temp_df) <- c("s.num", "e.num", "i.num", "r.num")
  
  dflist_plot[[n]] <- temp_df
  
  print(paste(n, "/10", " trials completed.", sep = ""))
  
}

plot(sim)

