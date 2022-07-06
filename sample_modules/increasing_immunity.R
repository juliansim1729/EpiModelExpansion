# Increasing Immunity SI Model
# 
# This disease follows a SI pattern - people can either be susceptible or
# infected. However, in contrast to the traditional SI model, people gain
# additional immunity to the disease with each successful recovery from
# the disease. 
# 
# The modeling of arrivals, departures, and deaths are left for  future work.

library("EpiModel")
library("ndtv")

# Increasing Immunity Module
immunity <- function(dat, at) {
    if (at == 2) {
        n <- sum(dat$attr$active == 1)
        # some people are naturally more resistant to this disease
        dat$attr$immunity <- rpois(n, 0.5) + 1
        print(dat$attr$immunity)
    } else {
        dat$attr$immunity <- dat$attr$immunity
    }
    
    if (at == 2) {
        dat$epi$meanImmunity <- c(NA_real_, mean(dat$attr$immunity, na.rm = TRUE))
    } else {
        dat$epi$meanImmunity[at] <- mean(dat$attr$immunity, na.rm = TRUE)
    }
    return(dat)
}

# Infection Module: S -> I
si.infection <- function(dat, at) {
    active <- dat$attr$active
    status <- dat$attr$status
    immunity <- dat$attr$immunity
    
    nw <- dat$nw
    
    ids.si <- which(active == 1 & status == "i")
    nActive <- sum(active == 1)
    
    nElig.si <- length(ids.si)
    n.si <- 0
    
    if (nElig.si > 0 && nElig.si < nActive) {
        del <- discord_edgelist(dat, at)
        del <- attr_edgelist(dat, at, del, susattr = "immunity")
        if (!(is.null(del))) {
            del$transProb <- dat$param$si.prob
            del$actRate <- dat$param$act.rate
            del$adjProb <- 1 - (1 - del$transProb)^del$actRate
            del$finalProb <- del$adjProb^(del$sus.immunity/2)
            
            transmit <- rbinom(nrow(del), 1, del$finalProb)
            del <- del[which(transmit == 1), ]
            idsNewInf <- unique(del$sus)
            n.si <- length(idsNewInf)
            if (n.si > 0) {
                dat$attr$status[idsNewInf] <- "i"
                dat$attr$infTime[idsNewInf] <- at
            }
        }
    }
    
    if (at == 2) {
        dat$epi$si.flow <- c(0, n.si)
    }
    else {
        dat$epi$si.flow[at] <- n.si
    }
    dat$nw <- nw
    return(dat)
}

# Recovery Module: I -> S
is.recovery <- function(dat, at) {
    active <- dat$attr$active
    status <- dat$attr$status
    immunity <- dat$attr$immunity
    
    is.rate <- dat$param$is.rate
    
    n.is <- 0
    idsElig.is <- which(active == 1 & status == "i")
    nElig.is <- length(idsElig.is)
    
    if (nElig.is > 0) {
        vec.is <- which(rbinom(nElig.is, 1, is.rate) == 1)
        if (length(vec.is) > 0) {
            ids.is <- idsElig.is[vec.is]
            n.is <- length(ids.is)
            status[ids.is] <- "s"
            immunity[ids.is] <- immunity[ids.is] * 1.25
        }
    }
    
    dat$attr$status <- status
    dat$attr$immunity <- immunity
    
    if (at == 2) {
        dat$epi$is.flow <- c(0, n.is)
        
        dat$epi$s.num <- c(0, sum(active == 1 & status == "s"))
        dat$epi$i.num <- c(0, sum(active == 1 & status == "i"))
    } else {
        dat$epi$is.flow[at] <- n.is
        
        dat$epi$s.num[at] <- sum(active == 1 & status == "s")
        dat$epi$i.num[at] <- sum(active == 1 & status == "i")
    }
    
    return(dat)
}

# Network Simulation

## Initialize the network and estimate the ERGM
nw <- network::network.initialize(500, directed = FALSE)
est <- netest(nw, formation = ~ edges, target.stats = 150,
               coef.diss = dissolution_coefs(~ offset(edges), 10))

## Epidemic model parameterization
param <- param.net(si.prob = 0.25, act.rate = 2, is.rate = 0.01)
init <- init.net(i.num = 10)
control <- control.net(nsteps = 100, nsims = 5, immunity.FUN = immunity,
                       infection.FUN = si.infection,
                       recovery.FUN = is.recovery, skip.check = TRUE, 
                       resimulate.network = FALSE, verbose.int = 0)

## Simulate the epidemic model
sim <- netsim(est, param, init, control)

## Plot the results
par(mfrow = c(1, 1))
plot(sim, y = c("s.num", "i.num"),
     mean.col = 1:2, qnts = 1, qnts.col = 1:2, legend = TRUE)

