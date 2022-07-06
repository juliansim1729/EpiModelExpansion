library(EpiModel)

## Modified infection module
infect_ie <- function(dat, at) {
    
    active <- dat$attr$active
    status <- dat$attr$status
    nw <- dat$nw
    
    idsInf <- which(active == 1 & status == "i")
    nActive <- sum(active == 1)
    
    nElig <- length(idsInf)
    nInf <- 0
    
    if (nElig > 0 && nElig < nActive) {
        del <- discord_edgelist(dat, at)
        if (!(is.null(del))) {
            del$transProb <- dat$param$ie.prob
            del$actRate <- dat$param$act.rate
            del$finalProb <- 1 - (1 - del$transProb)^del$actRate
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

infect_ee <- function(dat, at) {
    
    active <- dat$attr$active
    status <- dat$attr$status
    nw <- dat$nw
    
    idsInf <- which(active == 1 & status == "e")
    nActive <- sum(active == 1)
    
    nElig <- length(idsInf)
    nInf <- 0
    
    if (nElig > 0 && nElig < nActive) {
        del <- discord_edgelist(dat, at)
        if (!(is.null(del))) {
            del$transProb <- dat$param$ee.prob
            del$actRate <- dat$param$act.rate
            del$finalProb <- 1 - (1 - del$transProb)^del$actRate
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



## New disease progression module
progress <- function(dat, at) {
    
    active <- dat$attr$active
    status <- dat$attr$status
    
    ei.rate <- dat$param$ei.rate
    ir.rate <- dat$param$ir.rate
    
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
    
    dat$attr$status <- status
    print(table(dat$attr$status))
    
    if (at == 2) {
        dat$epi$ei.flow <- c(0, nInf)
        dat$epi$ir.flow <- c(0, nRec)
        dat$epi$e.num <- c(0, sum(active == 1 & status == "e"))
        dat$epi$r.num <- c(0, sum(active == 1 & status == "r"))
    }
    else {
        dat$epi$ei.flow[at] <- nInf
        dat$epi$ir.flow[at] <- nRec
        dat$epi$e.num[at] <- sum(active == 1 & status == "e")
        dat$epi$r.num[at] <- sum(active == 1 & status == "r")
    }
    
    return(dat)
}

## Initialize the network and estimate the ERGM
nw <- network::network.initialize(500, directed = FALSE)
est4 <- netest(nw, formation = ~ edges, target.stats = 150,
               coef.diss = dissolution_coefs(~ offset(edges), 10))

## Epidemic model parameterization
param <- param.net(ie.prob = 0.0005, ee.prob = 0.05, act.rate = 2, ei.rate = 0.075, ir.rate = 0.005)
init <- init.net(i.num = 25)
control <- control.net(nsteps = 750, nsims = 5, infection.FUN = NULL, 
                       infection_ie.FUN = infect_ie,
                       infection_ee.FUN = infect_ee, progress.FUN = progress,
                       recovery.FUN = NULL, skip.check = TRUE, 
                       resimulate.network = FALSE, verbose.int = 0)

## Simulate the epidemic model
sim4 <- netsim(est4, param, init, control)

## Plot the results
par(mfrow = c(1, 1))
plot(sim4, y = c("s.num", "i.num", "e.num", "r.num"), 
     sim.lines = TRUE, sim.lwd = 1, sim.col = 1:4, sim.alpha = 0.7,
     mean.col = 1:4, mean.lwd = 2, 
     qnts = 1, qnts.col = 1:4, qnts.alpha = 0.2, legend = TRUE)

