# Increasing Immunity Gene
# 
# This expands upon the increasing immunity model, where people gained
# additional resistance to infection (represented as a decreasing 
# probability of infection) upon each successful recovery from a disease.
# This model represents this as a gene contained within a specific part
# of the population, such that only some people have this increasing 
# resistance.

num.m1 <- num.m2 <- 500
nw <- network::network.initialize(num.m1 + num.m2, bipartite = num.m1,
                                  directed = FALSE)

est <- netest(nw, 
              formation = ~ edges + b1degree(0:1) + b2degree(0:1),
              target.stats = c(365, 240, 175, 240, 175),
              coef.diss = dissolution_coefs(~ offset(edges), 25))

## Epidemic model parameterization
param <- param.net(si.prob = 0.25, act.rate = 2, is.rate = 0.01)
init <- init.net(i.num = 10, i.num.g2 = 10)
control <- control.net(nsteps = 500, nsims = 5, immunity.FUN = immunity,
                       infection.FUN = si.infection,
                       recovery.FUN = is.recovery, skip.check = TRUE, 
                       resimulate.network = FALSE, verbose.int = 0)

## Simulate the epidemic model
sim <- netsim(est, param, init, control)

## Plot the results
par(mfrow = c(1, 1))
plot(sim, y = c("s.num", "i.num"),
     mean.col = 1:2, qnts = 1, qnts.col = 1:2, legend = TRUE)

