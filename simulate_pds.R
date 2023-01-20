## 0: Environment setup
library("ergm")
library("tergm")
library("EpiModel")
library("ndtv")

# Simulate Periods

data(faux.mesa.high)
nw <- faux.mesa.high

fmh_grades <- get.node.attr(faux.mesa.high, 'Grade')
fmh_schedule <- trad_classgen(fmh_grades, 12)

for (period in 1:9) {
  fmh_pdV <- paste0('pd', period)
  nw <- network::set.vertex.attribute(nw, fmh_pdV, fmh_schedule[, period + 1])
}

ergm_formula <- ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
  nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") + 
  nodematch("pd8")

sim_periods <- simulate(nw ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
                   nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") + 
                   nodematch("pd8"), coef = c(-50, 40, 40, 40, 40, 45, 45, 45, 40), nsim = 20)

summary(sim_periods)
save <- print(summary(sim_periods))
  



