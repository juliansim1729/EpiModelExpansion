library(EpiModel)
library(ergm)
data(faux.mesa.high)

# Network Simulation

## Network Params

nw_size <- 205

# want to keep edges relatively low to minimize random chance edges, try to pull
# info straight from faux.mesa.high as with nm(gender, race, grade) and gwesp
ergm_formula <- ~ edges + nodematch("pd1") + nodematch("pd2") + nodematch("pd3") + 
  nodematch("pd4") + nodematch("pd5") + nodematch("pd6") + nodematch("pd7") + 
  nodematch("pd8")# + edgecov("friendships")

target.stats <- 0.6*c(20910, 1144, 1144, 1144, 3019, 2822, 2639, 1144, 1144)

# target.stats <- c(0.6*c(20910, 1144, 1144, 1144, 3019, 2822, 2639, 1144, 1144), 203)

coef.diss <- dissolution_coefs(~offset(edges), 200)

## Simulation Build

### Network Initialization
nw <- faux.mesa.high
friendships <- faux.mesa.high[,]
nw %n% "friendships" <- friendships

# nw <- network::set.vertex.attribute(nw, "sex", rep(c("M", "F"), each = ))

### ClassGen

fmh <- faux.mesa.high

fmh_grades <- get.node.attr(fmh, 'Grade')
fmh_schedule <- trad_classgen(fmh_grades, 12)

for (period in 1:9) {
  fmh_pdV <- paste0('pd', period)
  # why is this repeating past 205
  # nw %v% fmh_pdV <- fmh_schedule[period + 1] 
  nw <- network::set.vertex.attribute(nw, fmh_pdV, fmh_schedule[, period + 1])
}


### Network Estimation
est <- netest(nw, formation = ergm_formula, target.stats = target.stats, 
              coef.diss = coef.diss)
