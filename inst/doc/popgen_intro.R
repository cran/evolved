## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(evolved)

## -----------------------------------------------------------------------------
# Define how many samples we want:
rr <- 10000

## -----------------------------------------------------------------------------
binom_nbrs <- rbinom(n = rr, size = 10, prob = 0.5)  # taking 10000 samples of 10

## -----------------------------------------------------------------------------
head(binom_nbrs, 10)

## -----------------------------------------------------------------------------
table(binom_nbrs)

## -----------------------------------------------------------------------------
plot(
  table(binom_nbrs),
  ylab="Frequency", xlab="Number of successes")

## -----------------------------------------------------------------------------
P_x3_s10_p0.5 <- dbinom(x = 3, size = 10, prob = 0.5) 
# x is the integer value we want to know the probability density of

P_x3_s10_p0.5

## -----------------------------------------------------------------------------
# comparing the stochastic realization of x = 3 with the expectation for x = 3
table(binom_nbrs)[4] / rr #why the division?

P_x3_s10_p0.5

## -----------------------------------------------------------------------------
plot(
  table(binom_nbrs)/rr, 
  ylab="Probability", xlab="Number of heads")

# Finally, we can mark the probability we are interested in
# by using a red dot in our plot:
points(x = 3, y = P_x3_s10_p0.5, col = "red", cex = 2)

## -----------------------------------------------------------------------------
popsize <- 10

## -----------------------------------------------------------------------------
time <- 0

## -----------------------------------------------------------------------------
R <- 1.05

## -----------------------------------------------------------------------------
tmax <- 100 # this is the number of generations

## -----------------------------------------------------------------------------
all_generations <- seq(from = 1, to = tmax, by = 1)

## -----------------------------------------------------------------------------
# For each generation, beginning in the generation = 1 and:
for(generation in all_generations){ 
  
  N_t <- popsize[length(popsize)] # by indexing 
  # the vector in this way (i.e. using "[length()]"),
  # we guarantee we are always taking the last value of 
  # this vector.
  
  N_t_plus_one <- N_t * R # this is the application of Euler's formula
  
  #Now, let's store the population size at that generation:
  popsize <- c(popsize, N_t_plus_one)
  
  #let's not forget to record the time that we are on:
  time <- c(time, generation) 
}

## -----------------------------------------------------------------------------
#
plot(NA, xlim=c(0,tmax), ylim=c(0,1500),
     xlab="time", ylab="Population size",
     main="Malthusian growth")
lines(x = time, y = popsize, col = "blue", lwd = 3)

## ---- fig.cap='A white mother Spirit bear and a black cub offspring; the father must have been black and the cub is a heterozygote for the coat color polymorphism (photo mod. from Hedrick and Ritland (2012)', fig.width=2.5, fig.height=2, echo = F----
knitr::include_graphics('spirit_bear.png')

## -----------------------------------------------------------------------------
sim_pops <- OneGenHWSim(n.ind = 100, n.sim = 5, p = 0.467)

#now, checking the result of our simulations:
sim_pops

## -----------------------------------------------------------------------------
# To remember how this works, let's imagine you want to 
# arbitrarily calculate (f(A1) + 3) / 4.5 for all your simulations. 
#You should run:

freqs_A1 <- sim_pops$A1A1 / (sim_pops$A1A1 + sim_pops$A1A2 + sim_pops$A2A2)

result <- (freqs_A1 + 3) / 4.5
result

# the above has no biological "meaning". 
# it is just to remind you how vectorized calculation works

