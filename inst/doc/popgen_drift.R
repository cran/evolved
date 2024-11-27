## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(evolved)
library(plot3D)

# Let's also store our par() configs so 
# we can restore them whenever we change it in this tutorial
oldpar <- par(no.readonly = TRUE)  

## -----------------------------------------------------------------------------
N <- 32 #population size
n_alleles <- 2*N 
p_gen0 <- 0.25 #Frequency of allele A1 in the first gen
p_gen1 <- rbinom(1, n_alleles, p_gen0) / n_alleles
p_gen1

## -----------------------------------------------------------------------------
p_gen2 <- rbinom(1, n_alleles, p_gen1) / n_alleles
p_gen2

## -----------------------------------------------------------------------------
p_gen3 <- rbinom(1, n_alleles, p_gen2) / n_alleles
p_gen3

## -----------------------------------------------------------------------------
p_gen4 <- rbinom(1, n_alleles, p_gen3) / n_alleles
p_gen4

## -----------------------------------------------------------------------------
p_gen5 <- rbinom(1, n_alleles, p_gen4) / n_alleles
p_gen5

## -----------------------------------------------------------------------------
generations <- seq(from = 0, to = 5, by = 1)
p_through_time <- c(p_gen0, p_gen1, p_gen2, p_gen3, p_gen4, p_gen5)
plot(generations, p_through_time, type="l", lwd = 2, col = "darkorchid3",
     ylab = "p", xlab = "generations", las = 1)

## -----------------------------------------------------------------------------
#We will simulate five flasks through 10 generations:
WFDriftSim(Ne = 32, n.gen = 10, p0 = 0.5, n.sim = 5)

## -----------------------------------------------------------------------------
# R will not return anything in the console when you run this, but 
# after running you should have this function in your R session
# as an object. Note that if you name another object as "isFixed"
# the function will be overwritten.

isFixed <- function(p, tol = 0.00000000001){
	
	if (p <= tol | p >= (1 - tol)){
		return(TRUE)
	} else{
		return(FALSE)
	}

}

# Assuming your current allele frequency is stored in the p_t object, the
# line below uses the function created above to test if p_t is equal to 
# zero OR equal to one

## -----------------------------------------------------------------------------
# The function will return `TRUE` if p_t is equal to zero or one.

## -----------------------------------------------------------------------------
WFDriftSim(Ne = 32, n.gen = 10, p0 = 0.5, n.sim = 100)

## ---- echo=F------------------------------------------------------------------
knitr::include_graphics('Buri_1956_fig6_wout_axes.png')

## -----------------------------------------------------------------------------
data = WFDriftSim(Ne = 8, n.gen = 50, p0 = 0.5, n.sim = 50,
                  print.data = T, plot.type = "none")

#then, if we want to see the 2nd to 5th generations of 
# the simulations number 14, 17 and 18, we type:
sims <- c(14,17,18)
gens <- 3:6
subset_data <- data[sims, gens]
subset_data

#and we can also plot those results:

#first opening an empty plot:
plot(NA, ylab = "Alleleic frequency", xlab = "Generation",
     xlim = c(2, 5), ylim = c(0, 1))

#creating a set of colors to paint our lines
cols <- rainbow(n = nrow(subset_data))

#then we finally add the lines to our plot:
for (i in 1:nrow(subset_data)) {
  lines(x = gens - 1, y = subset_data[i,], col = cols[i])
}

## ---- echo=FALSE--------------------------------------------------------------
pops <- c(rep(100000, times= 5), 50, rep(1000, times= 4))
CS <- log(pops, base=10)
time <- 1:10
plot(x=time, y=CS, xaxt="n", yaxt="n", ylim=c(1,5), ylab="Census size (individuals)", xlab="Time since study started (generations)")
lines(x=time, y=CS)

#x ticks:
xtick<-seq(1, 10, by=1)
axis(side=1, at=xtick, labels = TRUE)

#y ticks:
ytick<-c(1,log(50, 10),3,5)
axis(side=2, at=ytick, labels = c("1", "50", "1000", "100000"))

## ---- fig.width=10------------------------------------------------------------
# A simple genetic drift simulator as a Markov process
N       <- 16
popsize <- 2*N

# Now we define a transition matrix, such that:
# Each element of the transition matrix is a transition probability
#   Specifically: element Pi,j is the probability of going from
#   j alleles to i alleles in a single sampling step 
#  

tmat <- matrix(0, nrow = popsize + 1, ncol = popsize + 1) # along rows: current allele count; along cols: future allele count

# Fill in elements with probabilities from the binomial distribution
# If you have j alleles in generation t-1, then this defines the probability
# of sampling (0,1....2N) alleles in the next generation, under a binomial 
# sampling process:

# Note also that first column and row are absorbing states of 0

# Here is the vector of frequencies:
probvec <- (1:popsize) / popsize  

# What is the probability of getting to 0 allele copies, given 
# that you have 1? This is P01, and the transition probability 
# would be computed using 1/2N (=1/32) as the sampling probability 
# for the allele. 
# Here, this is computed using the function dbinom, which is a shortcut 
#   to the analytical binomial probability:
dbinom(1, popsize, prob = 1/32)

# Now we will set up the full matrix by iterating this over rows:

for (ii in 1:nrow(tmat)){
	tmat[ii, 2:ncol(tmat)] <- dbinom(ii - 1, size = popsize, prob = probvec)
}

# Note: we ignore the first column and let almost all elements equal zero, 
# because the probability of sampling k alleles given that you currently have zero
# is zero everywhere except the special case of P00, which is 1 
# (if you have zero now, you will have zero in the next gen with probability 1)
# To address this, we manually set element P00 equal to 1:
tmat[1,1] <- 1

#Now, all the columns should sum to 1:
colSums(tmat)

# Pt <- tmat^2*P0 

# Initial vector of population frequencies:
# Like Buri, suppose we do an experiment with 100 populations
# each with equal allele frequencies (p = q = 0.5) initially.
p_init <- rep(0, popsize + 1)

# note, in this indexing:
#   p_init[1] is the number of populations with 0 alleles
#   p_init[2] is the number with 1 allele
#   p_init[popsize + 1] is the number with 2N alleles

# we want the initial population to have 2N/2 = N p alleles (for 50:50 proportion of alleles)
#  so we fill in the N+1 element of p_init vector with the 
#  number of populations

p_init[N+1] <- 100

## ---- echo = T, eval = F------------------------------------------------------
#  #Now, we will introduce drift in our simulation
#  # after 1 generation of random mating in Wright-Fisher (WF) population,
#  #  the distribution of populations is:
#  tmat %*% p_init
#  
#  # and after 2 generations, it is:
#  tmat %*% (tmat %*% p_init)
#  
#  # and after 3 generations, it is:
#  tmat %*% (tmat %*% (tmat %*% p_init))

## -----------------------------------------------------------------------------
# This is easy enough to iterate.
# Here, we will make a matrix to hold 
# the results, for up to ngen generations

ngen    <- 20
freqmat <- matrix(NA, nrow=popsize+1, ncol=ngen)

curr_pop <- p_init 

for (ii in 1:ngen){
	
	# recompute the new pop frequency distribution
	curr_pop <- tmat %*% curr_pop
	# store it:
	freqmat[,ii] <- curr_pop
}



plot3D::persp3D(x = c(0, probvec), z=freqmat, theta=45, phi=20, contour =F,
                xlab="Allele frequency", zlab="Number of populations",
                ylab="Generations")
 

# Plotting selected generations:
########## 
plot.new()
par(oma=c(1,1,1,1), mfrow=c(2,2), mar=c(2,2,2,0))

pfx <- function(gen){
	plot.new()
	plot.window(xlim=c(0,33), ylim=c(0, 15))
	axis(1, at=seq(-4, 36, by=4))
	axis(2, at=seq(-2, 16, by=2), las=1)
	lines(x=c(16,16), y=c(0, 18), lwd=5, col="gray60")
	title(main = paste("After", gen, ifelse(gen == 1, "gen", "gens")))
}

# Expectation after 1 generation of drift:
pfx(1)
points(x=0:32, y=freqmat[,1], pch=21, bg="red", cex=1.5)

# Expectation after 5 generations of drift
pfx(5)
points(x=0:32, y=freqmat[,5], pch=21, bg="red", cex=1.5)

# Expectation after 10 generations of drift
pfx(10)
points(x=0:32, y=freqmat[,10], pch=21, bg="red", cex=1.5)

# Expectation after 15 generations of drift
pfx(15)
points(x=0:32, y=freqmat[,15], pch=21, bg="red", cex=1.5)

# Restoring old par() configs:
par(oldpar)

