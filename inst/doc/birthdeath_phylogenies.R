## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(LSD)
library(ape)
library(BAMMtools)
library(evolved)

# Let's also store our par() configs so 
# we can restore them whenever we change it in this tutorial
oldpar <- par(no.readonly = TRUE)  

## -----------------------------------------------------------------------------
args(simulateTree)

## -----------------------------------------------------------------------------
# Set parameters for simulation
S <- 1
E <- 0

# Set seed so we all get same results
set.seed(1)

# First, generate tree with no extinction and 6 tips
tree_pb <- simulateTree(pars = c(S, E), max.taxa = 6, max.t = 10)

# Then we reset the seed. From now on, you will 
# have different results each time you run a phylogeny:
set.seed(NULL)

## -----------------------------------------------------------------------------
plot.phylo(tree_pb, show.tip.label=F)

# Add a timescale (in Mya, and zero is the time the simulation stopped):
axisPhylo()

## -----------------------------------------------------------------------------
# Generating tree with high relative extinction and 100 tips
tree_lowE <- simulateTree(pars=c(1, 0.05), max.taxa=100, max.t = Inf)

# And a pure birth tree, holding all arguments the same except with no extinction:
tree_pb <- simulateTree(pars=c(0.01, 0), max.taxa=100, max.t = Inf)

## -----------------------------------------------------------------------------
plot.new()
par(mfrow=c(1,2), mar=c(0,0,0,0))
plot.phylo(tree_pb, show.tip.label=F)
plot.phylo(tree_lowE, show.tip.label=F)

# Restoring old par() configs:
par(oldpar)

## -----------------------------------------------------------------------------
# plot lineage through time plots:
lttPlot(tree_pb, col = "blue", knitr = T)

lttPlot(tree_lowE, col = "red", knitr = T)

# or, in a single plot:

plot(x=c(0,1), y = c(log(2), log(100)), lwd=1, col="gray50", type = "l")
lttPlot(tree_pb, rel.time=T, add=T, col="blue", lwd=1.5, knitr = T)
lttPlot(tree_lowE, rel.time=T, add=T, col="red", lwd=1.5, knitr = T)

## -----------------------------------------------------------------------------
tree1 <- simulateTree(c(0.5, 0), max.taxa = 40, max.t = Inf)

tree2 <- simulateTree(c(1, 0), max.taxa = 40, max.t = Inf)

tree3 <- simulateTree(c(5, 0), max.taxa = 40, max.t = Inf)

matrix(c(estimateSpeciation(tree1), estimateSpeciation(tree2), estimateSpeciation(tree3),
         0.5, 1, 5), ncol = 2, dimnames = list(NULL, c("Estimated", "True")))

## -----------------------------------------------------------------------------
REPS <- 1000
x <- numeric(REPS)
res <- data.frame(true_S=x, true_E=x, est_S=x)

for(i in 1:REPS){

  # pick a speciation rate with any value between 0 and 2:
	sim_S <- runif(1, min = 0, max = 2)
 
  # in the pure birth, extinction rate is always zero
	sim_E <- 0
	
	# simulate tree:	
	tree_sim <- simulateTree(pars=c(sim_S, sim_E), 
	                         max.taxa=50, max.t = Inf)
 
	#fitting the model:
	fit <- estimateSpeciation(tree_sim)
	
	res$true_S[i] <- sim_S
	res$true_E[i] <- sim_E

	res$est_S[i] <- fit
}

## -----------------------------------------------------------------------------
heatscatter(res$true_S, res$est_S, 
            xlab = "Simulated speciation rate",
            ylab="Estimated speciation rate")

# Then,we add the 1:1 line  (i.e. a perfect estimation)
abline(a = 0, b = 1, lwd=2)

## -----------------------------------------------------------------------------
REPS <- 1000
x <- numeric(REPS)
res <- data.frame(true_S=x, true_E=x, est_S=x)

for(i in 1:REPS){

  # Pick a speciation rate with any value between 0 and 2:
	sim_S <- runif(1, min = 0, max = 2)
 
  # Now we will change the extinction fraction to always equal 0.05
	sim_E <- sim_S * 0.05
	
	# simulate tree:	
	tree_sim <- simulateTree(pars=c(sim_S, sim_E), 
	                         max.taxa=50, max.t = Inf)
 
	#fitting the model:
	fit <- estimateSpeciation(tree_sim)
	
	res$true_S[i] <- sim_S
	res$true_E[i] <- sim_E

	res$est_S[i] <- fit
}

## -----------------------------------------------------------------------------
heatscatter(res$true_S, res$est_S, 
            xlab = "Simulated speciation rate",
            ylab="Estimated speciation rate")

#Then,we add the 1:1 line  (i.e. a perfect estimation)
abline(a = 0, b = 1, lwd=2)

## -----------------------------------------------------------------------------
tree <- simulateTree(c(1, 0.95), max.taxa = 50, max.t = Inf)

# estimating:
fitCRBD(tree)

## ---- eval = FALSE------------------------------------------------------------
#  REPS <- 1000
#  x <- numeric(REPS)
#  res <- data.frame(true_S=x, true_E=x, est_S=x, est_E=x)
#  
#  for(i in 1:REPS){
#  
#    # pick a speciation rate with any value between 0 and 2:
#  	sim_S <- runif(1, min = 0, max = 2)
#  
#    # pick a relative extinction rate:
#  	rel_ex <- runif(1  , 0, 0.95)
#  
#    # calculate E from that extinction fraction
#  	sim_E <- sim_S * rel_ex
#  	
#  	# simulate tree:	
#  	tree_sim <- simulateTree(pars=c(sim_S, sim_E),
#  	                         max.taxa=50, max.t = 1000)
#  
#  	# fitting the model:
#  	fit <- fitCRBD(tree_sim)
#  	
#  	res$true_S[i] <- sim_S
#  	res$true_E[i] <- sim_E
#  
#  	res$est_S[i] <- fit["S"]
#  	res$est_E[i] <- fit["E"]
#  }

## ---- eval = FALSE------------------------------------------------------------
#  heatscatter(res$true_S, res$est_S,
#              xlab = "Simulated speciation rate",
#              ylab="Estimated speciation rate")
#  
#  # Then,we add the 1:1 line  (i.e. a perfect estimation)
#  abline(a = 0, b = 1, lwd=2)
#  
#  # Making a simple linear model:
#  S_lm <- lm(res$est_S~res$true_S)
#  
#  # And checking how much variation is explained
#  summary(S_lm)$r.squared

## ---- eval = FALSE------------------------------------------------------------
#  sim_K = res$true_E/res$true_S
#  est_K = res$est_E/res$est_S
#  
#  LSD::heatscatter(sim_K, est_K,
#                   xlab = "Simulated E/S",
#                   ylab="Estimated E/S")
#  
#  #making a simple linear model:
#  K_lm <- lm(est_K~sim_K)
#  summary(K_lm)$r.squared
#  
#  #Then,we add the 1:1 line  (i.e. a perfect estimation)
#  abline(a = 0, b = 1, lwd=2)

## -----------------------------------------------------------------------------
data("whale_phylo")

## ---- fig.height=10, fig.width=6----------------------------------------------
par(cex=0.6)
plotPaintedWhales(knitr = T)

# Restoring old par() configs:
par(oldpar)

## -----------------------------------------------------------------------------
data(data_whales)
head(data_whales)

## -----------------------------------------------------------------------------
plot(x=data_whales$log_mass, data_whales$S, pch = 16,
     xlab="Mass (log(g))", ylab="Speciation rate", col=data_whales$color)
legend(x="topright",legend=c("other odontocetes","Baleen whales",
      "Beaked whales","Belugas and narwhals",
      "Dolphins","Other mysticetes","Porpoises"),
    pch=16,pt.cex=1.5,pt.bg=c("black",
"#DF536B","#61D04F","#2297E6","#28E2E5",
"#CD0BBC","#F5C710"),bty="n")

# Then we construct a linear model:
whales_lm <- lm(data_whales$S~data_whales$log_mass)
summary(whales_lm)

abline(whales_lm, col="red")

## -----------------------------------------------------------------------------
data(whales)
data(events.whales)

## ---- fig.height=10, fig.width=7----------------------------------------------
x <- getEventData(whales, events.whales, burnin = 0.1)

par(mfrow=c(1,2)) #making a panel

plotPaintedWhales(ftype="off", direction="rightwards", mar=c(5.1,0,2.1,2.1), show.legend = TRUE, knitr = T)
plot.bammdata(x, lwd = 3, mar=c(5.1,0,2.1,2.1), direction = "leftwards")

# Restoring old par() configs:
par(oldpar)

