## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
#loading packages we will need today:
library(ape)
library(evolved)

## -----------------------------------------------------------------------------
data("cytOxidase")

## ---- results = F-------------------------------------------------------------
cytOxidase["human"]

## -----------------------------------------------------------------------------
length(cytOxidase)

## -----------------------------------------------------------------------------
names(cytOxidase)

## -----------------------------------------------------------------------------
unlist(lapply(cytOxidase, nchar))

## -----------------------------------------------------------------------------
cytOxidase["platypus"]

## -----------------------------------------------------------------------------
countSeqDiffs(cytOxidase, "human", "alligator")  

## -----------------------------------------------------------------------------
nchar(cytOxidase["platypus"])

## -----------------------------------------------------------------------------
seq_diff_vec <- c(4, 8, 3, 6, 0) # note that these numbers are completely made up
names(seq_diff_vec) <- c("snake-chimp", "human-echinoderm", "alligator-insect", 
                         "chimp-echinoderm", "human-chimp")

seq_diff_vec

## -----------------------------------------------------------------------------
seq_diff_vec["snake-chimp"]

## -----------------------------------------------------------------------------
m <- matrix(data = NA,  # placeholder value for each matrix's cell will be NA 
                   # (later will be replaced by real data)
       nrow = 3,  # one row for each taxon group
       ncol = 3,  # one col for each taxon group
       dimnames = list(c("snake", "human", "insect"), # these are the row names
                       c("snake", "human", "insect")) # these are the column names
)

## -----------------------------------------------------------------------------
m

## -----------------------------------------------------------------------------
m[1,] <- c(0, 5, 2) # replacing the entire first row with fake numbers
m[2,] <- c(5, 0, 80) # replacing the entire second row with fake numbers
m[3,] <- c(2, 80, 0) # repalcing the entire third row with fake numbers
m

## ---- echo=FALSE--------------------------------------------------------------
animal_phy <- ape::read.tree(text=paste0("(cnidaria:1,((((bryozoa:1,(mollusk:1,annelid:1):1):1,(crustacea:1,insect:1):1):1,(echinoderm:1,(lamprey:1,(shark:1,(fish:1,(frog:1,((platypus:1,(human:1,chimpanzee:1):1):1,(snake:1,(alligator:1,bird:1):1):1):1):1):1):1):1):1):1):1);"))

plot.phylo(animal_phy)

# We can also add node labels to facilitate our discussion:
ape::nodelabels(cex=.65, bg = "lightgreen")

## -----------------------------------------------------------------------------
# For instance, what is the probability of observing 10 substitutions in a 37-long 
# AA sequence after 10 million years of divergence
# if the substitution rate is equal to 0.1 substitutions
# per site per million years?

rate_A <- 0.1
AA_seq_length <- 37

obs_subs <- 10  # amount of substitutions that have occurred
obs_time <- 10  # amount of time that has passed

prob_given_rate_A <- dpois(
  x = obs_subs, lambda = obs_time * rate_A * AA_seq_length)

prob_given_rate_A # this is the likelihood

# finally, we take the (natural) log, to get
# the log-likelihood of observing the data
# given the model and the rate value:
log(prob_given_rate_A)

# the questions is, then: what is the likelihood of observing >the same< data 
# under different values of substitution rate?

