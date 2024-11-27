## ----include=FALSE------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(evolved)

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
nsims = 1000
sims <- OneGenHWSim(n.ind = 57, p = .2, n.sim = nsims)

boxplot(sims, frame.plot=F,
        ylab="Number of individuals", xlab="Genotype")
for(i in 1:nrow(sims)){
  lines(x=1:3, y=sims[i, ], col="#A020F064")  
}

## -----------------------------------------------------------------------------
head(sims)

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
drifting = WFDriftSim(Ne = 10, n.gen = 100, p0 = .5, n.sim = 1000, 
                        print.data = T, plot.type = "static", knitr = TRUE)

#View the first 10 generation of a few simulations.
drifting[1:6, 1:11]

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
fixchecker <- function(x){
  res = suppressWarnings(
      min(which(x %in% 0:1), na.rm = T)
    )
    return(res)  
}

hist(x = apply(X = drifting, MARGIN = 1, fixchecker),
     main="Time until fixation", xlab = "Fixation time")

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
selected = NatSelSim(w11 = .2, w12 = .3, w22 = .1,
            n.gen = 20, print.data = T, plot.type = "static", knitr= TRUE)

## -----------------------------------------------------------------------------
# a numeric tolerance to accept some frequency as close enough to equilibrium
tol = 0.0001 

aux = abs(apply(selected, 2, diff))

# answer (in number of generations)
apply(aux>tol, 2, which.min)

