## ----include=FALSE------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------

## -----------------------------------------------------------------------------
library(evolved)
library(ape)

## -----------------------------------------------------------------------------
data("whale_phylo")

## ----fig.height=12, fig.width=6, fig.align='center', eval=FALSE---------------
#  plot(whale_phylo, cex = 0.6)
#  axisPhylo()

## ----fig.height=12, fig.width=6, fig.align='center'---------------------------
plotPaintedWhales(knitr = TRUE)

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
lttPlot(whale_phylo, knitr = T)

## -----------------------------------------------------------------------------
whalerate = estimateSpeciation(whale_phylo)

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
sims=vector()

for(i in 1:10000){
  sims = c(sims, simulateBirthDeathRich(S = whalerate, E=0, t = 36))
}

hist(sims, breaks = 100)

## ----fig.height=5, fig.width=6, fig.align='center'----------------------------
t = simulateTree(c(whalerate, 0), max.taxa = 40, max.t = Inf)
ape::ltt.plot(t, col="white", ylim=c(0,87))

for(i in 1:100){
  t = simulateTree(c(whalerate, 0), max.taxa = 40, max.t = Inf)
  ape::ltt.lines(t, col="#FF000032")  
}

#Now, we add the actual whale phylogeny:
ape::ltt.lines(whale_phylo, col="black")

