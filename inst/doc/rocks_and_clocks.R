## ----include=FALSE------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(evolved)

# Let's also store our par() configs so 
# we can restore them whenever we change it in this tutorial
oldpar <- par(no.readonly = TRUE)  

## -----------------------------------------------------------------------------
data("dinos_fossil")
head(dinos_fossil)

## ----fig.height=5, fig.width=6, , fig.align='center'--------------------------
spDTT = calcFossilDivTT(dinos_fossil, tax.lvl = "species")
genusDTT = calcFossilDivTT(dinos_fossil, tax.lvl = "genus")
famDTT = calcFossilDivTT(dinos_fossil, tax.lvl = "family")

# And to allow comparisons, we will use relative richness:
plot(x=genusDTT$age, xlim = rev(range(genusDTT$age)),
     y=log(genusDTT$div)-log(max(genusDTT$div)), 
     xlab="Time (Million years ago)",
     ylab="Log relative diversity",
     type="l", col="blue", ylim=c(-7,0))

lines(x=famDTT$age,
     y=log(famDTT$div)-log(max(famDTT$div)), 
     col="red")

lines(x=spDTT$age,
     y=log(spDTT$div)-log(max(spDTT$div)), 
     col="black")

## ----fig.height=5, fig.width=6, , fig.align='center'--------------------------
# Family-level:
plotRawFossilOccs(dinos_fossil, tax.lvl = "family", knitr = TRUE)

# Genus level:
plotRawFossilOccs(dinos_fossil, tax.lvl = "genus", knitr = TRUE)

# Species level:
plotRawFossilOccs(dinos_fossil, tax.lvl = "species", knitr = TRUE)

## -----------------------------------------------------------------------------
colnames(dinos_fossil)

## -----------------------------------------------------------------------------
data("birds_spp")

## -----------------------------------------------------------------------------
sum_w_fossil = sum(birds_spp %in% dinos_fossil$species)

sum_w_fossil / length(birds_spp)

## -----------------------------------------------------------------------------
data("timeseries_fossil")
head(timeseries_fossil)

## ----fig.height=5, fig.width=6, , fig.align='center'--------------------------
clades = unique(timeseries_fossil$clade)[1:4]

cols= c("#ffd353", "#ef8737", "#bb292c", "#62205f")

par(mfrow=c(2,2))
for(i in 1:length(clades)){
  aux = timeseries_fossil[timeseries_fossil$clade==clades[i], ]
  plot(aux$time_ma, log(aux$richness), col=cols[i], lwd=3,
       main=clades[i], type="l", frame.plot = F,
       xlab="Time (Mya)", ylab="Log richness",
       xlim=rev(range(aux$time_ma)))
}

# Restoring old par() configs:
par(oldpar)

## -----------------------------------------------------------------------------
data(cytOxidase)

summary(cytOxidase)

head(cytOxidase)

## -----------------------------------------------------------------------------
countSeqDiffs(cytOxidase, "snake", "bird")

## -----------------------------------------------------------------------------
countSeqDiffs(cytOxidase, "snake", "bird")/nchar(cytOxidase["snake"])

## ----fig.height=5, fig.width=6, , fig.align='center'--------------------------
plotProteinSeq(cytOxidase, c("snake", "bird", "cnidaria"), knitr = TRUE)

