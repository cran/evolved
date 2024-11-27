## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("paleobioDB")

## -----------------------------------------------------------------------------
library(evolved)
library(paleobioDB)

## -----------------------------------------------------------------------------
data("dinos_fossil")

## -----------------------------------------------------------------------------
dim(dinos_fossil)

## -----------------------------------------------------------------------------
colnames(dinos_fossil) 

## -----------------------------------------------------------------------------
head(dinos_fossil, n = 5)

## -----------------------------------------------------------------------------
sum(dinos_fossil$genus == "Majungasaurus")

## -----------------------------------------------------------------------------
sum(dinos_fossil$species == "Tyrannosaurus rex")

## -----------------------------------------------------------------------------
Trex <- dinos_fossil[dinos_fossil$species == "Tyrannosaurus rex",]

#this still has the same columns as the dinosaur dataset:
colnames(Trex)

## -----------------------------------------------------------------------------
plotRawFossilOccs(Trex, use.midpoint =  F, knitr = T)

## -----------------------------------------------------------------------------
plotRawFossilOccs(Trex, use.midpoint = F, knitr = T)
segments(x0 = min(Trex$midpoint),
         x1 = max(Trex$midpoint),
         y0 = 35, y1 = 35, lwd=3, col="red"
          )

## -----------------------------------------------------------------------------
plotRawFossilOccs(dinos_fossil, tax.lvl = "species", knitr = T)

## -----------------------------------------------------------------------------
plotRawFossilOccs(dinos_fossil, tax.lvl  = "family", knitr = T)

## -----------------------------------------------------------------------------
plotRawFossilOccs(dinos_fossil, tax.lvl  = "order", knitr = T)

## -----------------------------------------------------------------------------
hist(dinos_fossil$midpoint,
     breaks=100, #increasing breaks to see data in a better precision
     xlab="Absolute time (Mya)", main = "Fossil Occurrence midpoints",
     
     #reversing axis to represent past -> present
     xlim=c(rev(range(dinos_fossil$midpoint))) 
     )

## ---- include=T---------------------------------------------------------------
# first we load the species list:
data("birds_spp")

# then we count the number of extant dinosaurs within our fossil dataset:
n_fossilized_dinos <- sum(birds_spp %in% dinos_fossil$species)

#then we calculate the proportion:
n_fossilized_dinos/length(birds_spp)

## -----------------------------------------------------------------------------
# This line calculates diversity through the standard method:
SM <- calcFossilDivTT(dinos_fossil, method = "stdmethod")

# and this one calculates diversity using the range-through method:
RG <- calcFossilDivTT(dinos_fossil, method = "rangethrough")


# Now we can create an empty plot to store our estimates:
plot(NA, 
     xlim=rev(range(c(RG$age, SM$age))),
     ylim=range(c(RG$div, SM$div)),
     xlab="Absolute time (Mya)",
     ylab="Diversity"
     )  

# and then add the standard method diversity:
lines(x=SM$age, y = SM$div, type = "l", lwd=2)  

# and finally add the range-through diversity:
lines(x=RG$age, y = RG$div, type = "l", col="blue", lwd=2)  

## -----------------------------------------------------------------------------
#Creating a plot:
plot(NA, 
     xlim=rev(range(c(RG$age, SM$age))),
     
     ylim=c(0,700), # here is the part of the code I changed, in
                    # comparison with the former chunk of code
     
     xlab="Absolute time (Mya)",
     ylab="Diversity"
     )  

# adding the standard method diversity:
lines(x=SM$age, y = SM$div, type = "l", lwd=2)  

# adding the range-through diversity:
lines(x=RG$age, y = RG$div, type = "l", col="blue", lwd=2) 

# Here, red line marks the K-Pg mass extinction:
abline(v=66, col="red")

## -----------------------------------------------------------------------------
# Plotting again our diversity curves (note
# this is exactly what we did in the previous section), but in log scale
plot(NA, 
     xlim=rev(range(c(RG$age, SM$age))),
     ylim=range(c(0, max(log(c(RG$div, SM$div), base = 2)))), # note the base 2
     xlab="Absolute time (Mya)",
     ylab="Diversity")  

# adding the standard method diversity:
lines(x=SM$age, 
      y = log(SM$div, base=2),
      type = "l", lwd=2)  
# adding the range-through diversity:
lines(x=RG$age, 
      y = log(RG$div, base = 2),
      type = "l", col="blue", lwd=2)

# Here, red line marks the K-Pg mass extinction:
abline(v=66, col="red")

## -----------------------------------------------------------------------------
# Plotting number of occurrences in space:

# you can change the size of the cells using the  argument "res"
pbdb_map_occur(dinos_fossil, res = 1, cex = 0.3)

## ---- fig.height=7, fig.width=10----------------------------------------------
pbdb_map_occur(Trex, res = 1, cex=0.7)

## ---- echo=F, fig.cap="Example on how fossil layers can be dated using zircon crystals. Note that percentages represent atomic fractions, not percentages by mass."----
knitr::include_graphics('zircon_dating.png')

