## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
library(evolved)

## -----------------------------------------------------------------------------
simulateBirthDeathRich(t = 10, S = 1, E = 0)

## -----------------------------------------------------------------------------
data("timeseries_fossil")

## -----------------------------------------------------------------------------
unique(timeseries_fossil$clade)

## -----------------------------------------------------------------------------
# Here we will use dinosaurs:
dino_rich <- timeseries_fossil[timeseries_fossil$clade=="dinosauria",]
head(dino_rich)

## -----------------------------------------------------------------------------
unique(dino_rich$time_ma)

## -----------------------------------------------------------------------------
# We will use the early Jurassic, around 201 Mya, as our reference for this analysis
t_obs <- 201

rich_early_J <- dino_rich$richness[dino_rich$time_ma==t_obs] 
# species richness from this time point
rich_early_J

# plotting our data and our point of observation:
plot(x = dino_rich$time_ma,
     y=dino_rich$richness,
     xlim=c(max(dino_rich$time_ma), min(dino_rich$time_ma)),
     xlab = "Millions of years ago", ylab = "Dino species richness")

# connecting the dots:
lines(x = dino_rich$time_ma,
     y=dino_rich$richness)

# highlighting the point we will pick:
points(x=t_obs, y=rich_early_J, col="red", pch=16)
abline(v=t_obs, col="red")

## -----------------------------------------------------------------------------
# Richness at the stem age of the dinosaurs:
rich_0 <- 1

# The start age of our clade marks time 0 for diversification:
t0 = dino_rich$stem_age[1]

# Since we now know N_t, N_0, and the elapsed time, calculate R:
R_dino = (log(rich_early_J) - log(1)) / (t0 - t_obs)
R_dino

# Plot our data and project the richness since the beginning of our time series:
time_from_t0 = t0 - dino_rich$time_ma

projected_rich = rich_0 * exp(R_dino * time_from_t0)

# Finally, we calculate the difference between our projections and the data:
projected_rich-dino_rich$richness

# Plotting the difference between our projections and the data:
plot(x = dino_rich$time_ma,
     y=dino_rich$richness,
     xlim=c(max(dino_rich$time_ma), min(dino_rich$time_ma)),
     )
# Connecting the dots:
lines(x = dino_rich$time_ma,
     y=dino_rich$richness)

# Highlighting our point of observation again:
points(x=t_obs, y=rich_early_J, col="red", pch=16)

# Now, we will add the predicted richness curve based on our estimations:
lines(x = dino_rich$time_ma,
     y=projected_rich, col="red")

#Finally, we plot the differences between our projections and our data using bars:
segments(x0 = dino_rich$time_ma,
         x1 = dino_rich$time_ma,
         y0 = dino_rich$richness,
         y1 = projected_rich,
         col="red")

## -----------------------------------------------------------------------------
#we can plot the data again, this time with extra care with the scale of our axes:
plot(x = dino_rich$time_ma,
     y=dino_rich$richness,
     xlim=c(max(dino_rich$time_ma), min(dino_rich$time_ma)),
     
     #we will change the y-axis limits to fit all our calculations:
     ylim=c(
       min(c(dino_rich$richness, projected_rich)), 
       max(c(dino_rich$richness, projected_rich))),
     )

#connecting the dots:
lines(x = dino_rich$time_ma,
     y=dino_rich$richness)

#highlighting our observation point:
points(x=t_obs, y=rich_early_J, col="red", pch=16)

#adding the predictions of the model:
lines(x = dino_rich$time_ma,
     y=projected_rich, col="red")

#Finally, we plot the differences between our projections and our data using bars:
segments(x0 = dino_rich$time_ma,
         x1 = dino_rich$time_ma,
         y0 = dino_rich$richness,
         y1 = projected_rich,
         col="red")

## -----------------------------------------------------------------------------
perc_error <- (projected_rich - dino_rich$richness)/dino_rich$richness * 100

min(perc_error)
max(perc_error)

hist(perc_error, xlab = "Error as a percentage of the data")

## -----------------------------------------------------------------------------
proj_rich_dinos_KPg <- (R_dino +1)^(dino_rich$stem_age[1]-66)
proj_rich_dinos_KPg

