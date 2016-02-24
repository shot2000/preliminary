#spatial Regression analysis
setwd("H:/00-Dissertation/02_Preliminary_Research/Regression Analysis")
# read the spatial weight file
tazgal <- read.gal("TAZ_CarbEM4Regression0514.gal",override.id=TRUE)
attributes (tazgal)
# Creating a spatial weights list from a gal file
tazw <- nb2listw(tazgal)
