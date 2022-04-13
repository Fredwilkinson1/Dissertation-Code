###The below code was used to do UK analysis and create graphs.It was then re-run with variables and names changed to adapt for SE analysis. ###

#### Grid point code #################
#Loading libraries and reading in the data
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data")
library("ncdf4")
library("raster")
nc.pre <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc")
#Bringing in the relevant data and naming as precip
precip <- brick("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/cru_ts4.05.1901.2020.pre.dat.nc", varname="pre")
# Now setting up the first entry of our table ready to merge the rest to
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) # the date here will need to be changed with each new file
# UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] >-12 & PrecipStartDate[,"x"] <4 & PrecipStartDate[,"y"] > 48 & PrecipStartDate[,"y"] <64,] # this is the UK limits

#Testing for 1 year
head(PrecipStartDate)
UKish <- PrecipStartDate[PrecipStartDate[,1] > -10 & 
                           PrecipStartDate[,1] < 3 &
                           PrecipStartDate[,2] > 50 &
                           PrecipStartDate[,2] < 59,]
plot(UKish[,1], UKish[,2], pch = 20)
nrow(UKish)
length(UKish[1,])
SE <- PrecipStartDate[PrecipStartDate[,1] > 0.5 & 
                           PrecipStartDate[,1] < 2 &
                           PrecipStartDate[,2] > 51 &
                           PrecipStartDate[,2] < 53,]
NW <- PrecipStartDate[PrecipStartDate[,1] > -4 & 
                           PrecipStartDate[,1] < -2.5 &
                           PrecipStartDate[,2] > 54 &
                           PrecipStartDate[,2] < 56,]
plot(NW[,1], NW[,2], pch = 20)
View(NW)
NW2 <-NW[c(1:6, 8:9, 11:12),]
plot(UKish[,1], UKish[,2], pch = 20)
points(SE[,1], SE[,2], pch = 20, col = "Red")
points(NW2[,1], NW2[,2], pch = 20, col = "Blue")
nrow(UKish) # the number of points in this table
View(UKish) #view all the points
#######################



###################################
########## Precip ####################
##################################
#Loading libraries and reading in the data
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data")
library("ncdf4")
library("raster")
nc.pre <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc")
#Bringing in the relevant data and naming as precip
precip <- brick("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/cru_ts4.05.1901.2020.pre.dat.nc", varname="pre")
# Now setting up the first entry of our table ready to merge the rest to
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) # the date here will need to be changed with each new file
# UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] >-12 & PrecipStartDate[,"x"] <4 & PrecipStartDate[,"y"] > 48 & PrecipStartDate[,"y"] <64,] # this is the UK limits

#Testing for 1 year 
head(PrecipStartDate)
UKish <- PrecipStartDate[PrecipStartDate[,1] > -12 & 
                                   PrecipStartDate[,1] < 4 &
                                   PrecipStartDate[,2] > 48 &
                                   PrecipStartDate[,2] < 64,]

plot(UKish[,1], UKish[,2], pch = 20)
nrow(UKish) # the number of points in this table
View(UKish) #view all the points
UKish <- UKish[c(-1,-2),] # to remove rows 1 and 2 to narrow down initial square
head(UKish)

plot(PrecipStartDate[,1], PrecipStartDate[,2], pch = 20)

# Gathering all years
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) 
UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] >0.5 & PrecipStartDate[,"x"] <2 & PrecipStartDate[,"y"] > 51 & PrecipStartDate[,"y"] <53,] # this is the UK limits

print("Precip")
for (i in 2:length(names(precip))){
  if (i %% 100 == 0){
    print(paste("Number", i, "out of", length(names(precip))))
  }
  precip2 <- names(precip)[i]
  PrecipStartDate <- rasterToPoints(precip[[precip2]])
  precipUK <- PrecipStartDate[PrecipStartDate[,"x"] >0.5 & PrecipStartDate[,"x"] <2 & PrecipStartDate[,"y"] > 51 & PrecipStartDate[,"y"] <53,] # have again limtied to UK here
  UK_1901_2020 <- merge(UK_1901_2020, precipUK)
  colnames(UK_1901_2020)[i+2] <- precip2
}

head(UK_1901_2020)
View(UK_1901_2020[,1:4])

###################
###### PET ########
#############################
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Potential Evapotranspiration Data")
library("ncdf4")
library("raster")
nc.pre <- nc_open("cru_ts4.05.1901.2020.pet.dat.nc")
# This brings in the relevant file
Pet <- brick("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Potential Evapotranspiration Data/cru_ts4.05.1901.2020.pet.dat.nc", varname="pet")
# We now set up the first entry of our table ready to merge the rest to
PetStartDate <- rasterToPoints(Pet$X1901.01.16) # the date here will need to be changed with each new file

## Grid check
UKish <- PetStartDate[PetStartDate[,1] > 0.5 & 
                        PetStartDate[,1] < 2 &
                        PetStartDate[,2] > 51 &
                        PetStartDate[,2] < 53,]

plot(UKish[,1], UKish[,2], pch = 20)
nrow(UKish) # the number of points in this table
View(UKish) #view all the points
###


UK_Pet_1901_2020 <- PetStartDate[PetStartDate[,"x"] >0.5 & PetStartDate[,"x"] <2 & PetStartDate[,"y"] > 51 & PetStartDate[,"y"] <53,] # this is the UK limits

print("PET")
for (i in 2:length(names(Pet))){
  if (i %% 100 == 0){
    print(paste("Number", i, "out of", length(names(Pet))))
  }
  Pet2 <- names(Pet)[i]
  PetStartDate <- rasterToPoints(Pet[[Pet2]])
  Pet_UK <- PetStartDate[PetStartDate[,"x"] >0.5 & PetStartDate[,"x"] <2 & PetStartDate[,"y"] > 51 & PetStartDate[,"y"] <53,] # have again limtied to UK here
  UK_Pet_1901_2020 <- merge(UK_Pet_1901_2020, Pet_UK)
  colnames(UK_Pet_1901_2020)[i+2] <- Pet2
}
head(UK_Pet_1901_2020)
View(UK_Pet_1901_2020[, 1:4])


############
### Doing the caclulation below here using UK_1901_2020 as the Precip and UK_Pet_1901_2020 as PET
################
dim(UK_1901_2020)
dim(UK_Pet_1901_2020)

UK_WaterBal_1901_2020 <- UK_1901_2020 - UK_Pet_1901_2020


#### Saving these somewhere ####
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Southeast")
### RAW tables, no averaging ###
## Precip 
# write.csv(UK_1901_2020, file="SE_1901-2020-UK-Precip.csv")
## PET 
# write.csv(UK_Pet_1901_2020, file="SE_1901-2020-UK-Pet.csv")
## WaterBal
# write.csv(UK_WaterBal_1901_2020, file="SE_1901-2020-UK-WaterBal.csv")

### Averaged for the region ###
# Removing the first and second column which are the x & y coords as we average over each column i.e. for the whole UK
UK_1901_2020_pre_means <- colMeans(UK_1901_2020)[c(-1,-2)] 
UK_1901_2020_pet_means <- colMeans(UK_Pet_1901_2020)[c(-1,-2)]
UK_1901_2020_waterbal_means <- colMeans(UK_WaterBal_1901_2020)[c(-1,-2)]
# plot(UK_1901_2020_pre_means, type ="l")

## Precip 
write.csv(UK_1901_2020_pre_means, file="SE_1901-2020-UK-Precip_MEANS.csv")
# UK_1901_2020_pre_means <- read.csv("SE_1901-2020-UK-Precip_MEANS.csv")
## PET 
write.csv(UK_1901_2020_pet_means, file="SE_1901-2020-UK-Pet_MEANS.csv")
## WaterBal
write.csv(UK_1901_2020_waterbal_means, file="SE_1901-2020-UK-WaterBals_MEANS.csv")

########################################
########SPEI for SE WaterBal #######
########################################
library(SPEI)
spei3_SE <- spei(UK_1901_2020_waterbal_means, 24, distribution = "log-Logistic")
spei3_SE
plot.spei(spei3_SE, main = "SE England SPEI Plot for 24-month Accumulation Period", xlab = "Month")



##### Time Series Graphs ######
#Time series lag removal
ts_test_waterbal <- ts(UK_1901_2020_waterbal_means, frequency=12, start=c(1901))
plot.ts(ts_test_waterbal)
ts_test_waterbal_components <- decompose(ts_test_waterbal)
ts_test_waterbal_components$seasonal
plot(ts_test_waterbal_components) # the trend line here is a trend minus seasonal variation 

ts_test_pre_means <- ts(UK_1901_2020_pre_means, frequency=12, start=c(1901))
plot.ts(ts_test_pre_means)
ts_test_pre_means_components <- decompose(ts_test_pre_means)
ts_test_pre_means_components$seasonal
plot(ts_test_pre_means_components) # the trend line here is a trend minus seasonal variation 

ts_test_pet_means <- ts(UK_1901_2020_pet_means, frequency=12, start=c(1901))
plot.ts(ts_test_pet_means)
ts_test_pet_means_components <- decompose(ts_test_pet_means)
ts_test_pet_means_components$seasonal
plot(ts_test_pet_means_components) # the trend line here is a trend minus seasonal variation 



### Rough drought assignment test ### not used in final analysis ###
spei3_vect <- as.numeric(spei3$fitted)
dro <- ifelse(spei3_vect > 2, "vw", 
              ifelse(spei3_vect > 0, "w", 
                     ifelse(spei3_vect > -2, "d", "vd")))
table(dro)
dro_tab <- matrix(dro, ncol = 12, byrow = TRUE)
colnames(dro_tab) <- c("J", "F", "M", "A", "Ma", "Ju", "Ju", "Au", "S", "O", "N", "D")
rownames(dro_tab) <- 1901:2020

View(dro_tab)

# ## dplyr pipeline
# library(dplyr)
# dro_datfram <- data.frame(dro_tab)
# filter(dro_tab, J == "vd")

## Heatmap testing ### Again, not used in final analysis ###
spei3_mat <- matrix(na.omit(spei3_vect)[-1], ncol = 12, byrow = TRUE)
colnames(spei3_mat) <- c("J", "F", "M", "A", "Ma", "Ju", "Ju", "Au", "S", "O", "N", "D")
rownames(spei3_mat) <- 1903:2020
heatmap(spei3_mat)
  
  
