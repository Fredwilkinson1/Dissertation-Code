#### Grid point code #################
#Loading libraries and reading in the data
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data")
library("ncdf4")
library("raster")
nc.pre <- nc_open("cru_ts4.05.1901.2020.pre.dat.nc")
#Bringing in the relevant data and naming as precip
precip <- brick("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/cru_ts4.05.1901.2020.pre.dat.nc", varname="pre")
# We now set up the first entry of our table ready to merge the rest to
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) # the date here will need to be changed with each new file
# UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] >-12 & PrecipStartDate[,"x"] <4 & PrecipStartDate[,"y"] > 48 & PrecipStartDate[,"y"] <64,] # this is the UK limits

head(PrecipStartDate)
NW <- PrecipStartDate[PrecipStartDate[,1] > -4 & 
                           PrecipStartDate[,1] < -2.5 &
                           PrecipStartDate[,2] > 54 &
                           PrecipStartDate[,2] < 56,]
NW <-NW[c(1:6, 8:9, 11:12),]
plot(NW[,1], NW[,2], pch = 20)
nrow(NW) # the number of points in this table
View(NW) #view all the points
NW <- NW[c(-1,-2),] # to remove rows 1 and 2 to narrow down initial square
head(NW)

names(precip)
names(precip)[1]

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
# We now set up the first entry of our table ready to merge the rest to
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) # the date here will need to be changed with each new file
# UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] >-12 & PrecipStartDate[,"x"] <4 & PrecipStartDate[,"y"] > 48 & PrecipStartDate[,"y"] <64,] # this is the UK limits

# Gathering all years
PrecipStartDate <- rasterToPoints(precip$X1901.01.16) # the date here will need to be changed with each new file
NW_UK_1901_2020 <- PrecipStartDate[PrecipStartDate[,"x"] > -4 & PrecipStartDate[,"x"] < -2.5 & PrecipStartDate[,"y"] > 54 & PrecipStartDate[,"y"] <56,] # this is the UK limits
NW_UK_1901_2020 <- NW_UK_1901_2020[c(1:8, 11:12),]

print("Precip")
for (i in 2:length(names(precip))){ 
  # print (i)
  if (i %% 100 == 0){
    print(paste("Number", i, "out of", length(names(precip))))
  }
  precip2 <- names(precip)[i]
  PrecipStartDate <- rasterToPoints(precip[[precip2]])
  precipUK <- PrecipStartDate[PrecipStartDate[,"x"] > -4 & PrecipStartDate[,"x"] < -2.5 & PrecipStartDate[,"y"] > 54 & PrecipStartDate[,"y"] <56,] # have again limtied to UK here
  precipUK <- precipUK[c(1:8, 11:12),]
  NW_UK_1901_2020 <- merge(NW_UK_1901_2020, precipUK)
  # colnames(UK_1901_2020)[i+2] <- precip2
}

head(NW_UK_1901_2020)
View(NW_UK_1901_2020[,1:4])



###################
###### PET ########
#############################
#Here's the good code
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Potential Evapotranspiration Data")
library("ncdf4")
library("raster")
nc.pre <- nc_open("cru_ts4.05.1901.2020.pet.dat.nc")
# This brings in the relevant file
Pet <- brick("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Potential Evapotranspiration Data/cru_ts4.05.1901.2020.pet.dat.nc", varname="pet")
# We now set up the first entry of our table ready to merge the rest to
PetStartDate <- rasterToPoints(Pet$X1901.01.16) # the date here will need to be changed with each new file

## Grid check
NWPET <- PetStartDate[PetStartDate[,1] > -4 & 
                        PetStartDate[,1] < -2.5 &
                        PetStartDate[,2] > 54 &
                        PetStartDate[,2] < 56,]
NWPET2 <-NWPET[c(1:6, 8:11),]
plot(NWPET2[,1], NWPET2[,2], pch = 20)
nrow(NWPET2) # the number of points in this table
View(NWPET2) #view all the points




NW_UK_Pet_1901_2020 <- PetStartDate[PetStartDate[,"x"] > -4 & PetStartDate[,"x"] < -2.5 & PetStartDate[,"y"] > 54 & PetStartDate[,"y"] <56,] # this is the UK limits
NW_UK_Pet_1901_2020 <-NW_UK_Pet_1901_2020[c(1:6, 8:11),]

print("PET")
for (i in 2:length(names(Pet))){
  if (i %% 100 == 0){
    print(paste("Number", i, "out of", length(names(Pet))))
  }
  Pet2 <- names(Pet)[i]
  PetStartDate <- rasterToPoints(Pet[[Pet2]])
  Pet_UK <- PetStartDate[PetStartDate[,"x"] > -4 & PetStartDate[,"x"] < -2.5 & PetStartDate[,"y"] > 54 & PetStartDate[,"y"] <56,] # have again limtied to UK here
  Pet_UK <- Pet_UK[c(1:6, 8:11),]
  NW_UK_Pet_1901_2020 <- merge(NW_UK_Pet_1901_2020, Pet_UK)
  colnames(NW_UK_Pet_1901_2020)[i+2] <- Pet2
}
head(NW_UK_Pet_1901_2020)
View(NW_UK_Pet_1901_2020[, 1:4])



############
### Do the caclulation below here using UK_1901_2020 as the Precip and UK_Pet_1901_2020 as PET
################
dim(NW_UK_1901_2020)
dim(NW_UK_Pet_1901_2020)

NW_UK_WaterBal_1901_2020 <- NW_UK_1901_2020 - NW_UK_Pet_1901_2020

View(NW_UK_1901_2020)
View(NW_UK_Pet_1901_2020)
#### Saving these somewhere ####
setwd("/Users/Fred/Documents/Year 3/Dissertation/Data/UK CRU Temp and Precip Data/Northwest")
### RAW tables, no averaging ###
## Precip 
write.csv(NW_UK_1901_2020, file="NW_1901-2020-UK-Precip.csv")
## PET 
write.csv(NW_UK_Pet_1901_2020, file="NW_1901-2020-UK-Pet.csv")
## WaterBal
write.csv(NW_UK_WaterBal_1901_2020, file="NW_1901-2020-UK-WaterBal.csv")

### Averaged for the region ###
# we remove the first and second column which are the x & y coords as we average over each column i.e. for the whole UK
NW_1901_2020_pre_means <- colMeans(NW_UK_1901_2020)[c(-1,-2)] 
NW_1901_2020_pet_means <- colMeans(NW_UK_Pet_1901_2020)[c(-1,-2)]
NW_1901_2020_waterbal_means <- colMeans(NW_UK_WaterBal_1901_2020)[c(-1,-2)]
# plot(UK_1901_2020_pre_means, type ="l")

## Precip 
write.csv(NW_1901_2020_pre_means, file="NW_1901-2020-UK-Precip_MEANS.csv")
## PET 
write.csv(NW_1901_2020_pet_means, file="NW_1901-2020-UK-Pet_MEANS.csv")
## WaterBal
write.csv(NW_1901_2020_waterbal_means, file="NW_1901-2020-UK-WaterBals_MEANS.csv")

########################################
########SPEI for SE WaterBal test #######
########################################
library(SPEI)
spei3_NW <- spei(NW_1901_2020_waterbal_means, 24, distribution = "log-Logistic")
spei3_NW
plot.spei(spei3_NW, main = "NW England SPEI Plot for 24-month Accumulation Period", xlab = "Month")


##### Time Series Graphs ######
#Time series lag removal
ts_test_waterbal <- ts(NW_1901_2020_waterbal_means, frequency=12, start=c(1901))
plot.ts(ts_test_waterbal)
ts_test_waterbal_components <- decompose(ts_test_waterbal)
ts_test_waterbal_components$seasonal
plot(ts_test_waterbal_components) # the trend line here is a trend minus seasonal variation 

ts_test_pre_means <- ts(NW_1901_2020_pre_means, frequency=12, start=c(1901))
plot.ts(ts_test_pre_means)
ts_test_pre_means_components <- decompose(ts_test_pre_means)
ts_test_pre_means_components$seasonal
plot(ts_test_pre_means_components) # the trend line here is a trend minus seasonal variation 

ts_test_pet_means <- ts(NW_1901_2020_pet_means, frequency=12, start=c(1901))
plot.ts(ts_test_pet_means)
ts_test_pet_means_components <- decompose(ts_test_pet_means)
ts_test_pet_means_components$seasonal
plot(ts_test_pet_means_components) # the trend line here is a trend minus seasonal variation 



### Rough drought assignment
spei3_NW_vect <- as.numeric(spei3_NW$fitted)
dro <- ifelse(spei3_NW_vect > 2, "vw", 
              ifelse(spei3_NW_vect > 0, "w", 
                     ifelse(spei3_NW_vect > -2, "d", "vd")))
table(dro)
dro_tab <- matrix(dro, ncol = 12, byrow = TRUE)
colnames(dro_tab) <- c("J", "F", "M", "A", "Ma", "Ju", "Ju", "Au", "S", "O", "N", "D")
rownames(dro_tab) <- 1901:2020

View(dro_tab)

