#Uploaded to Git "MPAs_warming" on April 10, 2017: https://github.com/johnfbruno/MPAs_warming.git
#new path to files in desktop Git folder: MPA_warming ms files for Git:  Dropbox/JB/Manuscripts/MPA_warming ms files for Git/MPAs_warming

############################
#### new code for MPA warming project for new large grain (not downscaled, N=Native) CMPI5 extract 
#creatd March 23, 2017 by JB, based on code snippets from CC
############################

library(raster)

setwd("~/Dropbox/MPAs_warming/Layers/CMIP5/updated_CMIP5_files") 

setwd("~/Dropbox/JB/Manuscripts/MPA_warming ms files for Git/MPAs_warming") #set to local Git folder

##########################
### Mean, Native 8.5 ####
##########################

#set up the model projections
source("revrotate..R") #make sure file is in pathway, or setwd() to the attached file
meanTrend8.5N<-raster("trend_yearmean_ensemble_tos_RCP85.nc")
extent(meanTrend8.5N)<-c(-180,180,-90,90) #need to reset for this layer, orginal orientation was 0-360 longitude
plot(meanTrend8.5N) #have a look
meanTrend8.5N<-revrotate(meanTrend8.5N)
extent(meanTrend8.5N)<-c(-180,180,-90,90)
plot(meanTrend8.5N)
plot(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#set up table
table1<-matrix(ncol=8,nrow=5)
table1[,1]<-c("Metric","Mean","Mean","Max","Max")
table1[,2]<-c("Model","8.5","4.5","8.5","4.5")

#all MPAs
nn.buffered.dat<-read.csv("nn_extractedA2c.csv") #read in .csv nn_extractedA2.csv with the MPA coordinates
ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
oceanmpa.trend_mean.8.5N<-extract(meanTrend8.5N,ocean_mpa[,2:3])
mean(oceanmpa.trend_mean.8.5N) #0.0351363/yr = 3.5 degrees c over 100 years
sd(oceanmpa.trend_mean.8.5N)
hist(oceanmpa.trend_mean.8.5N, main="mean trend all MPAs RCP8.5")
length(oceanmpa.trend_mean.8.5N) #8236
table1[1,4]<-paste("All MPAs (",length(oceanmpa.trend_mean.8.5N),")",sep='');length(oceanmpa.trend_mean.8.5N) #8236
write.csv(oceanmpa.trend_mean.8.5N, file="oceanmpa.trend_mean.8.5.csv")
table1[2,4]<-paste(round(mean(oceanmpa.trend_mean.8.5N,na.rm=T),3),"±",round(sd(oceanmpa.trend_mean.8.5N,na.rm=T),3),sep='')

#no-take reserves
reserves<-read.csv("notakeonly_coord.csv")  #read in .csv for no-take marine reserves
plot(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(reserves[,1:2])
reserves.trend_mean.8.5N<-extract(meanTrend8.5N,reserves[,1:2])
mean(reserves.trend_mean.8.5N) #0.03260264/yr = 3.5 degrees c over 100 years
sd(reserves.trend_mean.8.5N)
length(reserves.trend_mean.8.5N) #309
write.csv(reserves.trend_mean.8.5N, file="reserves.trend_mean.8.5.csv")
table1[1,3]<-paste("Reserves (",length(reserves.trend_mean.8.5N),")",sep='')
table1[2,3]<-paste(round(mean(reserves.trend_mean.8.5N,na.rm=T),3),"±",round(sd(reserves.trend_mean.8.5N,na.rm=T),3),sep='')

#Tropical MPAs < 23.5
tropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude>(-23.5)&Centroid_Latitude<23.5) #subset the points in tropical waters
plot(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(tropical_ocean_mpa[,2:3], col="red") 
tropical_ocean_mpa.trend_mean.8.5N<-extract(meanTrend8.5N,tropical_ocean_mpa[,2:3])
mean(tropical_ocean_mpa.trend_mean.8.5N) #0.03119445/yr = 3.5 degrees c over 100 years
sd(tropical_ocean_mpa.trend_mean.8.5N) #0.002228162
hist(tropical_ocean_mpa.trend_mean.8.5N, main="mean trend tropical MPAs RCP8.5")
length(tropical_ocean_mpa.trend_mean.8.5N) #2458
write.csv(tropical_ocean_mpa.trend_mean.8.5N, file="tropical_ocean_mpa.trend_mean.8.5N.csv")
table1[1,5]<-paste("Tropical (",length(tropical_ocean_mpa.trend_mean.8.5N),")",sep='')
table1[2,5]<-paste(round(mean(tropical_ocean_mpa.trend_mean.8.5N,na.rm=T),3),"±",round(sd(tropical_ocean_mpa.trend_mean.8.5N,na.rm=T),3),sep='')

#Subtropical MPAs 23.5-40
subtropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<40&Centroid_Latitude>23.5|Centroid_Latitude>-40&Centroid_Latitude<(-23.5)) #subset the points in subtropical waters
points(subtropical_ocean_mpa[,2:3], col="blue") 
subtropical_ocean_mpa.trend_mean.8.5N<-extract(meanTrend8.5N,subtropical_ocean_mpa[,2:3])
mean(subtropical_ocean_mpa.trend_mean.8.5N) #/yr = 3.5 degrees c over 100 years
sd(subtropical_ocean_mpa.trend_mean.8.5N) #
hist(subtropical_ocean_mpa.trend_mean.8.5N, main="mean trend subtropical MPAs RCP8.5")
length(subtropical_ocean_mpa.trend_mean.8.5N) #
write.csv(subtropical_ocean_mpa.trend_mean.8.5N, file="ST_ocean_mpa.trend_mean.8.5N.csv")
table1[1,6]<-paste("Subtropical (",length(subtropical_ocean_mpa.trend_mean.8.5N),")",sep='')
table1[2,6]<-paste(round(mean(subtropical_ocean_mpa.trend_mean.8.5N,na.rm=T),3),"±",round(sd(subtropical_ocean_mpa.trend_mean.8.5N,na.rm=T),3),sep='')

#Temerate MPAs 40-66.5
temperate_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<66.5&Centroid_Latitude>40.0|Centroid_Latitude>-66.5&Centroid_Latitude<(-40.0)) #subset the points in temperate waters
points(temperate_ocean_mpa[,2:3], col="green") 
temperate_ocean_mpa.trend_mean.8.5N<-extract(meanTrend8.5N,temperate_ocean_mpa[,2:3])
mean(temperate_ocean_mpa.trend_mean.8.5N) #/yr = 3.5 degrees c over 100 years
sd(temperate_ocean_mpa.trend_mean.8.5N) #
hist(temperate_ocean_mpa.trend_mean.8.5N, main="mean trend temperate MPAs RCP8.5")
length(temperate_ocean_mpa.trend_mean.8.5N) #
write.csv(temperate_ocean_mpa.trend_mean.8.5N, file="temperate_ocean_mpa.trend_mean.8.5N.csv")
table1[1,7]<-paste("Temperate (",length(temperate_ocean_mpa.trend_mean.8.5N),")",sep='')
table1[2,7]<-paste(round(mean(temperate_ocean_mpa.trend_mean.8.5N,na.rm=T),3),"±",round(sd(temperate_ocean_mpa.trend_mean.8.5N,na.rm=T),3),sep='')

#Polar MPAs > 66.5
polar_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<(-66.5)|Centroid_Latitude>66.5) #subset the points in polar waters
points(polar_ocean_mpa[,2:3], col="white") 
polar_ocean_mpa.trend_mean.8.5N<-extract(meanTrend8.5N,polar_ocean_mpa[,2:3])
mean(polar_ocean_mpa.trend_mean.8.5N) #/yr = 3.5 degrees c over 100 years
sd(polar_ocean_mpa.trend_mean.8.5N) #
hist(polar_ocean_mpa.trend_mean.8.5N, main="mean trend polar MPAs RCP8.5")
length(polar_ocean_mpa.trend_mean.8.5N) #
write.csv(polar_ocean_mpa.trend_mean.8.5N, file="polar_ocean_mpa.trend_mean.8.5N.csv")
table1[1,8]<-paste("Polar (",length(polar_ocean_mpa.trend_mean.8.5N),")",sep='')
table1[2,8]<-paste(round(mean(polar_ocean_mpa.trend_mean.8.5N,na.rm=T),3),"±",round(sd(polar_ocean_mpa.trend_mean.8.5N,na.rm=T),3),sep='')

##########################
### Max, Native 8.5 ####
##########################

#set up the model projections
source("revrotate..R") #make sure file is in pathway, or setwd() to the attached file
maxTrend8.5N<-raster("trend_yearmax_ensemble_RCP85.nc")
extent(maxTrend8.5N)<-c(-180,180,-90,90) #need to reset for this layer, orginal orientation was 0-360 longitude
plot(maxTrend8.5N) #have a look
maxTrend8.5N<-revrotate(maxTrend8.5N)
extent(maxTrend8.5N)<-c(-180,180,-90,90)
plot(maxTrend8.5N)
plot(maxTrend8.5N, main = ("RCP 8.5 warming rate for max SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#all MPAs
nn.buffered.dat<-read.csv("~/Dropbox/MPAs_warming/Data/nn_extractedA2c.csv") #read in .csv nn_extractedA2.csv with the MPA coordinates
ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
oceanmpa.trend_max.8.5N<-extract(maxTrend8.5N,ocean_mpa[,2:3])
mean(oceanmpa.trend_max.8.5N) #0.0351363/yr = 3.5 degrees c over 100 years
sd(oceanmpa.trend_max.8.5N)
hist(oceanmpa.trend_max.8.5N, main="max trend all MPAs RCP8.5")
length(oceanmpa.trend_max.8.5N) #8236
write.csv(oceanmpa.trend_max.8.5N, file="oceanmpa.trend_max.8.5.csv")
table1[4,4]<-paste(round(mean(oceanmpa.trend_max.8.5N,na.rm=T),3),"±",round(sd(oceanmpa.trend_max.8.5N,na.rm=T),3),sep='')

#no-take reserves
reserves<-read.csv("notakeonly_coord.csv")  #read in .csv for no-take marine reserves
plot(maxTrend8.5N)
points(reserves[,1:2])
reserves.trend_max.8.5N<-extract(maxTrend8.5N,reserves[,1:2])
mean(reserves.trend_max.8.5N) #0.03260264/yr = 3.5 degrees c over 100 years
sd(reserves.trend_max.8.5N)
length(reserves.trend_max.8.5N) #309
write.csv(reserves.trend_max.8.5N, file="reserves.trend_max.8.5.csv")
table1[4,3]<-paste(round(mean(reserves.trend_max.8.5N,na.rm=T),3),"±",round(sd(reserves.trend_max.8.5N,na.rm=T),3),sep='')

#Tropical MPAs < 23.5
tropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude>(-23.5)&Centroid_Latitude<23.5) #subset the points in tropical waters
plot(maxTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(tropical_ocean_mpa[,2:3], col="red") 
tropical_ocean_mpa.trend_max.8.5N<-extract(maxTrend8.5N,tropical_ocean_mpa[,2:3])
mean(tropical_ocean_mpa.trend_max.8.5N) #0.03119445/yr = 3.5 degrees c over 100 years
sd(tropical_ocean_mpa.trend_max.8.5N) #0.002228162
hist(tropical_ocean_mpa.trend_max.8.5N, main="max trend tropical MPAs RCP8.5")
length(tropical_ocean_mpa.trend_max.8.5N) #2458
write.csv(tropical_ocean_mpa.trend_max.8.5N, file="tropical_ocean_mpa.trend_max.8.5N.csv")
table1[4,5]<-paste(round(mean(tropical_ocean_mpa.trend_max.8.5N,na.rm=T),3),"±",round(sd(tropical_ocean_mpa.trend_max.8.5N,na.rm=T),3),sep='')

#Subtropical MPAs 23.5-40
subtropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<40&Centroid_Latitude>23.5|Centroid_Latitude>-40&Centroid_Latitude<(-23.5)) #subset the points in subtropical waters
points(subtropical_ocean_mpa[,2:3], col="blue") 
subtropical_ocean_mpa.trend_max.8.5N<-extract(maxTrend8.5N,subtropical_ocean_mpa[,2:3])
mean(subtropical_ocean_mpa.trend_max.8.5N) #/yr = 3.5 degrees c over 100 years
sd(subtropical_ocean_mpa.trend_max.8.5N) #
hist(subtropical_ocean_mpa.trend_max.8.5N, main="max trend subtropical MPAs RCP8.5")
length(subtropical_ocean_mpa.trend_max.8.5N) #
write.csv(subtropical_ocean_mpa.trend_max.8.5N, file="ST_ocean_mpa.trend_max.8.5N.csv")
table1[4,6]<-paste(round(mean(subtropical_ocean_mpa.trend_max.8.5N,na.rm=T),3),"±",round(sd(subtropical_ocean_mpa.trend_max.8.5N,na.rm=T),3),sep='')

#Temerate MPAs 40-66.5
temperate_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<66.5&Centroid_Latitude>40.0|Centroid_Latitude>-66.5&Centroid_Latitude<(-40.0)) #subset the points in temperate waters
points(temperate_ocean_mpa[,2:3], col="green") 
temperate_ocean_mpa.trend_max.8.5N<-extract(maxTrend8.5N,temperate_ocean_mpa[,2:3])
mean(temperate_ocean_mpa.trend_max.8.5N) #/yr = 3.5 degrees c over 100 years
sd(temperate_ocean_mpa.trend_max.8.5N) #
hist(temperate_ocean_mpa.trend_max.8.5N, main="max trend temperate MPAs RCP8.5")
length(temperate_ocean_mpa.trend_max.8.5N) #
write.csv(temperate_ocean_mpa.trend_max.8.5N, file="temperate_ocean_mpa.trend_max.8.5N.csv")
table1[4,7]<-paste(round(mean(temperate_ocean_mpa.trend_max.8.5N,na.rm=T),3),"±",round(sd(temperate_ocean_mpa.trend_max.8.5N,na.rm=T),3),sep='')

#Polar MPAs > 66.5
polar_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<(-66.5)|Centroid_Latitude>66.5) #subset the points in polar waters
points(polar_ocean_mpa[,2:3], col="white") 
polar_ocean_mpa.trend_max.8.5N<-extract(maxTrend8.5N,polar_ocean_mpa[,2:3])
mean(polar_ocean_mpa.trend_max.8.5N) #/yr = 3.5 degrees c over 100 years
sd(polar_ocean_mpa.trend_max.8.5N) #
hist(polar_ocean_mpa.trend_max.8.5N, main="max trend polar MPAs RCP8.5")
length(polar_ocean_mpa.trend_max.8.5N) #
write.csv(polar_ocean_mpa.trend_max.8.5N, file="polar_ocean_mpa.trend_max.8.5N.csv")
table1[4,8]<-paste(round(mean(polar_ocean_mpa.trend_max.8.5N,na.rm=T),3),"±",round(sd(polar_ocean_mpa.trend_max.8.5N,na.rm=T),3),sep='')

##########################
### Mean, Native 4.5 ####
##########################

#set up the model projections
source("revrotate..R") #make sure file is in pathway, or setwd() to the attached file
meanTrend4.5N<-raster("trend_yearmean_ensemble_tos_RCP45.nc") #trend_yearmean_ensemble_tos_RCP45.nc
extent(meanTrend4.5N)<-c(-180,180,-90,90) #need to reset for this layer, orginal orientation was 0-360 longitude
plot(meanTrend4.5N) #have a look
meanTrend4.5N<-revrotate(meanTrend4.5N)
extent(meanTrend4.5N)<-c(-180,180,-90,90)
plot(meanTrend4.5N)
plot(meanTrend4.5N, main = ("RCP 4.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#all MPAs
nn.buffered.dat<-read.csv("~/Dropbox/MPAs_warming/Data/nn_extractedA2c.csv") #read in .csv nn_extractedA2.csv with the MPA coordinates
ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
oceanmpa.trend_mean.4.5N<-extract(meanTrend4.5N,ocean_mpa[,2:3])
mean(oceanmpa.trend_mean.4.5N) 
sd(oceanmpa.trend_mean.4.5N)
hist(oceanmpa.trend_mean.4.5N, main="mean trend all MPAs RCP4.5")
length(oceanmpa.trend_mean.4.5N) #8236
write.csv(oceanmpa.trend_mean.4.5N, file="oceanmpa.trend_mean.4.5.csv")
table1[3,4]<-paste(round(mean(oceanmpa.trend_mean.4.5N,na.rm=T),3),"±",round(sd(oceanmpa.trend_mean.4.5N,na.rm=T),3),sep='')

#no-take reserves
reserves<-read.csv("notakeonly_coord.csv")  #read in .csv for no-take marine reserves
plot(meanTrend4.5N)
points(reserves[,1:2])
reserves.trend_mean.4.5N<-extract(meanTrend4.5N,reserves[,1:2])
mean(reserves.trend_mean.4.5N)
sd(reserves.trend_mean.4.5N)
length(reserves.trend_mean.4.5N) #309
write.csv(reserves.trend_mean.4.5N, file="reserves.trend_mean.4.5.csv")
table1[3,3]<-paste(round(mean(reserves.trend_mean.4.5N,na.rm=T),3),"±",round(sd(reserves.trend_mean.4.5N,na.rm=T),3),sep='')

#Tropical MPAs < 23.5
tropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude>(-23.5)&Centroid_Latitude<23.5) #subset the points in tropical waters
points(tropical_ocean_mpa[,2:3], col="red") 
tropical_ocean_mpa.trend_mean.4.5N<-extract(meanTrend4.5N,tropical_ocean_mpa[,2:3])
mean(tropical_ocean_mpa.trend_mean.4.5N) 
sd(tropical_ocean_mpa.trend_mean.4.5N)
hist(tropical_ocean_mpa.trend_mean.4.5N, main="mean trend tropical MPAs RCP4.5")
length(tropical_ocean_mpa.trend_mean.4.5N) #2458
write.csv(tropical_ocean_mpa.trend_mean.4.5N, file="tropical_ocean_mpa.trend_mean.4.5N.csv")
table1[3,5]<-paste(round(mean(tropical_ocean_mpa.trend_mean.4.5N,na.rm=T),3),"±",round(sd(tropical_ocean_mpa.trend_mean.4.5N,na.rm=T),3),sep='')

#Subtropical MPAs 23.5-40
subtropical_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude<40&Centroid_Latitude>23.5|Centroid_Latitude>-40&Centroid_Latitude<(-23.5)) #subset the points in subtropical waters
points(subtropical_ocean_mpa[,2:3], col="blue") 
subtropical_ocean_mpa.trend_mean.4.5N<-extract(meanTrend4.5N,subtropical_ocean_mpa[,2:3])
mean(subtropical_ocean_mpa.trend_mean.4.5N) 
sd(subtropical_ocean_mpa.trend_mean.4.5N) 
hist(subtropical_ocean_mpa.trend_mean.4.5N, main="mean trend subtropical MPAs RCP4.5")
length(subtropical_ocean_mpa.trend_mean.4.5N) 
write.csv(subtropical_ocean_mpa.trend_mean.4.5N, file="ST_ocean_mpa.trend_mean.4.5N.csv")
table1[3,6]<-paste(round(mean(subtropical_ocean_mpa.trend_mean.4.5N,na.rm=T),3),"±",round(sd(subtropical_ocean_mpa.trend_mean.4.5N,na.rm=T),3),sep='')

#Temerate MPAs 40-66.5
temperate_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude<66.5&Centroid_Latitude>40.0|Centroid_Latitude>-66.5&Centroid_Latitude<(-40.0)) #subset the points in temperate waters
points(temperate_ocean_mpa[,2:3], col="green") 
temperate_ocean_mpa.trend_mean.4.5N<-extract(meanTrend4.5N,temperate_ocean_mpa[,2:3])
mean(temperate_ocean_mpa.trend_mean.4.5N) 
sd(temperate_ocean_mpa.trend_mean.4.5N) 
hist(temperate_ocean_mpa.trend_mean.4.5N, main="mean trend temperate MPAs RCP4.5")
length(temperate_ocean_mpa.trend_mean.4.5N) 
write.csv(temperate_ocean_mpa.trend_mean.4.5N, file="temperate_ocean_mpa.trend_mean.4.5N.csv")
table1[3,7]<-paste(round(mean(temperate_ocean_mpa.trend_mean.4.5N,na.rm=T),3),"±",round(sd(temperate_ocean_mpa.trend_mean.4.5N,na.rm=T),3),sep='')

#Polar MPAs > 66.5
polar_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<(-66.5)|Centroid_Latitude>66.5) #subset the points in polar waters
points(polar_ocean_mpa[,2:3], col="white") 
polar_ocean_mpa.trend_mean.4.5N<-extract(meanTrend4.5N,polar_ocean_mpa[,2:3])
mean(polar_ocean_mpa.trend_mean.4.5N) 
sd(polar_ocean_mpa.trend_mean.4.5N) 
hist(polar_ocean_mpa.trend_mean.4.5N, main="mean trend polar MPAs RCP4.5")
length(polar_ocean_mpa.trend_mean.4.5N) 
write.csv(polar_ocean_mpa.trend_mean.4.5N, file="polar_ocean_mpa.trend_mean.4.5N.csv")
table1[3,8]<-paste(round(mean(polar_ocean_mpa.trend_mean.4.5N,na.rm=T),3),"±",round(sd(polar_ocean_mpa.trend_mean.4.5N,na.rm=T),3),sep='')

##########################
### Max, Native 4.5 ####
##########################

source("revrotate..R") #make sure file is in pathway, or setwd() to the attached file
maxTrend4.5N<-raster("trend_yearmax_ensemble_RCP45.nc") #trend_yearmax_ensemble_tos_RCP45.nc
extent(maxTrend4.5N)<-c(-180,180,-90,90) #need to reset for this layer, orginal orientation was 0-360 longitude
plot(maxTrend4.5N) #have a look
maxTrend4.5N<-revrotate(maxTrend4.5N)
extent(maxTrend4.5N)<-c(-180,180,-90,90)
plot(maxTrend4.5N)
plot(maxTrend4.5N, main = ("RCP 4.5 warming rate for max SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#all MPAs
nn.buffered.dat<-read.csv("~/Dropbox/MPAs_warming/Data/nn_extractedA2c.csv") #read in .csv nn_extractedA2.csv with the MPA coordinates
ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
oceanmpa.trend_max.4.5N<-extract(maxTrend4.5N,ocean_mpa[,2:3])
mean(oceanmpa.trend_max.4.5N) 
sd(oceanmpa.trend_max.4.5N)
hist(oceanmpa.trend_max.4.5N, main="max trend all MPAs RCP4.5")
length(oceanmpa.trend_max.4.5N) #8236
write.csv(oceanmpa.trend_max.4.5N, file="oceanmpa.trend_max.4.5.csv")
table1[5,4]<-paste(round(mean(oceanmpa.trend_max.4.5N,na.rm=T),3),"±",round(sd(oceanmpa.trend_max.4.5N,na.rm=T),3),sep='')

#no-take reserves
reserves<-read.csv("notakeonly_coord.csv")  #read in .csv for no-take marine reserves
plot(maxTrend4.5N)
points(reserves[,1:2])
reserves.trend_max.4.5N<-extract(maxTrend4.5N,reserves[,1:2])
mean(reserves.trend_max.4.5N)
sd(reserves.trend_max.4.5N)
length(reserves.trend_max.4.5N) #309
write.csv(reserves.trend_max.4.5N, file="reserves.trend_max.4.5.csv")
table1[5,3]<-paste(round(mean(reserves.trend_max.4.5N,na.rm=T),3),"±",round(sd(reserves.trend_max.4.5N,na.rm=T),3),sep='')

#Tropical MPAs < 23.5
tropical_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude>(-23.5)&Centroid_Latitude<23.5) #subset the points in tropical waters
points(tropical_ocean_mpa[,2:3], col="red") 
tropical_ocean_mpa.trend_max.4.5N<-extract(maxTrend4.5N,tropical_ocean_mpa[,2:3])
mean(tropical_ocean_mpa.trend_max.4.5N) 
sd(tropical_ocean_mpa.trend_max.4.5N)
hist(tropical_ocean_mpa.trend_max.4.5N, main="max trend tropical MPAs RCP4.5")
length(tropical_ocean_mpa.trend_max.4.5N) #2458
write.csv(tropical_ocean_mpa.trend_max.4.5N, file="tropical_ocean_mpa.trend_max.4.5N.csv")
table1[5,5]<-paste(round(mean(tropical_ocean_mpa.trend_max.4.5N,na.rm=T),3),"±",round(sd(tropical_ocean_mpa.trend_max.4.5N,na.rm=T),3),sep='')

#Subtropical MPAs 23.5-40
subtropical_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude<40&Centroid_Latitude>23.5|Centroid_Latitude>-40&Centroid_Latitude<(-23.5)) #subset the points in subtropical waters
points(subtropical_ocean_mpa[,2:3], col="blue") 
subtropical_ocean_mpa.trend_max.4.5N<-extract(maxTrend4.5N,subtropical_ocean_mpa[,2:3])
mean(subtropical_ocean_mpa.trend_max.4.5N) 
sd(subtropical_ocean_mpa.trend_max.4.5N) 
hist(subtropical_ocean_mpa.trend_max.4.5N, main="max trend subtropical MPAs RCP4.5")
length(subtropical_ocean_mpa.trend_max.4.5N) 
write.csv(subtropical_ocean_mpa.trend_max.4.5N, file="ST_ocean_mpa.trend_max.4.5N.csv")
table1[5,6]<-paste(round(mean(subtropical_ocean_mpa.trend_max.4.5N,na.rm=T),3),"±",round(sd(subtropical_ocean_mpa.trend_max.4.5N,na.rm=T),3),sep='')

#Temerate MPAs 40-66.5
temperate_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude<66.5&Centroid_Latitude>40.0|Centroid_Latitude>-66.5&Centroid_Latitude<(-40.0)) #subset the points in temperate waters
points(temperate_ocean_mpa[,2:3], col="green") 
temperate_ocean_mpa.trend_max.4.5N<-extract(maxTrend4.5N,temperate_ocean_mpa[,2:3])
mean(temperate_ocean_mpa.trend_max.4.5N) 
sd(temperate_ocean_mpa.trend_max.4.5N) 
hist(temperate_ocean_mpa.trend_max.4.5N, main="max trend temperate MPAs RCP4.5")
length(temperate_ocean_mpa.trend_max.4.5N) 
write.csv(temperate_ocean_mpa.trend_max.4.5N, file="temperate_ocean_mpa.trend_max.4.5N.csv")
table1[5,7]<-paste(round(mean(temperate_ocean_mpa.trend_max.4.5N,na.rm=T),3),"±",round(sd(temperate_ocean_mpa.trend_max.4.5N,na.rm=T),3),sep='')

#Polar MPAs > 66.5
polar_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude<(-66.5)|Centroid_Latitude>66.5) #subset the points in polar waters
points(polar_ocean_mpa[,2:3], col="white") 
polar_ocean_mpa.trend_max.4.5N<-extract(maxTrend4.5N,polar_ocean_mpa[,2:3])
mean(polar_ocean_mpa.trend_max.4.5N) 
sd(polar_ocean_mpa.trend_max.4.5N) 
hist(polar_ocean_mpa.trend_max.4.5N, main="max trend polar MPAs RCP4.5")
length(polar_ocean_mpa.trend_max.4.5N) 
write.csv(polar_ocean_mpa.trend_max.4.5N, file="polar_ocean_mpa.trend_max.4.5N.csv")
table1[5,8]<-paste(round(mean(polar_ocean_mpa.trend_max.4.5N,na.rm=T),3),"±",round(sd(polar_ocean_mpa.trend_max.4.5N,na.rm=T),3),sep='')

######################
### Extract all SST values to compare to MPA values ####
######################


#masking non-downscaled
R.msk<-raster("satmin_monthly.asc.txt")
R.msk<-raster("satmin_monthly2.asc") #remove.txt
#plot(R.msk)
R.msk<-crop(R.msk,c(-180,180,-45,45)) #crop to match extent
R.msk<-resample(R.msk,R_non.dscld) #resample to fit resolution (takes a little while)
mskd.R_non.dscld<-mask(R_non.dscld,R.msk,maskvalue=NA)
plot(mskd.R_non.dscld)

#all values takes ~10min for me to plot!!!!!!!!
x<-values(mskd.R_non.dscld)
y<-values(mskd.R_dscld)
plot(x,y)



(MPA.vals_dscld-MPA.vals_non.dscld)-(y-x) #overall mean
hist(MPA.vals_dscld-MPA.vals_non.dscld)
hist(y-x)


#Here is some code for checking downscaling bias between MPA's. I'm still working on it but we're in a giant windstorm right now and i hear our neighbors generator going so I wanted to send what I have in case we lose power. 
########################################
#compare downscaling bias in MPA cells

#crop non-downscaled raster to 45N-S

R_non.dscld #set as non-downscaled raster
R_non.dscld<-crop(R_non.dscld,c(-180,180,-45,45))

#compare MPA values between downscaled vs non-downscaled

R_dscld #set as downscaled raster
MPAs #set as lat lon points of MPAs

MPA.vals_dscld<-extract(R_dscld,MPAs)
MPA.vals_non.dscld<-extract(R_non.dscld,MPAs)

plot(MPA.vals_dscld,MPA.vals_non.dscld) #scatterplotk showing difference between downscaled mpas and non. notice trends. 



####compare background values between downscaled and non-downscaled

#mask out land - downscaled first
#raster from downscaled A2 scenario (just using the mask to get the land out)
R.msk<-raster("C:/Users/Chris/Desktop/Business/John Bruno/downscaled 2100/A2 (RCP 8.5)/2100/satmin_monthly.asc")
#plot(R.msk)
R.msk<-crop(R.msk,c(-180,180,-45,45)) #crop to match extent
R.msk<-resample(R.msk,R_dscld) #resample to fit resolution (takes a little while)
mskd.R_dscld<-mask(R_dscld,R.msk,maskvalue=NA)
plot(mskd.R_dscld)

#masking non-downscaled
R.msk<-raster("C:/Users/Chris/Desktop/Business/John Bruno/downscaled 2100/A2 (RCP 8.5)/2100/satmin_monthly.asc")
#plot(R.msk)
R.msk<-crop(R.msk,c(-180,180,-45,45)) #crop to match extent
R.msk<-resample(R.msk,R_non.dscld) #resample to fit resolution (takes a little while)
mskd.R_non.dscld<-mask(R_non.dscld,R.msk,maskvalue=NA)
plot(mskd.R_non.dscld)

#all values takes ~10min for me to plot!!!!!!!!
x<-values(mskd.R_non.dscld)
y<-values(mskd.R_dscld)
plot(x,y)


(MPA.vals_dscld-MPA.vals_non.dscld)-(y-x) #overall mean
hist(MPA.vals_dscld-MPA.vals_non.dscld)
hist(y-x)

################
### figures ####
###############

# boxplots

# maps
plot(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)
abline(h=66.5, lty=3)
abline(h=40, lty=3)
abline(h=23.5, lty=3)
abline(h=-23.5, lty=3)
abline(h=-40, lty=3)
abline(h=-66.5, lty=3)

# maps
plot(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(reserves[,1:2], pch=20, cex=.5)
abline(h=66.5, lty=3)
abline(h=40, lty=3)
abline(h=23.5, lty=3)
abline(h=-23.5, lty=3)
abline(h=-40, lty=3)
abline(h=-66.5, lty=3)

#make map with the image function
image(meanTrend8.5N, main = ("RCP 8.5 warming rate for mean SST"), ylim = c(-73, 73),col=rev(rainbow(200, start=.8, end=.21)), xlab='',ylab='')
points(ocean_mpa[,2:3], pch=20, cex=.5)
abline(h=66.5, lty=3)
abline(h=40, lty=3)
abline(h=23.5, lty=3)
abline(h=-23.5, lty=3)
abline(h=-40, lty=3)
abline(h=-66.5, lty=3)

scalebar(d=4000,xy=c(80,-54),label=c(0,'',4000),cex=.9,type='bar',divs=4,below="kilometers",adj=c(0.5,-1.1))

#function for creating the compass rose (goes above the plotting function)
compassRose<-function(x,y,rot=0,cex=1,cex.dir=1,llwd=1) {
  oldcex<-par(cex=cex)
  mheight<-strheight("M")
  xylim<-par("usr")
  plotdim<-par("pin")
  xmult<-(xylim[2]-xylim[1])/(xylim[4]-xylim[3])*plotdim[2]/plotdim[1]
  point.angles<-seq(0,7*pi/4,by=pi/4)+pi*rot/180
  crspans<-rep(c(mheight*3,mheight/2),4)
  xpoints<-cos(point.angles)*crspans*xmult+x
  ypoints<-sin(point.angles)*crspans+y
  polygon(xpoints,ypoints,lwd=llwd)
  txtxpoints<-cos(point.angles[c(1,3,5,7)])*1.33*crspans[1]*xmult+x
  txtypoints<-sin(point.angles[c(1,3,5,7)])*1.33*crspans[1]+y
  text(txtxpoints,txtypoints,c("E","N","W","S"),cex=cex.dir)
  par(oldcex) 
}

compassRose(-125,-53,cex=.5,cex.dir=1.2) #goes after initial plot


### stuff below is misc recent code, mostly from Apri 6, 2017 ####


#1)
setwd("~/Dropbox/MPAs_warming/Layers/CMIP5/updated_CMIP5_files") 
maxTrend8.5N<-raster("trend_yearmax_ensemble_tos_RCP85.nc")
extent(maxTrend8.5N)<-c(-180,180,-90,90) #need to reset for this layer, orginal orientation was 0-360 longitude
plot(maxTrend8.5N)

#2)
nn.buffered.dat<-read.csv("~/Dropbox/MPAs_warming/Data/nn_extractedA2c.csv")# read in attached .csv nn_extractedA2.csv
Ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
points(Ocean_mpa[,2:3]) 

#world < 70
mpa_coord <- read.csv("~/Dropbox/MPAs_warming/Data/mpa_coords.csv", comment.char="#")
plot(mpa_coord)

plot(maxTrend8.5N, main = ("RCP 8.5 warming rate"),  ylim = c(-90, 90), col=rev(rainbow(200, start=.8, end=.3)))
points(mpa_coord[,2:3], pch=16, cex=.5)

#Galapagos islands (zoomed in)
plot(maxTrend8.5N, main = ("RCP 8.5 warming rate in the Galapagos Islands)"), xlim = c(-92.5, -88.6), ylim = c(-2.3,1.26), col=rev(rainbow(200, start=.8, end=.3)))

tropical_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude>(-30)&Centroid_Latitude<30) #subset the points in tropical waters
points(tropical_ocean_mpa[,3:4]) #plot points to make sure they fit (lon,lat=3,4)

#2b) Subtropical 
subtropical_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude>(-45)&Centroid_Latitude<45) #subset the points in tropical and subtropical waters
points(subtropical_ocean_mpa[,3:4]) #plot points to make sure they fit (lon,lat=3,4)

#2c) delinate 40-40 MPAs (toprical + subtropical) to compare to native version -> "compare"
nn.buffered.dat<-read.csv("nn_extractedA2c.csv")# read in attached .csv nn_extractedA2.csv
Ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water
compare_ocean_mpa<-subset(Ocean_mpa,Centroid_Latitude>(-40)&Centroid_Latitude<40) #subset the points in tropical and subtropical waters
points(compare_ocean_mpa[,2:3],col="red") #plot points to make sure they fit (lon,lat=2,3)
comparempa.trend_max.8.5N<-extract(maxTrend8.5N,compare_ocean_mpa[,2:3])
max(comparempa.trend_max.8.5N) #average for the MPA's trend under rcp8.5, 0.03211357/yr = 3.2 degrees c over 100 years
hist(comparempa.trend_max.8.5N, main="max trend compare RCP8.5")
length(comparempa.trend_max.8.5N) #5196
write.csv(comparempa.trend_max.8.5N, file="comparempa.trend_max.8.5.csv")

#compare downscaled to native data
model_compare.dat<-read.csv("~/Dropbox/MPAs_warming/Layers/CMIP5 downscaled/downscaled_comparempa.trend_max.8.5.csv")
head(model_compare.dat)
plot(downscaled_CMIP5_8.5 ~ CMIP5_8.5, model_compare.dat)

model1 <- lm(downscaled_CMIP5_8.5 ~ CMIP5_8.5, data = model_compare.dat)
summary(model1) #R2 = 0.13, P is very small




#3)
tropmpa.trend_max.8.5N<-extract(maxTrend8.5N,tropical_ocean_mpa[,3:4])
max(tropmpa.trend_max.8.5N) #average for the MPA's trend under rcp8.5, 0.02820026/yr = 2.8 degrees c over 100 years

#3b)
subtropmpa.trend_max.8.5N<-extract(maxTrend8.5N,subtropical_ocean_mpa[,3:4])
max(subtropmpa.trend_max.8.5N) #average for the MPA's trend under rcp8.5,  0.03274306/yr = 3.3 degrees c over 100 years

hist(subtropmpa.trend_max.8.5N, main="max trend RCP8.5")
density((maxTrend8.5N), main="max trend RCP8.5")

length(subtropmpa.trend_max.8.5N) #6019
summary(subtropmpa.trend_max.8.5N) #median = 0.03241
write.csv(subtropmpa.trend_max.8.5N, file="subtropmpa.trend_max.8.5.csv")


