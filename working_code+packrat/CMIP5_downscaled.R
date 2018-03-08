#Uploaded to Git "MPAs_warming" on August 5, 2017: https://github.com/johnfbruno/MPAs_warming.git
#new path to files in desktop Git folder: MPA_warming ms files for Git:  Dropbox/JB/Manuscripts/MPA_warming ms files for Git/MPAs_warming

############################
#### new code for MPA warming project for the downscaled CMPI5 ansemble values. Code is based on the file written for the large grain (not downscaled) CMPI5 values: "CMIP5_extract_2 copy.R" 
#creatd August 5, 2017 by JB
############################

library(raster)
source("revrotate.R") #make sure file is in pathway, or setwd() to the attached file

setwd("~/Dropbox/MPAs_warming/Layers/CMIP5/updated_CMIP5_files") 

setwd("~/Dropbox/JB/Manuscripts/MPA_warming ms files for Git/MPAs_warming") #set to local Git folder

##########################
## Mean, Downscaled 8.5 ##
##########################

#set up the model projections
meanTrend8.5DS<-raster("DS_trend_yearmean_tos_rcp85.nc")

#define "ocean_mpa"
nn.buffered.dat<-read.csv("nn_extractedA2c.csv") #read in .csv nn_extractedA2.csv with the MPA coordinates
ocean_mpa<-subset(nn.buffered.dat,nn.buffered.dat$km<50) #subset the points less than 50 km from water

#have a look
plot(meanTrend8.5DS) 
points(ocean_mpa[,2:3], pch=20, cex=.5)

#fancy plot
plot(meanTrend8.5DS, main = ("RCP 8.5_DS warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#define extent: tropical & subtropical MPAs < 40
trop_subtropic_ocean_mpa<-subset(ocean_mpa,Centroid_Latitude>(-40)&Centroid_Latitude<40) 

#extract values and calculate basic stats
trop_subtropic_ocean_mpa.trend_mean.8.5DS<-extract(meanTrend8.5DS,trop_subtropic_ocean_mpa[,2:3])
mean(trop_subtropic_ocean_mpa.trend_mean.8.5DS) #0.02749042/yr
sd(trop_subtropic_ocean_mpa.trend_mean.8.5DS) #0.005198534
length(trop_subtropic_ocean_mpa.trend_mean.8.5DS) #5196 (2458+2738)
hist(trop_subtropic_ocean_mpa.trend_mean.8.5DS, main="RCP8.5 downscaled", xlim = c(0.015, 0.050))


##########################
## Mean, Downscaled 4.5 ##
##########################

#set up the model projections
meanTrend4.5DS<-raster("DStrend_yearmean_tos_rcp45.nc")

#have a look
plot(meanTrend4.5DS) 
points(ocean_mpa[,2:3], pch=20, cex=.5)

#fancy plot
plot(meanTrend4.5DS, main = ("RCP 4.5_DS warming rate for mean SST"),  ylim = c(-71.2, 71.2), col=rev(rainbow(200, start=.8, end=.23)))
points(ocean_mpa[,2:3], pch=20, cex=.5)

#extract values and calculate basic stats
trop_subtropic_ocean_mpa.trend_mean.4.5DS<-extract(meanTrend4.5DS,trop_subtropic_ocean_mpa[,2:3])
mean(trop_subtropic_ocean_mpa.trend_mean.4.5DS) #0.01412256/yr
sd(trop_subtropic_ocean_mpa.trend_mean.4.5DS) #0.001870677
length(trop_subtropic_ocean_mpa.trend_mean.4.5DS) #5196 (2458+2738)
hist(trop_subtropic_ocean_mpa.trend_mean.4.5DS, main="RCP4.5 downscaled", xlim = c(0.01, 0.025))
