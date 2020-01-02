#Plot time series chlorophyll -a monthly mean
#Subhadeep Maishal
#Birla Institute of Technology, Mesra
# Function to check if pkgs are installed, install missing pkgs, and load
pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop(x, " :Package not found")
  }
}

# create list of required packages
list.of.packages <- c( "lubridate", "maps",  "mapdata", "maptools", "mapproj", "ncdf4",
                       "reshape2", "colorRamps", "sp", "plyr", "devtools", "ggplot2", "gridExtra")

# create list of installed packages
pkges = installed.packages()[,"Package"]

for (pk in list.of.packages) {
  pkgTest(pk)
}

# create list of required packages not installed
#new.packages <- list.of.packages[!(list.of.packages %in% pkges)]

# install missing packages
#if(length(new.packages)) install.packages(new.packages)

# check if devtools pkgs are install. Install missing pkgs.
if(!('rerddapXtracto' %in% pkges)) {
  devtools::install_github("rmendels/rerddapXtracto")}
if(!('plotdap' %in% pkges)) {
  devtools::install_github('ropensci/plotdap')} 
if(!('rerddap' %in% pkges)) {
  devtools::install_github("ropensci/rerddap")}

library(rerddap)
library(plotdap)
library(rerddapXtracto)
#%%%SUBHADEEP MAISHAL
#SUBSET
xcoord<-c(40, 100)
ycoord<-c(0,35)

##Format Box Coordinates for cosmetics, to make a nice map title
ttext<-paste(paste(abs(xcoord), collapse="-"),"W, ", paste(ycoord, collapse="-"),"N")
# Use rerddap to get information about the dataset
dataInfo <- rerddap::info('erdSWchlamday')

# Display the dataset metadata
dataInfo
## <ERDDAP info> erdSWchlamday 
##  Base URL: https://upwell.pfeg.noaa.gov/erddap/ 
##  Dimensions (range):  
##      time: (1997-09-16T00:00:00Z, 2010-12-16T12:00:00Z) 
##      altitude: (0.0, 0.0) 
##      latitude: (-90.0, 90.0) 
##      longitude: (0.0, 360.0) 
##  Variables:  
##      chlorophyll: 
##          Units: mg m-3

# Extract the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

# Set the altitude coordinate to zero
zcoord <- 0.

# Extract the beginning and ending dates of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tcoord <- c(tt[2],"last")

# Extract the timeseries data using rxtracto_3D
chlSeaWiFS<-rxtracto_3D(dataInfo,parameter=parameter,
                        tcoord=tcoord,
                        xcoord=xcoord,ycoord=ycoord,zcoord=zcoord)

## Cleanup the returned data, remove extraneous zcoord dimension for chlorophyll 
chlSeaWiFS$chlorophyll <- drop(chlSeaWiFS$chlorophyll)
# Use rerddap to get information about the dataset
# if you encouter an error reading the nc file clear the rerrdap cache: 
# rerddap::cache_delete_all(force = TRUE)
dataInfo <- rerddap::info('erdMH1chlamday')
dataInfo

## <ERDDAP info> erdMH1chlamday 
##  Base URL: https://upwell.pfeg.noaa.gov/erddap/ 
##  Dimensions (range):  
##      time: (2003-01-16T00:00:00Z, 2019-04-16T00:00:00Z) 
##      latitude: (-89.97918, 89.97916) 
##      longitude: (-179.9792, 179.9792) 
##  Variables:  
##      chlorophyll: 
##          Units: mg m^-3
# Extract the parameter name from the metadata in dataInfo
parameter <- dataInfo$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")

# Run rxtracto_3D
chlMODIS<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)


# Use rerddap to get information about the dataset
# if you encouter an error reading the nc file clear the rerrdap cache: 
# rerddap::cache_delete_all(force = TRUE)
dataInfo <- rerddap::info('nesdisVHNSQchlaMonthly')
#dataInfo <- rerddap::info('erdVHNchlamday')  # alternate dataset to use
dataInfo

## <ERDDAP info> nesdisVHNSQchlaMonthly 
##  Base URL: https://upwell.pfeg.noaa.gov/erddap/ 
##  Dimensions (range):  
##      time: (2012-01-02T12:00:00Z, 2019-04-01T12:00:00Z) 
##      altitude: (0.0, 0.0) 
##      latitude: (-89.75626, 89.75625) 
##      longitude: (-179.9812, 179.9813) 
##  Variables:  
##      chlor_a: 
##          Units: mg m^-3
## This extracts the parameter name from the metadata  in dataInfo
parameter <- dataInfo$variable$variable_name

#Extract the start and end times of the dataset from the metadata in dataInfo
global <- dataInfo$alldata$NC_GLOBAL

# Populate the time vector with the time_coverage_start from dataInfo
# Use the "last" option for the ending date
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")
#tcoord <- c(tt[1], tt[2])

# Run rxtracto_3D
chlVIIRS<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord,zcoord=zcoord)

## Clean up, remove extraneous zcoord dimension for chlorophyll 
chlVIIRS$chlor_a <- drop(chlVIIRS$chlor_a)
#chlVIIRS$chla <- drop(chlVIIRS$chla)
## Spatially average all the data within the box for each dataset.
## The c(3) indicates the dimension to keep - in this case time 
chlSeaWiFS$avg <- apply(chlSeaWiFS$chlorophyll, c(3),function(x) mean(x,na.rm=TRUE))
chlMODIS$avg <- apply(chlMODIS$chlorophyll, c(3),function(x) mean(x,na.rm=TRUE))
chlVIIRS$avg <- apply(chlVIIRS$chlor_a, c(3),function(x) mean(x,na.rm=TRUE))
#chlVIIRS$avg <- apply(chlVIIRS$chla, c(3),function(x) mean(x,na.rm=TRUE))

## Temporally average all of the data into one map 
## The c(1,2) indicates the dimensions to keep - in this case latitude and longitude  
chlSeaWiFS$avgmap <- apply(chlSeaWiFS$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))
chlMODIS$avgmap <- apply(chlMODIS$chlorophyll,c(1,2),function(x) mean(x,na.rm=TRUE))
chlVIIRS$avgmap <- apply(chlVIIRS$chlor_a,c(1,2),function(x) mean(x,na.rm=TRUE))
#chlVIIRS$avgmap <- apply(chlVIIRS$chla,c(1,2),function(x) mean(x,na.rm=TRUE))
## To print out a file uncomment the png command and the dev.off command
##png(file="CHL_timeseries.png", width=10,height=7.5,units="in",res=500)

## To print out a file uncomment the png command and the dev.off command
##png(file="CHL_timeseries.png", width=10,height=7.5,units="in",res=500)
plot(as.Date(chlSeaWiFS$time), chlSeaWiFS$avg, 
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     ylim=c(0,1.5),
     ylab="Chlorophyll", main=ttext)
#axis(2)

# Now add MODIS and VIIRS  data 
points(as.Date(chlMODIS$time), chlMODIS$avg, type='b', bg="magenta", pch=21,cex=.7)
points(as.Date(chlVIIRS$time), chlVIIRS$avg, type='b', bg="black", pch=21,cex=.7)

text(as.Date("1997-03-01"),2.8, "SeaWiFS",col="blue", pos=4)
text(as.Date("1997-03-01"),2.5, "MODIS",col="magenta", pos=4)
text(as.Date("1997-03-01"),2.2, "VIIRS",col="black", pos=4)


# Reading in three datasets, which  have different datset attributes (ie parameter 
# name and the presence or absence of an altitude field) is cumbersome.  ESA makes 
# a "mission-less" product, which seemlessly integrates data from all these sensors 
# into one.  So lets redo this exercise iusing this dateset instead and compare the results.  

dataInfo <- rerddap::info('pmlEsaCCI31OceanColorMonthly')

# This identifies the parameter to choose - there are > 60 in this dataset1 
parameter <- 'chlor_a'

global <- dataInfo$alldata$NC_GLOBAL
tt <- global[ global$attribute_name %in% c('time_coverage_end','time_coverage_start'), "value", ]
tcoord <- c(tt[2],"last")
# if you encouter an error reading the nc file clear the rerrdap cache: 
rerddap::cache_delete_all(force = TRUE)

chlOCCCI<-rxtracto_3D(dataInfo,parameter=parameter,
                      tcoord=tcoord,
                      xcoord=xcoord,ycoord=ycoord)

# Now spatially average the data into a timeseries
chlOCCCI$avg <- apply(chlOCCCI$chlor_a, c(3),function(x) mean(x,na.rm=TRUE))

# Now temporally average the data into one map 
chlOCCCI$avgmap <- apply(chlOCCCI$chlor_a,c(1,2),function(x) mean(x,na.rm=TRUE))

#Add ESA OCCI data to the plot

## Plot SeaWIFS
plot(as.Date(chlSeaWiFS$time), chlSeaWiFS$avg, 
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
     ylim=c(0,1.5),
     ylab="Chlorophyll", main=ttext)
#axis(2)

## Add MODIS, VIIRS and OCCCI data 
points(as.Date(chlMODIS$time), chlMODIS$avg, type='b', bg="magenta", pch=21,cex=.7)
points(as.Date(chlVIIRS$time), chlVIIRS$avg, type='b', bg="black", pch=21,cex=.7)
points(as.Date(chlOCCCI$time), chlOCCCI$avg, type='b', bg="lightseagreen", pch=21,cex=1)
## Add text annotation for legend
text(as.Date("1997-03-01"),2.8, "SeaWiFS",col="blue", pos=4)
text(as.Date("1997-03-01"),2.5, "MODIS",col="magenta", pos=4)
text(as.Date("1997-03-01"),2.2, "VIIRS",col="black", pos=4)
text(as.Date("1997-03-01"),1.9, "OC-CCI",col="lightseagreen", pos=4)


## Plot SeaWIFS
plot(as.Date(chlOCCCI$time), chlOCCCI$avg, 
     type='b', bg="blue", pch=21, xlab="", cex=.7,
     xlim=as.Date(c("1997-01-01","2019-01-01")),
       ylim=c(0,1),
     ylab="Chlorophyll", main=ttext)
#axis(2)
text(as.Date("1997-03-01"),1.9, "OC-CCI",col="blue", pos=4)

plot(as.Date(chlOCCCI$time), chlOCCCI$avg, type="l", col="blue", lwd=2, xlim=as.Date(c("1997-01-01","2019-01-01")), ylim=c(0,0.8), ylab="Chlorophyll", main=ttext )
text(as.Date("1997-03-01"),1.9, "OC-CCI",col="blue", pos=4)









#plot map
coast <- map_data("worldHires", ylim = ycoord, xlim = xcoord)

# Put arrays into format for ggplot
melt_map <- function(lon,lat,var) {
  dimnames(var) <-list(Longitude=lon, Latitude=lat)
  ret <- melt(var,value.name="Chl")
}

# Loop for making 4 maps
datasetnames <- c("SeaWiFS","MODIS","VIIRS","OC-CCI")

plot_list = list()

for(i in 1:4) {
  
  if(i == 1) chl <- chlSeaWiFS
  if(i == 2) chl <- chlMODIS
  if(i == 3) chl <- chlVIIRS
  if(i == 4) chl <- chlOCCCI
  
  chlmap <- melt_map(chl$longitude, chl$latitude, chl$avgmap)
  
  p = ggplot(
    data = chlmap, 
    aes(x = Longitude, y = Latitude, fill = log(Chl))) +
    geom_raster(interpolate = FALSE, na.rm=T) +
    geom_polygon(data = coast, aes(x=long, y = lat, group = group), fill = "grey80") +
    theme_bw(base_size = 12) + ylab("Latitude") + xlab("Longitude") +
    coord_fixed(1.3, xlim = c(-120,-116), ylim = ycoord) +
    scale_fill_gradientn(colours = rev(rainbow(12)), na.value = NA, limits=c(-2,3)) +
    ggtitle(paste("Average", datasetnames[i])
    ) 
  
  plot_list[[i]] = p
}

# Now print out maps into a png file.  Can't use par function with ggplpot to get 
# multiple plots per page.  Here using a function in the gridExtra package

#png(file="CHL_averagemaps.png")
library(grid)
grid.arrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]], nrow = 2)

