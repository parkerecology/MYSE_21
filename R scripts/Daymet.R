


#getting daymet data 


library(tidyverse)
library(patchwork)
library(raster)
library(daymetr)
library(rasterVis)
library(rgdal)
library(ggplot2)


datadir <-"~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data"

download_daymet_ncss(
  location = c(37.588117,
               -84.321869,
               33.842316,
               -75.460621),
  start = 2015,
  end = 2016,
  frequency = "monthly",
  param = "tmax",
  path = datadir,
  silent = FALSE)



mf <- file.path(datadir, "tmax_monavg_2015_ncss.nc")

# load raster data
setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data")
r<-brick("tmax_monavg_2015_ncss.nc")

# to set the correct projection
raster::projection(r) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"

# reproject to lat lon
r <- raster::projectRaster(r, crs = "+init=epsg:4326")
library(rcartocolor)
library(viridis)
display_carto_all()

gplot(r)+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Longitude,y=Latitude),col="blue",fill="turquoise1",size=0.5)+
  scale_fill_carto_c(palette = "Geyser")+
coord_equal()

col<-viridis(100)
#col<-carto_pal(7, "Geyser")
levelplot(r,col.regions = col)


