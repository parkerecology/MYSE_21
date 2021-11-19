#user credintials
library(terra)
library(luna)
library(geodata)
library(tidyverse)
library(beepr)


datadir <-"~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data"

usr <- "parker_ecology"
pwd <- "MakeThisWork45!"


#get modis data for exporation 

prod <- getProducts()

# we want the MOD44B vegitation continous fields yearly L3 global
modis <- getProducts("^MOD|^MYD|^MCD")

product<-"MCD12Q1"
start <- "2015-06-01"
end <- "2021-08-10"

productInfo(product)

usa <- geodata::gadm("united states", level=2, path=".")
usa

i <- usa$NAME_1 == "North Carolina"
aoi <- usa[i,]

#plot(aoi, col="light gray")

mf <- luna::getModis(product, start, end, aoi=aoi, download = FALSE)
mf


mf <- luna::getModis(product, start, end, aoi=aoi, download=TRUE,
                     path=datadir, username=usr, password=pwd)
mf

mf <- file.path(datadir, "MOD44B.006_250m_aid0001.nc")
library(terra)

r <- terra::rast(mf[1])

setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data")
rn<-brick("MOD44B.006_250m_aid0001.nc",crs=newproj)

beep(8)

terra::plotRGB(r, r = 1, g = 4, b = 3, stretch="lin")

from <- c(1,3,11,14)
to   <- c(2,3,11,14)
reject <- c("01,10", "1", "1", "1")
qa_bits <- cbind(from, to, reject)
qa_bits

qc <- r[[12]]
plot(r$Percent_Tree_Cover_1, main = "tree cover")

quality_mask <- modis_mask(qc, 16, qa_bits)
plot(quality_mask, main= "moon visible")

prj <- crs(quality_mask)
prj
poly <- project(aoi, prj)


plot(quality_mask, main= "moon visible")
lines(poly, col="blue")



library(rasterVis)
library(raster)
library(rgdal)

r[r>100] <- NA

levelplot(r[[1:5]])

levelplot(r[[1:5]],labels = c("2016","2017","2018","2019","2020"))

newproj <- "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

nr<-projectRaster(r,newproj)

gplot(r) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal()




#extract canopy coverage at the finer scales

#load observation covs and presence absence data
setwd("~/github/MYSE_21")

s21 <- read_csv("data/sites_2015_2021.csv")
s21<-plyr::rename(s21,c("site"="site_code" ))
names(s21)

setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data")

r_brick<-brick("MOD44B.006_250m_aid0001.nc")

r_brick[r_brick>100] <- NA

coords <- data.frame(s21[,c(7,6)])

gplot(r_brick[[1]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Longitude,y=Latitude),col="red")+
  coord_equal()

values <- data.frame(extract(r_brick, coords))
names(values) <- gsub(pattern = "X", replacement = "cc_250m_", x = names(values))
names(values) <- substring(names(values),1,12)

s.no<-data.frame(s21[,c(2,7,6)])

gf <- cbind.data.frame(s.no,values)

cc_250m <- gather(gf,year,cc_250m,cc_250m_2015:cc_250m_2020, factor_key=TRUE)

# setwd("~/github/MYSE_21/data")
# 
# write_csv(cc_250m,"Yearly_canopy_250m.csv")


#extract landcover at the finer scales

#load observation covs and presence absence data
setwd("~/github/MYSE_21")

s21 <- read_csv("data/sites_2015_2021.csv")
s21<-plyr::rename(s21,c("site"="site_code" ))
names(s21)

setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/MODIS/MODIS_data")

lc_brick<-brick("MCD12Q1.006_500m_aid0001.nc",varname="LC_Prop1")

#lc_brick[lc_brick>100] <- NA

coords <- data.frame(s21[,c(7,6)])

gplot(lc_brick[[1]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Longitude,y=Latitude),col="red")+
  scale_fill_fermenter()+
  coord_equal()

values <- data.frame(extract(lc_brick, coords))
names(values) <- gsub(pattern = "X", replacement = "lc_500m_", x = names(values))
names(values) <- substring(names(values),1,12)

s.no<-data.frame(s21[,c(2,7,6)])

gf <- cbind.data.frame(s.no,values)

lc_500m <- gather(gf,year,lc_500m,lc_500m_2015:lc_500m_2020, factor_key=TRUE)

# setwd("~/github/MYSE_21/data")
# 
# write_csv(cc_250m,"Yearly_canopy_250m.csv")





# FedData to get NLCD urbanization layers ---------------------------------

library(FedData)
library(sf)

usa <- geodata::gadm("united states", level=2, path=".")
usa

i <- usa$NAME_1 == "North Carolina"
aoi <- usa[i,]

aoi <- sf::st_as_sf(aoi)

library(tigris)

us<-states()

NC<-subset(us,NAME=="North Carolina")



#set working dir 


setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/GIS data/NLCD")

NLCD_imp <- get_nlcd(
  template = NC,
  year = 2019,
  dataset = 'impervious',
  label = "NC impervious"
)


