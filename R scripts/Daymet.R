


#getting daymet data 


library(tidyverse)
library(patchwork)
library(raster)
library(daymetr)
library(rasterVis)
library(rgdal)
library(ggplot2)
library(ggpubr)

datadir <-"~/Documents/Contract work/UNCG/2021/MYSE Project/GIS data/Grided data/DAYMET"


# download_daymet_ncss(
#   location = c(37.588117,-84.321869, 33.842316,-75.460621),
#   start = 2015,
#   end = 2020,
#   frequency = "monthly",
#   param = c("tmax","tmin","prcp"),
#   path = datadir,
#   silent = FALSE)




# Max temperature monthly mean  -------------------------------------------



# load raster data
#mf <- file.path(datadir, "tmax_monavg_2015_ncss.nc")
setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/GIS data/Grided data/DAYMET")
tmax_15<-brick("tmax_monavg_2015_ncss.nc")
tmax_16<-brick("tmax_monavg_2016_ncss.nc")
tmax_17<-brick("tmax_monavg_2017_ncss.nc")
tmax_18<-brick("tmax_monavg_2018_ncss.nc")
tmax_19<-brick("tmax_monavg_2019_ncss.nc")
tmax_20<-brick("tmax_monavg_2020_ncss.nc")

# to set the correct projection
raster::projection(tmax_15) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_16) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_17) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_18) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_19) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_20) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"


# reproject to lat lon
tmax_15 <- raster::projectRaster(tmax_15, crs = "+init=epsg:4326")
tmax_16 <- raster::projectRaster(tmax_16, crs = "+init=epsg:4326")
tmax_17 <- raster::projectRaster(tmax_17, crs = "+init=epsg:4326")
tmax_18 <- raster::projectRaster(tmax_18, crs = "+init=epsg:4326")
tmax_19 <- raster::projectRaster(tmax_19, crs = "+init=epsg:4326")
tmax_20 <- raster::projectRaster(tmax_20, crs = "+init=epsg:4326")



library(rcartocolor)
library(viridis)

# col<-viridis(100)
# #col<-carto_pal(7, "Geyser")
# levelplot(tmax_15,col.regions = col)


#load the points 

setwd("~/github/MYSE_21/data")

S21 <- read_csv("sites_2015_2021.csv")
S21<-plyr::rename(S21, c("site"="site_code"))
coords <- data.frame(S21[,c(7,6)])

#check coords for crs match 
gplot(tmax_15[[4]])+
geom_raster(aes(fill=value))+
geom_point(
data=coords,
aes(x=Longitude,y=Latitude),col="blue",fill="turquoise1",size=0.5)+
scale_fill_carto_c(palette = "Geyser",na.value="white")+
coord_equal()+
  theme_pubr()



#extract values 
library(maps)
v_tmax_15 <- data.frame(extract(tmax_15, coords))
v_tmax_16 <- data.frame(extract(tmax_16, coords))
v_tmax_17 <- data.frame(extract(tmax_17, coords))
v_tmax_18 <- data.frame(extract(tmax_18, coords))
v_tmax_19 <- data.frame(extract(tmax_19, coords))
v_tmax_20 <- data.frame(extract(tmax_20, coords))


v_tmax_15%>%
  filter(is.na(.))#no NAs 


tmax_all<-cbind(v_tmax_15,
                v_tmax_16,
                v_tmax_17,
                v_tmax_18,
                v_tmax_19,
                v_tmax_20)


tmax_all$site_code<-S21$site_code

names(tmax_all) <- gsub(pattern = "X", replacement = "tmax_", x = names(tmax_all))
names(tmax_all) <- substring(names(tmax_all),1,12)


library(stringr)
tmax_08<-
  tmax_all%>%
  select_if(stringr::str_detect(names(.),"08") | stringr::str_detect(names(.),"site_code"))


#gather vars into long format

tmax_l <- gather(tmax_all,time,tmax,tmax_2015.01:tmax_2020.12, factor_key=TRUE)

#aggregate into yearly, quartly, seasonal precip

#add new cols for month and year

tmax_l$Year<-substring(tmax_l$time,6,9)
tmax_l$month<-substring(tmax_l$time,11,12)

#some formatting
#tmax_l$Year_d<-as.Date(tmax_l$Year,format="%Y")
tmax_l$Year_n<-as.numeric(tmax_l$Year)

#change month to numeric 
tmax_l$month<-as.numeric(tmax_l$month)


tmax_year_mean<-tmax_l%>%
  dplyr::group_by(Year,site_code)%>%
  summarise(tmax_ym=mean(tmax))
  

tmax_sumr_mean<-tmax_l%>%
  filter(month==6 | month==7 | month==8)%>%
  dplyr::group_by(Year,site_code)%>%
  summarise(tmax_sm=mean(tmax))

#change back to wide format for yearly site covarites 
tmax_year_mean_w <- spread(tmax_year_mean,Year,tmax_ym)
tmax_sumr_mean_w <- spread(tmax_sumr_mean,Year,tmax_sm)

#paste var names with dplyr
tmax_sumr_mean_w<-tmax_sumr_mean_w%>%
  rename_with(~paste0("tmax_sumr_",.x),-site_code)

tmax_year_mean_w<-tmax_year_mean_w%>%
  rename_with(~paste0("tmax_year_",.x),-site_code)



# Min temperature monthly mean  -------------------------------------------




# load raster data
#mf <- file.path(datadir, "tmin_monavg_2015_ncss.nc")
setwd("~/Documents/Contract work/UNCG/2021/MYSE Project/GIS data/Grided data/DAYMET")
tmin_15<-brick("tmin_monavg_2015_ncss.nc")
tmin_16<-brick("tmin_monavg_2016_ncss.nc")
tmin_17<-brick("tmin_monavg_2017_ncss.nc")
tmin_18<-brick("tmin_monavg_2018_ncss.nc")
tmin_19<-brick("tmin_monavg_2019_ncss.nc")
tmin_20<-brick("tmin_monavg_2020_ncss.nc")

# to set the correct projection
raster::projection(tmin_15) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_16) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_17) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_18) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_19) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_20) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"


# reproject to lat lon
tmin_15 <- raster::projectRaster(tmin_15, crs = "+init=epsg:4326")
tmin_16 <- raster::projectRaster(tmin_16, crs = "+init=epsg:4326")
tmin_17 <- raster::projectRaster(tmin_17, crs = "+init=epsg:4326")
tmin_18 <- raster::projectRaster(tmin_18, crs = "+init=epsg:4326")
tmin_19 <- raster::projectRaster(tmin_19, crs = "+init=epsg:4326")
tmin_20 <- raster::projectRaster(tmin_20, crs = "+init=epsg:4326")



#extract values 
v_tmin_15 <- data.frame(extract(tmin_15, coords))
v_tmin_16 <- data.frame(extract(tmin_16, coords))
v_tmin_17 <- data.frame(extract(tmin_17, coords))
v_tmin_18 <- data.frame(extract(tmin_18, coords))
v_tmin_19 <- data.frame(extract(tmin_19, coords))
v_tmin_20 <- data.frame(extract(tmin_20, coords))


v_tmax_15%>%
  filter(is.na(.))#no NAs 


tmin_all<-cbind(v_tmin_15,
                v_tmin_16,
                v_tmin_17,
                v_tmin_18,
                v_tmin_19,
                v_tmin_20)


tmin_all$site_code<-S21$site_code

names(tmin_all) <- gsub(pattern = "X", replacement = "tmin_", x = names(tmin_all))
names(tmin_all) <- substring(names(tmin_all),1,12)


tmin_all <- tmin_all%>%
  relocate("site_code")


library(stringr)
# tmin_<-
#   tmin_all%>%
#   select_if(stringr::str_detect(names(.),".01") | stringr::str_detect(names(.),"site_code"))


#gather vars into long format

tmin_l <- gather(tmin_all,time,tmin,tmin_2015.01:tmin_2020.12, factor_key=TRUE)

#aggregate into yearly, quartly, seasonal precip

#add new cols for month and year

tmin_l$Year<-substring(tmin_l$time,6,9)
tmin_l$month<-substring(tmin_l$time,11,12)

#some formatting
#tmin_l$Year_d<-as.Date(tmin_l$Year,format="%Y")
tmin_l$Year_n<-as.numeric(tmin_l$Year)

#change month to numeric 
tmin_l$month<-as.numeric(tmin_l$month)

library(ggplot2)

ggplot(tmin_l,aes(month,tmin,fill=tmin))+
         geom_boxplot(aes(group=as.character(month)))+
  facet_wrap(~Year)+
  theme_pubr()
  

tmin_jan<-tmin_l%>%
  filter(month==1)%>%
  dplyr::select(c(-time,-month,-Year_n))

tmin_year_mean<-tmin_l%>%
  dplyr::group_by(Year,site_code)%>%
  summarise(tmin_ym=mean(tmin))


tmin_wint_mean<-tmin_l%>%
  filter(month==12 | month==1 | month==2)%>%
  dplyr::group_by(Year,site_code)%>%
  summarise(tmin_wn=mean(tmin))

#change back to wide format for yearly site covarites 
tmin_year_mean_w <- spread(tmin_year_mean,Year,tmin_ym)
tmin_wint_mean_w <- spread(tmin_wint_mean,Year,tmin_wn)
tmin_jan_w <- spread(tmin_jan,Year,tmin)

#paste var names with dplyr
tmin_wint_mean_w<-tmin_wint_mean_w%>%
  rename_with(~paste0("tmin_wint_",.x),-site_code)

tmin_year_mean_w<-tmin_year_mean_w%>%
  rename_with(~paste0("tmin_year_",.x),-site_code)

tmin_jan_w<-tmin_jan_w%>%
  rename_with(~paste0("tmin_jan_",.x),-site_code)

#add NAs as a placeholder for 2021 

#tmax vars
tmax_sumr_mean_w$tmax_sumr_2021<-NA
tmax_year_mean_w$tmax_year_2021<-NA
           tmax_08$tmax_2021.08<-NA

#tmin vars 
tmin_wint_mean_w$tmin_wint_2021<-NA
tmin_year_mean_w$tmin_year_2021<-NA
       tmin_jan_w$tmin_jan_2021<-NA
       
# setwd("~/github/MYSE_21/data/Daymet vars")     
# 
# write_csv(tmax_sumr_mean_w,"tmax_sumr_mean_w.csv")
# write_csv(tmax_year_mean_w,"tmax_year_mean_w.csv")
# write_csv(tmax_08,"tmax_08.csv")
# 
# write_csv(tmin_wint_mean_w,"tmin_wint_mean_w.csv")
# write_csv(tmin_year_mean_w,"tmin_year_mean_w.csv")
# write_csv(tmin_jan_w,"tmin_jan_w.csv")

       


