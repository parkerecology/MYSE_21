#load weather data
library(readr)

#long data 
dl <- read_csv("data/Seq_visits_long.csv")

length(unique(dl$site_code))#how many sites? 189

#bat data
buv <- read_csv("data/all_bats_unvetted.csv", 
                col_types = cols(date = col_date(format = "%Y%m%d")))

length(unique(buv$site))#how many sites? 189

#other obs covs
s21 <- read_csv("data/sites_2015_2021.csv")
s21<-plyr::rename(s21,c("site"="site_code" ))

names(s21)

dl<-merge(dl,s21[,c(2:5,8:11)],by=c("site_code"),all.x=TRUE)
library(skimr)


#format bat data cols to match 
library(tidyverse)

buv<-plyr::rename(buv,c("site"="site_code" ))

#drop un-needed cols 
buv<-buv[,-c(1,4)]

#cut out species of interests
#buv<-buv[,c(1,2,17)]#just PESU

#merge bat data and covs
dfl<-merge(buv,dl,by=c("site_code","date"),all.y = TRUE)

#change to wide 
library(data.table)

#get rid of night vars 
names(dfl)
dfl<-dfl[,-c(20,22)]

length(unique(dfl$site_code))#how many sites? 189

#add in replacements for RNOAA NAs
fin <- read_csv("data/na_sites_fin.csv", col_types = cols(date = col_date(format = "%m/%d/%y")))
names(fin)

library(weathermetrics)

fin$tmax<-fahrenheit.to.celsius(fin$tmax)
fin$tmin<-fahrenheit.to.celsius(fin$tmin)

#join in the replacement NA data
c <- left_join(dfl,fin[,c(1,7,10)],by=c("site_code","date")) %>% # this will generate tmax.x and tmax.y
  mutate(tmax = ifelse(is.na(tmax.x), tmax.y, tmax.x)) %>% # we generate a joint 'tmax' variable
  select(-tmax.y, -tmax.x) # drop the superfluous columns

d <- left_join(c,fin[,c(1,7,11)],by=c("site_code","date")) %>% # this will generate tmin.x and tmin.y
  mutate(tmin = ifelse(is.na(tmin.x), tmin.y, tmin.x)) %>% # we generate a joint 'tmin' variable
  select(-tmin.y, -tmin.x) # drop the superfluous columns

names(d)

dfl<-d[,c(1:21,31,32,23:30)]

dfl$tavg<-(dfl$tmax+dfl$tmin)/2

names(dfl)

dfl<-dfl[,c(1,3:21,32,22:31)]





#change to wide
dft<-
  data.table::dcast(setDT(dfl),site_code~visit,value.var=c(
    "CORRAF","CORTOW","EPTFUS","LASBOR","LASCIN","LASINT","LASNOC","MYOAUS",
    "MYOGRI","MYOLEI","MYOLUC","MYOSEP","MYOSOD","NYCHUM","PERSUB","TADBRA","tmax", "tmin", "tavg","J_date","prcp",
    "year","structure","canopy","water","elev","impervious","tree_canopy","dis_2_clutter"
  ))


names(dft)


setwd("~/github/MYSE_21/data")


#write_csv(dft,"all_sp_umf.csv")




