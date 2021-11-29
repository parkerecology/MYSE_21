###############################occupancy models NABat 2015-2021


# PESU Data formating ----------------------------------------------------------


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
buv<-buv[,c(1,2,17)]#just PESU

#merge bat data and covs
dfl<-merge(buv,dl,by=c("site_code","date"),all.y = TRUE)

#change to wide 
library(data.table)

#get rid of night vars 
names(dfl)
dfl<-dfl[,-c(5,7)]

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

dfl<-d[,c(1:6,16,17,8:15)]

dfl$tavg<-(dfl$tmax+dfl$tmin)/2

names(dfl)

dfl<-dfl[,c(1,3:8,17,9:16)]


#change to wide
dft<-
  data.table::dcast(setDT(dfl),site_code~visit,value.var=c(
    "PERSUB","tmax", "tmin", "tavg","J_date","prcp",
    "year","structure","canopy","water","elev","impervious","tree_canopy","dis_2_clutter"
  ))


library(skimr)

skim(dft)#take a look at the data 


#add site covarites
names(s21)
sc<-s21[,-c(1,6:7)]


#add yearly site covariates 
#cg_tmax <- read_csv("data/year_tmax_NClimGrid.csv")


#make unmarked dataframe
library(unmarked)
names(dft)

pesu_umf <- unmarkedMultFrame(y=as.data.frame(dft[,c(2:29)]),
                         siteCovs = data.frame(sc=sc),
                         obsCovs=list(
                           tmax=as.data.frame(dft[,c(30:57)]),
                           tmin=as.data.frame(dft[,c(58:85)]),
                           tavg=as.data.frame(dft[,c(86:113)]),
                           j_date=as.data.frame(dft[,c(114:141)]),
                           prcp=as.data.frame(dft[,c(142:169)]),
                           structure=as.data.frame(dft[,c(198:225)]),
                           canopy=as.data.frame(dft[,c(226:253)]),
                           water=as.data.frame(dft[,c(254:281)]),
                           elev=as.data.frame(dft[,c(282:309)]),
                           impervious=as.data.frame(dft[,c(310:337)]),
                           tree_canopy=as.data.frame(dft[,c(338:365)]),
                           dis_2_clutter=as.data.frame(dft[,c(366:393)])
                         ),
                         #yearlySiteCovs=list(
                         # year=as.data.frame(dft[,c(170:197)])),
                         numPrimary=7)
plot(pesu_umf)
summary(pesu_umf)


#set levels for refernce conditions 
levels(pesu_umf@obsCovs$structure)<-c("field","edge","corridor","interior")
levels(pesu_umf@siteCovs$sc.structure)<-c("field","edge","corridor","interior")
unique(pesu_umf@obsCovs$structure)
unique(pesu_umf@siteCovs$sc.structure)

levels(pesu_umf@obsCovs$canopy)<-c("closed","half","open")
levels(pesu_umf@siteCovs$sc.canopy)<-c("closed","half","open")
unique(pesu_umf@obsCovs$canopy)
unique(pesu_umf@siteCovs$sc.canopy)


# PESU colext Models ------------------------------------------------------------------


#null AIC 2421.53 
m.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.null)      

#J-date 
m.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date, data=pesu_umf)
summary(m.jdate)  

#J-date2 (wont run)  
m.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date+I(j_date^2), data=pesu_umf)
summary(m.jdate2)

#tmin 
m.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=pesu_umf)
summary(m.tmin)      

#tmin2 
m.tmin2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin+I(tmin^2), data=pesu_umf)
summary(m.tmin2) 


#tavg 
m.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=pesu_umf)
summary(m.tavg) 

#tavg 
m.tavg2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+I(tavg^2), data=pesu_umf)
summary(m.tavg2)

#tmax 
m.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=pesu_umf)
summary(m.tmax) 

#tmax2 
m.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=pesu_umf)
summary(m.tmax2) 

#structure 
m.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=pesu_umf)
summary(m.structure) 

#distance to clutter
m.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~dis_2_clutter, data=pesu_umf)
summary(m.dis_2_clutter) 


#canopy models
m.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=pesu_umf)
summary(m.canopy) 

m.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=pesu_umf)
summary(m.tree_canopy) 

m.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy+I(tree_canopy^2), data=pesu_umf)
summary(m.tree_canopy2) 

m.psi.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.psi.tree_canopy) 

m.psi.canopy<-colext(psiformula = ~sc.canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.psi.canopy) 

m.tree_canopy.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=pesu_umf)
summary(m.tree_canopy.tree_canopy) 

#Water models 

m.psi.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.psi.water) 

m.water.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=pesu_umf)
summary(m.water.water) 

m.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=pesu_umf)
summary(m.water) 

m.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.water.structure) 


## Water hypotheses: 
#Presence of a water feature will increase detection probability, predictions are:
#a: reduced clutter, test against structure only model (water wins)
library(AICcmodavg)

a.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(structure)"=m.structure,
                  "psi(.)gam(.)eps(.)p(water)"=m.water
)
modSel(a.models)

#b: some physical characteristics such as sound traveling more efficiently in higher humidity (not really testable without RH data)
#c: its occupancy probability not detection, test model with against occupancy model (not supported)

c.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(water)"=m.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.water.water
)

modSel(c.models)



#Canopy hypotheses:
#a: test which variable works better category or continuous (continuous)

ca.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.tree_canopy
)

modSel(ca.models)

#b: p or psi? or both? hmmm either via AIC, but not both. but if you look at the coefficients
#the SE for the PSI model is ~40 times the coefficient. So lets say tree canopy has a weak negative
#relationship with P, but that is probably better explained with the structure

cb.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.tree_canopy,
                   "psi(canopy)gam(.)eps(.)p(.)"=m.canopy,
                   "psi(tree canopy)gam(.)eps(.)p(.)"=m.tree_canopy,
                   "psi(tree canopy)gam(.)eps(.)p(tree canopy)"=m.tree_canopy.tree_canopy
)

modSel(cb.models)


#all p models
p.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(temp_low)"=m.tmin,
                  "psi(.)gam(.)eps(.)p(temp_low^2)"=m.tavg,
                  "psi(.)gam(.)eps(.)p(tmax)"=m.tmax,
                  "psi(.)gam(.)eps(.)p(structure)"=m.structure,
                  "psi(.)gam(.)eps(.)p(distance to clutter)"=m.dis_2_clutter,
                  "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                  "psi(.)gam(.)eps(.)p(water)"=m.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.water.water,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.water.structure
                  # "psi(.)gam(.)eps(.)p(global)"=m.p.global
)

modSel(p.models)


#Impervious hypothosis 

#PESU will have a negative association with increases in impervious substrate 

#A compare against p null model to see if it explains additional varitation

m.psi.impervious<-colext(psiformula = ~sc.impervious, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.psi.impervious) 

m.psi.structure<-colext(psiformula = ~sc.structure, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.psi.structure) 

i.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.water.structure,
                  "psi(.)gam(.)eps(.)p(impervious)"=m.psi.impervious,
                  "psi(.)gam(.)eps(.)p(structure)"=m.psi.structure)

modSel(i.models)

#impervious doesn't add anything for PESU 

#quick bayes occu estimate 
(re <- ranef(m.water.structure))

# Best Unbiased Predictors
occ_y<-data.frame(bup(re, stat="mean"))

occ_y<-plyr::rename(occ_y,c(
  "X1"="2015",
  "X2"="2016",
  "X3"="2017",
  "X4"="2018",
  "X5"="2019",
  "X6"="2020",
  "X7"="2021"
))

#add site name
pesu_op<-cbind(dft[,c(1)],occ_y)

#transform to long for plotting

tp<-gather(pesu_op,"year","PSI","2015":"2021",-site_code)

library(ggplot2)
library(ggpubr)

sp<-aggregate(PSI~year,tp,mean)

ggplot(sp,aes(year,PSI))+
  geom_point()+
  ylim(0,1)+
  theme_pubr()




# PESU Predictions -------------------------------------------------------------

pesu_p<-data.frame(expand.grid(
  structure=c("field","edge","corridor","interior"),
  water=unique(pesu_umf@obsCovs$water)))

dp<-predict(m.water.structure, type="det" , newdata=pesu_p, appendData=TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

#quick plot
ggplot(dp,aes(structure,Predicted,fill=water))+
  geom_point(aes(shape = water),position = pd)+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1,position=pd)+
  ylab("P")+
  ylim(0,1)+
  theme_pubr()+
  theme(axis.title.y = element_text(face = "italic"))



# EPFU Data formating ----------------------------------------------------------


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

names(buv)

#cut out species of interests
buv<-buv[,c(1,2,5)]#just EPFU

#merge bat data and covs
dfl<-merge(buv,dl,by=c("site_code","date"),all.y = TRUE)

#change to wide 
library(data.table)

#get rid of night vars 
names(dfl)
dfl<-dfl[,-c(5,7)]

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

dfl<-d[,c(1:6,16,17,8:15)]

dfl$tavg<-(dfl$tmax+dfl$tmin)/2

names(dfl)

dfl<-dfl[,c(1,3:8,17,9:16)]


#change to wide
dft<-
  data.table::dcast(setDT(dfl),site_code~visit,value.var=c(
    "EPTFUS","tmax", "tmin", "tavg","J_date","prcp",
    "year","structure","canopy","water","elev","impervious","tree_canopy","dis_2_clutter"
  ))


library(skimr)

skim(dft)#take a look at the data 


#add site covarites

names(s21)
sc<-s21[,-c(1,6:7)]


#add yearly site covariates 
#cg_tmax <- read_csv("data/year_tmax_NClimGrid.csv")


#make unmarked dataframe
library(unmarked)
names(dft)

epfu_umf <- unmarkedMultFrame(y=as.data.frame(dft[,c(2:29)]),
                              siteCovs = data.frame(sc=sc),
                              obsCovs=list(
                                tmax=as.data.frame(dft[,c(30:57)]),
                                tmin=as.data.frame(dft[,c(58:85)]),
                                tavg=as.data.frame(dft[,c(86:113)]),
                                j_date=as.data.frame(dft[,c(114:141)]),
                                prcp=as.data.frame(dft[,c(142:169)]),
                                structure=as.data.frame(dft[,c(198:225)]),
                                canopy=as.data.frame(dft[,c(226:253)]),
                                water=as.data.frame(dft[,c(254:281)]),
                                elev=as.data.frame(dft[,c(282:309)]),
                                impervious=as.data.frame(dft[,c(310:337)]),
                                tree_canopy=as.data.frame(dft[,c(338:365)]),
                                dis_2_clutter=as.data.frame(dft[,c(366:393)])
                              ),
                              #yearlySiteCovs=list(
                              # year=as.data.frame(dft[,c(170:197)])),
                              numPrimary=7)
plot(epfu_umf)
summary(epfu_umf)


#set levels for refernce conditions 
levels(epfu_umf@obsCovs$structure)<-c("field","edge","corridor","interior")
levels(epfu_umf@siteCovs$sc.structure)<-c("field","edge","corridor","interior")
unique(epfu_umf@obsCovs$structure)
unique(epfu_umf@siteCovs$sc.structure)

levels(epfu_umf@obsCovs$canopy)<-c("closed","half","open")
levels(epfu_umf@siteCovs$sc.canopy)<-c("closed","half","open")
unique(epfu_umf@obsCovs$canopy)
unique(epfu_umf@siteCovs$sc.canopy)


# EPFU colext Models ------------------------------------------------------------------


#null AIC 2421.53 
m.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=epfu_umf)
summary(m.null)      

#J-date 
m.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date, data=epfu_umf)
summary(m.jdate)  

#J-date2 (wont run)  
m.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date+I(j_date^2), data=epfu_umf)
summary(m.jdate2)

#tmin 
m.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=epfu_umf)
summary(m.tmin)      

#tmin2 
m.tmin2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin+I(tmin^2), data=epfu_umf)
summary(m.tmin2) 


#tavg 
m.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=epfu_umf)
summary(m.tavg) 

#tavg 
m.tavg2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+I(tavg^2), data=epfu_umf)
summary(m.tavg2)

#tmax 
m.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=epfu_umf)
summary(m.tmax) 

#tmax2 
m.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=epfu_umf)
summary(m.tmax2) 

#structure 
m.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=epfu_umf)
summary(m.structure) 

#distance to clutter
m.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~dis_2_clutter, data=epfu_umf)
summary(m.dis_2_clutter) 


#canopy models
m.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=epfu_umf)
summary(m.canopy) 

m.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=epfu_umf)
summary(m.tree_canopy) 

m.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy+I(tree_canopy^2), data=epfu_umf)
summary(m.tree_canopy2) 

m.psi.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=epfu_umf)
summary(m.psi.tree_canopy) 

m.psi.canopy<-colext(psiformula = ~sc.canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=epfu_umf)
summary(m.psi.canopy) 

m.tree_canopy.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=epfu_umf)
summary(m.tree_canopy.tree_canopy) 

#Water models 

m.psi.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=epfu_umf)
summary(m.psi.water) 

m.water.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=epfu_umf)
summary(m.water.water) 

m.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=epfu_umf)
summary(m.water) 

m.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=epfu_umf)
summary(m.water.structure) 


## Water hypotheses: 
#Presence of a water feature will increase detection probability, predictions are:
#a: reduced clutter, test against structure only model (water wins)
library(AICcmodavg)

a.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(structure)"=m.structure,
                  "psi(.)gam(.)eps(.)p(water)"=m.water
)
modSel(a.models)

#b: some physical characteristics such as sound traveling more efficiently in higher humidity (not really testable without RH data)
#c: its occupancy probability not detection, test model with against occupancy model (not supported)

c.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(water)"=m.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.water.water
)

modSel(c.models)



#Canopy hypotheses:
#a: test which variable works better category or continuous (continuous)

ca.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.tree_canopy
)

modSel(ca.models)

#b: p or psi? or both? hmmm either via AIC, but not both. but if you look at the coefficients
#the SE for the PSI model is ~40 times the coefficient. So lets say tree canopy has a weak negative
#relationship with P, but that is probably better explained with the structure

cb.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.tree_canopy,
                   "psi(canopy)gam(.)eps(.)p(.)"=m.psi.canopy,
                   "psi(tree canopy)gam(.)eps(.)p(.)"=m.tree_canopy,
                   "psi(tree canopy)gam(.)eps(.)p(tree canopy)"=m.tree_canopy.tree_canopy
)

modSel(cb.models)


#all p models
p.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(temp_low)"=m.tmin,
                  "psi(.)gam(.)eps(.)p(temp_low^2)"=m.tavg,
                  "psi(.)gam(.)eps(.)p(tmax)"=m.tmax,
                  "psi(.)gam(.)eps(.)p(structure)"=m.structure,
                  "psi(.)gam(.)eps(.)p(distance to clutter)"=m.dis_2_clutter,
                  "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                  "psi(.)gam(.)eps(.)p(water)"=m.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.water.water,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.water.structure
                  # "psi(.)gam(.)eps(.)p(global)"=m.p.global
)

modSel(p.models)


#Impervious hypothosis 

#PESU will have a negative association with increases in impervious substrate 

#A compare against p null model to see if it explains additional varitation

m.psi.impervious<-colext(psiformula = ~sc.impervious, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=epfu_umf)
summary(m.psi.impervious) 

m.psi.structure<-colext(psiformula = ~sc.structure, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=epfu_umf)
summary(m.psi.structure) 

i.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.water.structure,
                  "psi(.)gam(.)eps(.)p(impervious)"=m.psi.impervious,
                  "psi(.)gam(.)eps(.)p(structure)"=m.psi.structure)

modSel(i.models)

#impervious doesn't add anything for PESU 

#quick bayes occu estimate 
(re <- ranef(m.canopy))

# Best Unbiased Predictors
occ_y<-data.frame(bup(re, stat="mean"))

occ_y<-plyr::rename(occ_y,c(
  "X1"="2015",
  "X2"="2016",
  "X3"="2017",
  "X4"="2018",
  "X5"="2019",
  "X6"="2020",
  "X7"="2021"
))

#add site name
epfu_op<-cbind(dft[,c(1)],occ_y)

#transform to long for plotting

tp<-gather(epfu_op,"year","PSI","2015":"2021",-site_code)

library(ggplot2)
library(ggpubr)

sp<-aggregate(PSI~year,tp,mean)

ggplot(sp,aes(year,PSI))+
  geom_point()+
  ylim(0,1)+
  theme_pubr()



