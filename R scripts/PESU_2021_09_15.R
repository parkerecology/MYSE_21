####occupancy models NABAt 2015-2021


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

dl<-merge(dl,s21[,c(2:5,8:10)],by=c("site_code"),all.x=TRUE)
library(skimr)


#format bat data cols to match 
library(tidyverse)

buv<-plyr::rename(buv,c("site"="site_code" ))

#drop uneeded cols 
buv<-buv[,-c(1,4)]

#cut out species of interests
buv<-buv[,c(1,2,17)]#just PESU

#merge bat data and covs
dfl<-merge(buv,dl,by=c("site_code","date"),all.y = TRUE)

#change to wide 
library(data.table)

#get rid of night vars 
names(dfl)
dfl<-dfl[,-c(2,5,7)]

length(unique(dfl$site_code))#how many sites? 189


#change to wide
dft<-
  data.table::dcast(setDT(dfl),site_code~visit,value.var=c(
  "PERSUB","tmax", "tmin", "tavg","J_date","prcp","year","structure","canopy","water","elev","impervious","tree_canopy"
))


library(skimr)

skim(dft)#take a look at the data 


#add site covarites

names(s21)
sc<-s21[,-c(1,2,6:7)]


#add yearly site covariates 
#cg_tmax <- read_csv("data/year_tmax_NClimGrid.csv")


#make unmarked dataframe
library(unmarked)
names(dft)

umf <- unmarkedMultFrame(y=as.data.frame(dft[,c(2:29)]),
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
                           tree_canopy=as.data.frame(dft[,c(338:365)])
                         ),
                         #yearlySiteCovs=list(
                          # year=as.data.frame(dft[,c(170:197)])),
                         numPrimary=7)
plot(umf)
summary(umf)


#set levels for refernce conditions 
levels(umf@obsCovs$structure)<-c("field","edge","corridor","interior")
levels(umf@siteCovs$sc.structure)<-c("field","edge","corridor","interior")
unique(umf@obsCovs$structure)
unique(umf@siteCovs$sc.structure)

levels(umf@obsCovs$canopy)<-c("closed","half","open")
levels(umf@siteCovs$sc.canopy)<-c("closed","half","open")
unique(umf@obsCovs$canopy)
unique(umf@siteCovs$sc.canopy)


# Models ------------------------------------------------------------------



#null AIC 2421.53 
m.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=umf)
summary(m.null)      

#tmin AIC 2335
m.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=umf)
summary(m.tmin)      

#tavg AIC! TOO MANY NAs
m.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=umf)
summary(m.tavg) 

#tmax AIC 2333
m.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=umf)
summary(m.tmax) 

#tmax2 AIC 2333
m.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=umf)
summary(m.tmax2) 

#structure AIC 2333
m.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=umf)
summary(m.structure) 

#canopy models
m.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=umf)
summary(m.canopy) 

m.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=umf)
summary(m.tree_canopy) 

m.psi.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=umf)
summary(m.psi.tree_canopy) 

m.psi.canopy<-colext(psiformula = ~sc.canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=umf)
summary(m.psi.canopy) 

m.tree_canopy.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=umf)
summary(m.tree_canopy.tree_canopy) 

#Water models 

m.psi.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=umf)
summary(m.psi.water) 

m.water.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=umf)
summary(m.water.water) 

m.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=umf)
summary(m.water) 

m.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=umf)
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
#relationship with P
cb.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.tree_canopy,
                   "psi(canopy)gam(.)eps(.)p(.)"=m.canopy,
                   "psi(tree canopy)gam(.)eps(.)p(.)"=m.tree_canopy,
                   "psi(tree canopy)gam(.)eps(.)p(tree canopy)"=m.tree_canopy.tree_canopy
)

modSel(cb.models)

#c. tree canopy affects P via clutter, test against structure






p.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  #"psi(.)gam(.)eps(.)p(temp_low)"=m.tmin,
                  #"psi(.)gam(.)eps(.)p(temp_low^2)"=m.tavg,
                  #"psi(.)gam(.)eps(.)p(precip)"=m.tmax
                  "psi(.)gam(.)eps(.)p(structure)"=m.structure,
                  "psi(.)gam(.)eps(.)p(canopy)"=m.canopy,
                  "psi(.)gam(.)eps(.)p(water)"=m.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.water.water,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.water.structure
                  # "psi(.)gam(.)eps(.)p(global)"=m.p.global
)

modSel(p.models)




#quick bayes occu estimate 
(re <- ranef(m.water))

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
  



