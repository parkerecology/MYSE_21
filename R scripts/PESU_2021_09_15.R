####occupancy models NABAt 2015-2021


#load weather data
library(readr)

#long data 
dl <- read_csv("~/Documents/Contract work/UNCG/2021/MYSE Project/Seq_visits_long.csv")

#bat data
buv <- read_csv("~/Documents/Contract work/UNCG/2021/MYSE Project/all_bats_unvetted.csv", 
                col_types = cols(date = col_date(format = "%Y%m%d")))

#other obs covs
s21 <- read_csv("data/sites_2015_2021.csv")
s21<-plyr::rename(s21,c("site"="site_code" ))



dl<-merge(dl,s21[,c(1:4)],by=c("site_code"),all.x=TRUE)
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

#change to wide
dft<-
  data.table::dcast(setDT(dfl),site_code~visit,value.var=c(
  "PERSUB","tmax", "tmin", "tavg","J_date","prcp","year","structure","canopy","water"
))

#make unmarked dataframe
library(unmarked)
names(dft)

umf <- unmarkedMultFrame(y=as.data.frame(dft[,c(2:29)]),
                         #siteCovs = data.frame(sc=sc),
                         obsCovs=list(
                           tmax=as.data.frame(dft[,c(30:57)]),
                           tmin=as.data.frame(dft[,c(58:85)]),
                           tavg=as.data.frame(dft[,c(86:113)]),
                           j_date=as.data.frame(dft[,c(114:141)]),
                           prcp=as.data.frame(dft[,c(142:169)]),
                           structure=as.data.frame(dft[,c(198:225)]),
                           canopy=as.data.frame(dft[,c(226:253)]),
                           water=as.data.frame(dft[,c(254:281)])
                         ),
                         #yearlySiteCovs=list(
                          # year=as.data.frame(dft[,c(170:197)])),
                         numPrimary=7)
plot(umf)
summary(umf)



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

library(AICcmodavg)

p.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.null,
                  "psi(.)gam(.)eps(.)p(temp_low)"=m.tmin,
                  #"psi(.)gam(.)eps(.)p(temp_low^2)"=m.tavg,
                  "psi(.)gam(.)eps(.)p(precip)"=m.tmax
                  # "psi(.)gam(.)eps(.)p(structure)"=m.p.structure,
                  # "psi(.)gam(.)eps(.)p(canopy)"=m.p.canopy,
                  # "psi(.)gam(.)eps(.)p(water)"=m.p.water,
                  # "psi(.)gam(.)eps(.)p(global)"=m.p.global
)

modSel(p.models)




#quick bayes occu estimate 
(re <- ranef(m.null))

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
  



