

###############  NABat COLEXT models 2015-2021 ###################


#load observation covs and presence absence data
setwd("~/github/MYSE_21")

library(readr)
obs <- read_csv("data/all_sp_umf.csv") 

#load site covs 
library(tidyverse)

s21 <- read_csv("data/sites_2015_2021.csv")
s21<-plyr::rename(s21,c("site"="site_code" ))
names(s21)

# drop vars not needed
sc<-s21%>%
  select(!c(id,Latitude,Longitude))

#sc<-s21[,-c(1,6:7)]#drop vars not needed by col number



# PESU detection models ---------------------------------------------------

#cut out PESU observations
names(obs)
pesu<-obs[,c(1,394:421,450:813)]
names(pesu)


#make unmarked dataframe
library(unmarked)

pesu_umf <- unmarkedMultFrame(y=as.data.frame(pesu[,c(2:29)]),
                              siteCovs = data.frame(sc=sc),
                              obsCovs=list(
                                tmax=as.data.frame(pesu[,c(30:57)]),
                                tmin=as.data.frame(pesu[,c(58:85)]),
                                tavg=as.data.frame(pesu[,c(86:113)]),
                                j_date=as.data.frame(pesu[,c(114:141)]),
                                prcp=as.data.frame(pesu[,c(142:169)]),
                                structure=as.data.frame(pesu[,c(198:225)]),
                                canopy=as.data.frame(pesu[,c(226:253)]),
                                water=as.data.frame(pesu[,c(254:281)]),
                                elev=as.data.frame(pesu[,c(282:309)]),
                                impervious=as.data.frame(pesu[,c(310:337)]),
                                tree_canopy=as.data.frame(pesu[,c(338:365)]),
                                dis_2_clutter=as.data.frame(pesu[,c(366:393)])
                              ),
                              #yearlySiteCovs=list(
                              # year=as.data.frame(pesu[,c(170:197)])),
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

#global model for c-hat
library(AICcmodavg)

m.pesu.global<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+j_date+structure+tree_canopy+water+elev+dis_2_clutter, data=pesu_umf)
summary(m.pesu.global)

#goodness of fit test (takes a long time to run ~4hrs)
#system.time(pesu.gof<-mb.gof.test(m.pesu.global,nsim = 1000))
#pesu.gof#c-hat 2.23

### Single covarites

#null 
m.pesu.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.pesu.null)      

#J-date 
m.pesu.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date, data=pesu_umf)
summary(m.pesu.jdate)  

#J-date2 (wont run)  
m.pesu.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date+I(j_date^2), data=pesu_umf)
summary(m.pesu.jdate2)

#tmin 
m.pesu.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=pesu_umf)
summary(m.pesu.tmin)      

#tmin2 
m.pesu.tmin2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin+I(tmin^2), data=pesu_umf)
summary(m.pesu.tmin2) 

#tavg 
m.pesu.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=pesu_umf)
summary(m.pesu.tavg) 

#tavg 2
m.pesu.tavg2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+I(tavg^2), data=pesu_umf)
summary(m.pesu.tavg2)

#tmax 
m.pesu.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=pesu_umf)
summary(m.pesu.tmax) 

#tmax2 
m.pesu.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=pesu_umf)
summary(m.pesu.tmax2) 

#structure 
m.pesu.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=pesu_umf)
summary(m.pesu.structure) 

#distance to clutter
m.pesu.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~dis_2_clutter, data=pesu_umf)
summary(m.pesu.dis_2_clutter) 

#canopy catagory
m.pesu.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=pesu_umf)
summary(m.pesu.canopy) 

#canopy continous
m.pesu.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=pesu_umf)
summary(m.pesu.tree_canopy) 

#canopy continous2 
m.pesu.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy+I(tree_canopy^2), data=pesu_umf)
summary(m.pesu.tree_canopy2) 

#Water 
m.pesu.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=pesu_umf)
summary(m.pesu.water) 

p.pesu.s.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                  "psi(.)gam(.)eps(.)p(jdate)"=m.pesu.jdate,
                  "psi(.)gam(.)eps(.)p(tree canopy)"=m.pesu.tree_canopy,
                  "psi(.)gam(.)eps(.)p(temp_low)"=m.pesu.tmin,
                  "psi(.)gam(.)eps(.)p(temp_avg)"=m.pesu.tavg,
                  "psi(.)gam(.)eps(.)p(tmax)"=m.pesu.tmax,
                  "psi(.)gam(.)eps(.)p(temp_low^2)"=m.pesu.tmin2,
                  "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.pesu.tavg2,
                  "psi(.)gam(.)eps(.)p(tmax^2)"=m.pesu.tmax2,
                  "psi(.)gam(.)eps(.)p(structure)"=m.pesu.structure,
                  "psi(.)gam(.)eps(.)p(distance to clutter)"=m.pesu.dis_2_clutter,
                  "psi(.)gam(.)eps(.)p(canopy)"=m.pesu.canopy,
                  "psi(.)gam(.)eps(.)p(water)"=m.pesu.water
)

modSel(p.pesu.s.models)


#### Additive detection covariates

#water + jdate
m.pesu.water.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + j_date, data=pesu_umf)
summary(m.pesu.water.jdate) 

#water + structure
m.pesu.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + structure, data=pesu_umf)
summary(m.pesu.water.structure) 

#water + tmin
m.pesu.water.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmin, data=pesu_umf)
summary(m.pesu.water.tmin) 

#water + tavg
m.pesu.water.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tavg, data=pesu_umf)
summary(m.pesu.water.tavg)

#water + tmax
m.pesu.water.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmax, data=pesu_umf)
summary(m.pesu.water.tmax)

#water + canopy
m.pesu.water.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + canopy, data=pesu_umf)
summary(m.pesu.water.canopy)

#water + tree canopy 
m.pesu.water.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tree_canopy, data=pesu_umf)
summary(m.pesu.water.tree_canopy)

#water + distance to clutter
m.pesu.water.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + dis_2_clutter, data=pesu_umf)
summary(m.pesu.water.dis_2_clutter)

#structure + tmax
m.pesu.structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + tmax, data=pesu_umf)
summary(m.pesu.structure.tmax)

#structure + jdate
m.pesu.structure.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + j_date, data=pesu_umf)
summary(m.pesu.structure.jdate)


#### Interactive detection covariates

#water +/x structure
m.pesu.water_structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.pesu.water_structure) 

#water +/x tree_canopy
m.pesu.water_tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*tree_canopy, data=pesu_umf)
summary(m.pesu.water_tree_canopy) 

#water +/x structure + tmax
m.pesu.water_structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure+tmax, data=pesu_umf)
summary(m.pesu.water_structure.tmax) 


p.pesu.models<-list("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                         "psi(.)gam(.)eps(.)p(jdate)"=m.pesu.jdate,
                         "psi(.)gam(.)eps(.)p(tree canopy)"=m.pesu.tree_canopy,
                         "psi(.)gam(.)eps(.)p(temp_low)"=m.pesu.tmin,
                         "psi(.)gam(.)eps(.)p(temp_avg)"=m.pesu.tavg,
                         "psi(.)gam(.)eps(.)p(tmax)"=m.pesu.tmax,
                         "psi(.)gam(.)eps(.)p(temp_low^2)"=m.pesu.tmin2,
                         "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.pesu.tavg2,
                         "psi(.)gam(.)eps(.)p(tmax^2)"=m.pesu.tmax2,
                         "psi(.)gam(.)eps(.)p(structure)"=m.pesu.structure,
                         "psi(.)gam(.)eps(.)p(distance to clutter)"=m.pesu.dis_2_clutter,
                         "psi(.)gam(.)eps(.)p(canopy)"=m.pesu.canopy,
                         "psi(.)gam(.)eps(.)p(water)"=m.pesu.water,
                       "psi(.)gam(.)eps(.)p(water+jdate)"=m.pesu.water.jdate,
                       "psi(.)gam(.)eps(.)p(water+structure)"=m.pesu.water.structure,
                       "psi(.)gam(.)eps(.)p(water+tmin)"=m.pesu.water.tmin,
                       "psi(.)gam(.)eps(.)p(water+tmax)"=m.pesu.water.tmax,
                       "psi(.)gam(.)eps(.)p(water+tavg)"=m.pesu.water.tavg,
                       "psi(.)gam(.)eps(.)p(water+clutter)"=m.pesu.water.dis_2_clutter,
                       "psi(.)gam(.)eps(.)p(water+canopy)"=m.pesu.water.canopy,
                       "psi(.)gam(.)eps(.)p(water+tree canopy)"=m.pesu.water.tree_canopy,
                       "psi(.)gam(.)eps(.)p(structure+jdate)"=m.pesu.structure.jdate,
                       "psi(.)gam(.)eps(.)p(structure+tmax)"=m.pesu.structure.tmax,
                       "psi(.)gam(.)eps(.)p(water +/x structure )"=m.pesu.water_structure,
                       "psi(.)gam(.)eps(.)p(water +/x structure + tmax)"=m.pesu.water_structure.tmax,
                       "psi(.)gam(.)eps(.)p(water +/x tree_canopy )"=m.pesu.water_tree_canopy
)

aictab(cand.set=p.pesu.models, c.hat=2.23)


## Water hypotheses: 
#Presence of a water feature will increase detection probability, predictions are:
#a: reduced clutter, test against structure only model (water wins)
library(AICcmodavg)

a.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                  "psi(.)gam(.)eps(.)p(structure)"=m.pesu.structure,
                  "psi(.)gam(.)eps(.)p(water)"=m.pesu.water
)
modSel(a.models)

#b: some physical characteristics such as sound traveling more efficiently in higher humidity (not really testable without RH data)
#c: its occupancy probability not detection, test model with against occupancy model (not supported)

c.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                  "psi(.)gam(.)eps(.)p(water)"=m.pesu.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.pesu.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.pesu.water.water
)

modSel(c.models)



#Canopy hypotheses:
#a: test which variable works better category or continuous (continuous)

ca.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.pesu.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.pesu.tree_canopy
)

modSel(ca.models)

#b: p or psi? or both? hmmm either via AIC, but not both. but if you look at the coefficients
#the SE for the PSI model is ~40 times the coefficient. So lets say tree canopy has a weak negative
#relationship with P, but that is probably better explained with the structure

cb.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                   "psi(.)gam(.)eps(.)p(canopy)"=m.pesu.canopy,
                   "psi(.)gam(.)eps(.)p(tree canopy)"=m.pesu.tree_canopy,
                   "psi(canopy)gam(.)eps(.)p(.)"=m.pesu.canopy,
                   "psi(tree canopy)gam(.)eps(.)p(.)"=m.pesu.tree_canopy,
                   "psi(tree canopy)gam(.)eps(.)p(tree canopy)"=m.pesu.tree_canopy.tree_canopy
)

modSel(cb.models)


#all p models
p.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                  "psi(.)gam(.)eps(.)p(temp_low)"=m.pesu.tmin,
                  "psi(.)gam(.)eps(.)p(temp_low^2)"=m.pesu.tavg,
                  "psi(.)gam(.)eps(.)p(tmax)"=m.pesu.tmax,
                  "psi(.)gam(.)eps(.)p(structure)"=m.pesu.structure,
                  "psi(.)gam(.)eps(.)p(distance to clutter)"=m.pesu.dis_2_clutter,
                  "psi(.)gam(.)eps(.)p(canopy)"=m.pesu.canopy,
                  "psi(.)gam(.)eps(.)p(water)"=m.pesu.water,
                  "psi(water)gam(.)eps(.)p(.)"=m.pesu.psi.water,
                  "psi(water)gam(.)eps(.)p(water)"=m.pesu.water.water,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.pesu.water.structure
                  # "psi(.)gam(.)eps(.)p(global)"=m.pesu.p.global
)

modSel(p.models)


#Impervious hypothosis 

#PESU will have a negative association with increases in impervious substrate 

#A compare against p null model to see if it explains additional varitation

m.pesu.psi.impervious<-colext(psiformula = ~sc.impervious, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.pesu.psi.impervious) 

m.pesu.psi.structure<-colext(psiformula = ~sc.structure, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=pesu_umf)
summary(m.pesu.psi.structure) 

i.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.pesu.null,
                  "psi(.)gam(.)eps(.)p(water +/x structure)"=m.pesu.water.structure,
                  "psi(.)gam(.)eps(.)p(impervious)"=m.pesu.psi.impervious,
                  "psi(.)gam(.)eps(.)p(structure)"=m.pesu.psi.structure)

modSel(i.models)








m.pesu.psi.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.pesu.psi.tree_canopy) 

m.pesu.psi.canopy<-colext(psiformula = ~sc.canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.pesu.psi.canopy) 

m.pesu.tree_canopy.tree_canopy<-colext(psiformula = ~sc.tree_canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=pesu_umf)
summary(m.pesu.tree_canopy.tree_canopy) 

m.pesu.psi.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=pesu_umf)
summary(m.pesu.psi.water) 

m.pesu.water.water<-colext(psiformula = ~sc.water, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=pesu_umf)
summary(m.pesu.water.water) 



