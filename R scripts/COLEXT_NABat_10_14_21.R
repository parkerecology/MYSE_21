

###############  NABat COLEXT models 2015-2021 ###################


#load observation covs and presence absence data
setwd("~/github/MYSE_21")

library(readr)
obs <- read_csv("data/all_sp_umf.csv") 

#load site covs 
library(tidyverse)
library(ggplot2)
library(ggpubr)


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
m.pesu.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~scale(j_date), data=pesu_umf)
summary(m.pesu.jdate)  

#J-date2 
m.pesu.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~scale(j_date)+I(scale(j_date)^2), data=pesu_umf)
summary(m.pesu.jdate2)

#J-date3 
m.pesu.jdate3<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~scale(j_date)+I(scale(j_date)^2)+I(scale(j_date)^3), data=pesu_umf)
summary(m.pesu.jdate3)


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
m.pesu.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~scale(tree_canopy)+I(scale(tree_canopy)^2), data=pesu_umf)
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

#canopy + jdate
m.pesu.canopy.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy + j_date, data=pesu_umf)
summary(m.pesu.canopy.jdate)


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
                         "psi(.)gam(.)eps(.)p(jdate2)"=m.pesu.jdate2,
                         "psi(.)gam(.)eps(.)p(jdate3)"=m.pesu.jdate3,
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

aictab(cand.set=p.pesu.models,second.ord = FALSE, c.hat=2.23)


pesu.p.top_m<-m.pesu.water.tavg


pesu_p<-data.frame(expand.grid(
  tavg=seq(min(na.omit(pesu_umf@obsCovs$tavg)),max(na.omit(pesu_umf@obsCovs$tavg))),
  water=unique(pesu_umf@obsCovs$water)))

dp<-predict(pesu.p.top_m, type="det" , newdata=pesu_p, appendData=TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

#quick plot
ggplot(dp,aes(tavg,Predicted,fill=water))+
  geom_point(aes(shape = water),position = pd)+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1,position=pd)+
  ylab("P")+
  xlab("Average nightly temperature C")+
  ylim(0,1)+
  ggtitle("PESU")+
  theme_pubr()+
  theme(axis.title.y = element_text(face = "italic"))





# EPFU detection models ---------------------------------------------------


#cut out epfu observations
names(obs)
epfu<-obs[,c(1,58:85,450:813)]
names(epfu)


#make unmarked dataframe
library(unmarked)

epfu_umf <- unmarkedMultFrame(y=as.data.frame(epfu[,c(2:29)]),
                              siteCovs = data.frame(sc=sc),
                              obsCovs=list(
                                tmax=as.data.frame(epfu[,c(30:57)]),
                                tmin=as.data.frame(epfu[,c(58:85)]),
                                tavg=as.data.frame(epfu[,c(86:113)]),
                                j_date=as.data.frame(epfu[,c(114:141)]),
                                prcp=as.data.frame(epfu[,c(142:169)]),
                                structure=as.data.frame(epfu[,c(198:225)]),
                                canopy=as.data.frame(epfu[,c(226:253)]),
                                water=as.data.frame(epfu[,c(254:281)]),
                                elev=as.data.frame(epfu[,c(282:309)]),
                                impervious=as.data.frame(epfu[,c(310:337)]),
                                tree_canopy=as.data.frame(epfu[,c(338:365)]),
                                dis_2_clutter=as.data.frame(epfu[,c(366:393)])
                              ),
                              #yearlySiteCovs=list(
                              # year=as.data.frame(epfu[,c(170:197)])),
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

#global model for c-hat
library(AICcmodavg)

m.epfu.global<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+j_date+structure+tree_canopy+water+elev+dis_2_clutter, data=epfu_umf)
summary(m.epfu.global)

#goodness of fit test (takes a long time to run ~4hrs)
#system.time(epfu.gof<-mb.gof.test(m.epfu.global,nsim = 1000))
#epfu.gof#c-hat 

### Single covarites

#null 
m.epfu.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=epfu_umf)
summary(m.epfu.null)      

#J-date 
m.epfu.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date, data=epfu_umf)
summary(m.epfu.jdate)  

#J-date2 (wont run)  
m.epfu.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date+I(j_date^2), data=epfu_umf)
summary(m.epfu.jdate2)

#tmin 
m.epfu.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=epfu_umf)
summary(m.epfu.tmin)      

#tmin2 
m.epfu.tmin2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin+I(tmin^2), data=epfu_umf)
summary(m.epfu.tmin2) 

#tavg 
m.epfu.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=epfu_umf)
summary(m.epfu.tavg) 

#tavg 2
m.epfu.tavg2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+I(tavg^2), data=epfu_umf)
summary(m.epfu.tavg2)

#tmax 
m.epfu.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=epfu_umf)
summary(m.epfu.tmax) 

#tmax2 
m.epfu.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=epfu_umf)
summary(m.epfu.tmax2) 

#structure 
m.epfu.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=epfu_umf)
summary(m.epfu.structure) 

#distance to clutter
m.epfu.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~dis_2_clutter, data=epfu_umf)
summary(m.epfu.dis_2_clutter) 

#canopy catagory
m.epfu.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=epfu_umf)
summary(m.epfu.canopy) 

#canopy continous
m.epfu.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=epfu_umf)
summary(m.epfu.tree_canopy) 

#canopy continous2 
m.epfu.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy+I(tree_canopy^2), data=epfu_umf)
summary(m.epfu.tree_canopy2) 

#Water 
m.epfu.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=epfu_umf)
summary(m.epfu.water) 

p.epfu.s.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.epfu.null,
                         "psi(.)gam(.)eps(.)p(jdate)"=m.epfu.jdate,
                         "psi(.)gam(.)eps(.)p(tree canopy)"=m.epfu.tree_canopy,
                         "psi(.)gam(.)eps(.)p(temp_low)"=m.epfu.tmin,
                         "psi(.)gam(.)eps(.)p(temp_avg)"=m.epfu.tavg,
                         "psi(.)gam(.)eps(.)p(tmax)"=m.epfu.tmax,
                         "psi(.)gam(.)eps(.)p(temp_low^2)"=m.epfu.tmin2,
                         "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.epfu.tavg2,
                         "psi(.)gam(.)eps(.)p(tmax^2)"=m.epfu.tmax2,
                         "psi(.)gam(.)eps(.)p(structure)"=m.epfu.structure,
                         "psi(.)gam(.)eps(.)p(distance to clutter)"=m.epfu.dis_2_clutter,
                         "psi(.)gam(.)eps(.)p(canopy)"=m.epfu.canopy,
                         "psi(.)gam(.)eps(.)p(water)"=m.epfu.water
)

modSel(p.epfu.s.models)


#### Additive detection covariates

#water + jdate
m.epfu.water.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + j_date, data=epfu_umf)
summary(m.epfu.water.jdate) 

#water + structure
m.epfu.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + structure, data=epfu_umf)
summary(m.epfu.water.structure) 

#water + tmin
m.epfu.water.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmin, data=epfu_umf)
summary(m.epfu.water.tmin) 

#water + tavg
m.epfu.water.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tavg, data=epfu_umf)
summary(m.epfu.water.tavg)

#water + tmax
m.epfu.water.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmax, data=epfu_umf)
summary(m.epfu.water.tmax)

#water + canopy
m.epfu.water.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + canopy, data=epfu_umf)
summary(m.epfu.water.canopy)

#water + tree canopy 
m.epfu.water.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tree_canopy, data=epfu_umf)
summary(m.epfu.water.tree_canopy)

#water + distance to clutter
m.epfu.water.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + dis_2_clutter, data=epfu_umf)
summary(m.epfu.water.dis_2_clutter)

#structure + tmax
m.epfu.structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + tmax, data=epfu_umf)
summary(m.epfu.structure.tmax)

#structure + jdate
m.epfu.structure.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + j_date, data=epfu_umf)
summary(m.epfu.structure.jdate)


#canopy + jdate
m.epfu.canopy.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy + j_date, data=epfu_umf)
summary(m.epfu.canopy.jdate)


#### Interactive detection covariates

#water +/x structure
m.epfu.water_structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=epfu_umf)
summary(m.epfu.water_structure) 

#water +/x tree_canopy
m.epfu.water_tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*tree_canopy, data=epfu_umf)
summary(m.epfu.water_tree_canopy) 

#water +/x structure + tmax
m.epfu.water_structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure+tmax, data=epfu_umf)
summary(m.epfu.water_structure.tmax) 


p.epfu.models<-list("psi(.)gam(.)eps(.)p(.)"=m.epfu.null,
                    "psi(.)gam(.)eps(.)p(jdate)"=m.epfu.jdate,
                    "psi(.)gam(.)eps(.)p(tree canopy)"=m.epfu.tree_canopy,
                    "psi(.)gam(.)eps(.)p(temp_low)"=m.epfu.tmin,
                    "psi(.)gam(.)eps(.)p(temp_avg)"=m.epfu.tavg,
                    "psi(.)gam(.)eps(.)p(tmax)"=m.epfu.tmax,
                    "psi(.)gam(.)eps(.)p(temp_low^2)"=m.epfu.tmin2,
                    "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.epfu.tavg2,
                    "psi(.)gam(.)eps(.)p(tmax^2)"=m.epfu.tmax2,
                    "psi(.)gam(.)eps(.)p(structure)"=m.epfu.structure,
                    "psi(.)gam(.)eps(.)p(distance to clutter)"=m.epfu.dis_2_clutter,
                    "psi(.)gam(.)eps(.)p(canopy)"=m.epfu.canopy,
                    "psi(.)gam(.)eps(.)p(water)"=m.epfu.water,
                    "psi(.)gam(.)eps(.)p(water+jdate)"=m.epfu.water.jdate,
                    "psi(.)gam(.)eps(.)p(water+structure)"=m.epfu.water.structure,
                    "psi(.)gam(.)eps(.)p(water+tmin)"=m.epfu.water.tmin,
                    "psi(.)gam(.)eps(.)p(water+tmax)"=m.epfu.water.tmax,
                    "psi(.)gam(.)eps(.)p(water+tavg)"=m.epfu.water.tavg,
                    "psi(.)gam(.)eps(.)p(water+clutter)"=m.epfu.water.dis_2_clutter,
                    "psi(.)gam(.)eps(.)p(water+canopy)"=m.epfu.water.canopy,
                    "psi(.)gam(.)eps(.)p(water+tree canopy)"=m.epfu.water.tree_canopy,
                    "psi(.)gam(.)eps(.)p(structure+jdate)"=m.epfu.structure.jdate,
                    "psi(.)gam(.)eps(.)p(canopy+jdate)"=m.epfu.canopy.jdate,
                    "psi(.)gam(.)eps(.)p(structure+tmax)"=m.epfu.structure.tmax,
                    "psi(.)gam(.)eps(.)p(water +/x structure )"=m.epfu.water_structure,
                    "psi(.)gam(.)eps(.)p(water +/x structure + tmax)"=m.epfu.water_structure.tmax,
                    "psi(.)gam(.)eps(.)p(water +/x tree_canopy )"=m.epfu.water_tree_canopy
)

aictab(cand.set=p.epfu.models,second.ord = FALSE, c.hat=2.61)


epfu.p.top_m<-m.epfu.canopy.jdate

epfu_p<-data.frame(expand.grid(
  j_date=seq(min(na.omit(epfu_umf@obsCovs$j_date)),max(na.omit(epfu_umf@obsCovs$j_date))),
  canopy=unique(epfu_umf@obsCovs$canopy)))

dp<-predict(epfu.p.top_m, type="det" , newdata=epfu_p, appendData=TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

#quick plot
ggplot(dp,aes(j_date,Predicted,fill=canopy))+
  geom_line(aes(linetype = canopy),position = pd)+
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.5,position=pd)+
  ylab("P")+
  xlab("Julian date")+
  ylim(0,1)+
  ggtitle("EPFU")+
  theme_pubr()+
  scale_fill_grey()+
  theme(axis.title.y = element_text(face = "italic"))



# MYSE detection models --------------------------------------------------------

#cut out myse observations
names(obs)
myse<-obs[,c(1,310:337,450:813)]
names(myse)


#make unmarked dataframe
library(unmarked)

myse_umf <- unmarkedMultFrame(y=as.data.frame(myse[,c(2:29)]),
                              siteCovs = data.frame(sc=sc),
                              obsCovs=list(
                                tmax=as.data.frame(myse[,c(30:57)]),
                                tmin=as.data.frame(myse[,c(58:85)]),
                                tavg=as.data.frame(myse[,c(86:113)]),
                                j_date=as.data.frame(myse[,c(114:141)]),
                                prcp=as.data.frame(myse[,c(142:169)]),
                                structure=as.data.frame(myse[,c(198:225)]),
                                canopy=as.data.frame(myse[,c(226:253)]),
                                water=as.data.frame(myse[,c(254:281)]),
                                elev=as.data.frame(myse[,c(282:309)]),
                                impervious=as.data.frame(myse[,c(310:337)]),
                                tree_canopy=as.data.frame(myse[,c(338:365)]),
                                dis_2_clutter=as.data.frame(myse[,c(366:393)])
                              ),
                              #yearlySiteCovs=list(
                              # year=as.data.frame(myse[,c(170:197)])),
                              numPrimary=7)
plot(myse_umf)
summary(myse_umf)


#set levels for refernce conditions 
levels(myse_umf@obsCovs$structure)<-c("field","edge","corridor","interior")
levels(myse_umf@siteCovs$sc.structure)<-c("field","edge","corridor","interior")
unique(myse_umf@obsCovs$structure)
unique(myse_umf@siteCovs$sc.structure)

levels(myse_umf@obsCovs$canopy)<-c("closed","half","open")
levels(myse_umf@siteCovs$sc.canopy)<-c("closed","half","open")
unique(myse_umf@obsCovs$canopy)
unique(myse_umf@siteCovs$sc.canopy)

#global model for c-hat
library(AICcmodavg)

m.myse.global<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+j_date+structure+tree_canopy+water+elev+dis_2_clutter, data=myse_umf)
summary(m.myse.global)

#goodness of fit test (takes a long time to run ~4hrs)
#system.time(myse.gof<-mb.gof.test(m.myse.global,nsim = 1000))
#myse.gof#c-hat 1.51

### Single covarites

#null 
m.myse.null<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=myse_umf)
summary(m.myse.null)      

#J-date 
m.myse.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date, data=myse_umf)
summary(m.myse.jdate)  

#J-date2 (wont run)  
m.myse.jdate2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~j_date+I(j_date^2), data=myse_umf)
summary(m.myse.jdate2)

#tmin 
m.myse.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin, data=myse_umf)
summary(m.myse.tmin)      

#tmin2 
m.myse.tmin2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmin+I(tmin^2), data=myse_umf)
summary(m.myse.tmin2) 

#tavg 
m.myse.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg, data=myse_umf)
summary(m.myse.tavg) 

#tavg 2
m.myse.tavg2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tavg+I(tavg^2), data=myse_umf)
summary(m.myse.tavg2)

#tmax 
m.myse.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax, data=myse_umf)
summary(m.myse.tmax) 

#tmax2 
m.myse.tmax2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tmax+I(tmax^2), data=myse_umf)
summary(m.myse.tmax2) 

#structure 
m.myse.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure, data=myse_umf)
summary(m.myse.structure) 

#distance to clutter
m.myse.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~dis_2_clutter, data=myse_umf)
summary(m.myse.dis_2_clutter) 

#canopy catagory
m.myse.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy, data=myse_umf)
summary(m.myse.canopy) 

#PSI canopy catagory 
m.myse.psicanopy<-colext(psiformula = ~sc.canopy, gammaformula = ~1, epsilonformula = ~1, pformula = ~1, data=myse_umf)
summary(m.myse.psicanopy) 


#canopy continous
m.myse.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy, data=myse_umf)
summary(m.myse.tree_canopy) 

#canopy continous2 
m.myse.tree_canopy2<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~tree_canopy+I(tree_canopy^2), data=myse_umf)
summary(m.myse.tree_canopy2) 

#Water 
m.myse.water<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water, data=myse_umf)
summary(m.myse.water) 

p.myse.s.models<-fitList("psi(.)gam(.)eps(.)p(.)"=m.myse.null,
                         "psi(.)gam(.)eps(.)p(jdate)"=m.myse.jdate,
                         "psi(.)gam(.)eps(.)p(tree canopy)"=m.myse.tree_canopy,
                         "psi(.)gam(.)eps(.)p(temp_low)"=m.myse.tmin,
                         "psi(.)gam(.)eps(.)p(temp_avg)"=m.myse.tavg,
                         "psi(.)gam(.)eps(.)p(tmax)"=m.myse.tmax,
                         "psi(.)gam(.)eps(.)p(temp_low^2)"=m.myse.tmin2,
                         "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.myse.tavg2,
                         "psi(.)gam(.)eps(.)p(tmax^2)"=m.myse.tmax2,
                         "psi(.)gam(.)eps(.)p(structure)"=m.myse.structure,
                         "psi(.)gam(.)eps(.)p(distance to clutter)"=m.myse.dis_2_clutter,
                         "psi(.)gam(.)eps(.)p(canopy)"=m.myse.canopy,
                         "psi(.)gam(.)eps(.)p(water)"=m.myse.water
)

modSel(p.myse.s.models)


#### Additive detection covariates

#water + jdate
m.myse.water.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + j_date, data=myse_umf)
summary(m.myse.water.jdate) 

#water + structure
m.myse.water.structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + structure, data=myse_umf)
summary(m.myse.water.structure) 

#water + tmin
m.myse.water.tmin<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmin, data=myse_umf)
summary(m.myse.water.tmin) 

#water + tavg
m.myse.water.tavg<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tavg, data=myse_umf)
summary(m.myse.water.tavg)

#water + tmax
m.myse.water.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tmax, data=myse_umf)
summary(m.myse.water.tmax)

#water + canopy
m.myse.water.canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + canopy, data=myse_umf)
summary(m.myse.water.canopy)

#water + tree canopy 
m.myse.water.tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + tree_canopy, data=myse_umf)
summary(m.myse.water.tree_canopy)

#water + distance to clutter
m.myse.water.dis_2_clutter<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water + dis_2_clutter, data=myse_umf)
summary(m.myse.water.dis_2_clutter)

#structure + tmax
m.myse.structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + tmax, data=myse_umf)
summary(m.myse.structure.tmax)

#structure + jdate
m.myse.structure.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~structure + j_date, data=myse_umf)
summary(m.myse.structure.jdate)


#canopy + jdate
m.myse.canopy.jdate<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~canopy + j_date, data=myse_umf)
summary(m.myse.canopy.jdate)


#### Interactive detection covariates

#water +/x structure
m.myse.water_structure<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure, data=myse_umf)
summary(m.myse.water_structure) 

#water +/x tree_canopy
m.myse.water_tree_canopy<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*tree_canopy, data=myse_umf)
summary(m.myse.water_tree_canopy) 

#water +/x structure + tmax
m.myse.water_structure.tmax<-colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1, pformula = ~water*structure+tmax, data=myse_umf)
summary(m.myse.water_structure.tmax) 


p.myse.models<-list("psi(.)gam(.)eps(.)p(.)"=m.myse.null,
                    "psi(.)gam(.)eps(.)p(jdate)"=m.myse.jdate,
                    "psi(.)gam(.)eps(.)p(tree canopy)"=m.myse.tree_canopy,
                    "psi(.)gam(.)eps(.)p(temp_low)"=m.myse.tmin,
                    "psi(.)gam(.)eps(.)p(temp_avg)"=m.myse.tavg,
                    "psi(.)gam(.)eps(.)p(tmax)"=m.myse.tmax,
                    "psi(.)gam(.)eps(.)p(temp_low^2)"=m.myse.tmin2,
                    "psi(.)gam(.)eps(.)p(temp_avg^2)"=m.myse.tavg2,
                    "psi(.)gam(.)eps(.)p(tmax^2)"=m.myse.tmax2,
                    "psi(.)gam(.)eps(.)p(structure)"=m.myse.structure,
                    "psi(.)gam(.)eps(.)p(distance to clutter)"=m.myse.dis_2_clutter,
                    "psi(.)gam(.)eps(.)p(canopy)"=m.myse.canopy,
                    "psi(.)gam(.)eps(.)p(water)"=m.myse.water,
                    "psi(.)gam(.)eps(.)p(water+jdate)"=m.myse.water.jdate,
                    "psi(.)gam(.)eps(.)p(water+structure)"=m.myse.water.structure,
                    "psi(.)gam(.)eps(.)p(water+tmin)"=m.myse.water.tmin,
                    "psi(.)gam(.)eps(.)p(water+tmax)"=m.myse.water.tmax,
                    "psi(.)gam(.)eps(.)p(water+tavg)"=m.myse.water.tavg,
                    "psi(.)gam(.)eps(.)p(water+clutter)"=m.myse.water.dis_2_clutter,
                    "psi(.)gam(.)eps(.)p(water+canopy)"=m.myse.water.canopy,
                    "psi(.)gam(.)eps(.)p(water+tree canopy)"=m.myse.water.tree_canopy,
                    "psi(.)gam(.)eps(.)p(structure+jdate)"=m.myse.structure.jdate,
                    "psi(.)gam(.)eps(.)p(canopy+jdate)"=m.myse.canopy.jdate,
                    "psi(.)gam(.)eps(.)p(structure+tmax)"=m.myse.structure.tmax,
                    "psi(.)gam(.)eps(.)p(water +/x structure )"=m.myse.water_structure,
                    "psi(.)gam(.)eps(.)p(water +/x structure + tmax)"=m.myse.water_structure.tmax,
                    "psi(.)gam(.)eps(.)p(water +/x tree_canopy )"=m.myse.water_tree_canopy
)

aictab(cand.set=p.myse.models,second.ord = FALSE, c.hat=1.51)

#aictab(cand.set = c(m.myse.canopy,m.myse.psicanopy,m.myse.null),second.ord = FALSE, c.hat=1.51)

myse.p.top_m<-m.myse.canopy


myse_p<-data.frame(expand.grid(
  canopy=unique(myse_umf@obsCovs$canopy)))

dp<-predict(myse.p.top_m, type="det" , newdata=myse_p, appendData=TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

#quick plot
ggplot(dp,aes(canopy,Predicted,shape=canopy))+
  geom_point(aes(shape = canopy),size=4,position = pd)+
  geom_point(position = pd,size=2.5,color="white")+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=0.05,position=pd)+
  ylab("P")+
  xlab("")+
  ylim(0,1)+
  ggtitle("MYSE")+
  theme_pubr()+
  theme(legend.position = "None")+
  theme(axis.title.y = element_text(face = "italic"))







# Model selection with dredge ---------------------------------------------

#testing dredge
library(MuMIn)

pesu.d<-dredge(m.pesu.global,rank = "QAIC", chat = 2.23, fixed = "p(Int)")
epfu.d<-dredge(m.epfu.global,rank = "QAIC", chat = 2.61, fixed = "p(Int)")
myse.d<-dredge(m.myse.global,rank = "QAIC", chat = 1.51, fixed = "p(Int)")
























