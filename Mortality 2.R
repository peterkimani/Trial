#importing the dataset 
library(readstata13)
births<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
#selecting the variables that we want 
t<-c("v001","v005","v137","v152","v190","v101","v208","v228","v501","v714","bidx","b0","b2","b5","b9",
     "v113","v116","v119","v130","v131","v106","v149","v151","v101","v102","v010","v012","v212","bord","v525","b4","v212","m18","m15")
myvariables<-births[,c(t)]
#creating an child mortality variable 
myvariables$status<-as.numeric(myvariables$b5%in% "no")

#testing chisquare test 
library(dplyr)
myvariables %>% 
  summarise_each(funs(chisq.test(., 
                                 myvariables$status)$p.value), -one_of("status"))

#Selecting the best variables in the models 
#after performing chisquare test the significant variables based on chisquare are 
b<-c("v137","v208","v228","v501","bidx","b0","b2","b9",
       "v131","v106","v149","v101","v212","bord","b4","v212","m18")
library(FWDselect)
x<-myvariables[,c(b)]
y<-myvariables$status
output1<-qselection(x,y,qvector = c(1:5),criterion = "aic",method = "glm",family = "binomial",cluster = T,control=list(maxit=50))

library(dplyr)
f.myvariables<-myvariables %>% group_by(myvariables$v001) %>% 
  summarise(
    total_children=length(b5),
    total_observed_deaths=sum(b5=="no"),
    v137=mean(v137,na.rm=TRUE),
    v208=mean(v208,na.rm=TRUE),
    bidx=mean(bidx,na.rm=TRUE),
    bord=mean(bord,na.rm=TRUE),
    #v228=sum(v228=="yes"),
    v501=(sum(v501=="married"|v501=="living with partner")/length(b5)),
    b4=(sum(b4=="male")/length(b5))
  )

#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
names(f.data)[1]<-"DHSCLUST"
library(dplyr)
names(f.myvariables)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,f.myvariables,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
Division<-readOGR(".","Division")
location<-readOGR(".","Kenya__Administrative_Boundaries__Level_4")
bject1<-readOGR(".","kenya_divisions")
bject2<-readOGR(".","KEN_Adm2")
#dividing the region into cells 
library(raster)
e<-extent(bbox(counties))
r<-raster(e)
dim(r)<-c(20,20)
projection(r)<-CRS(proj4string(counties))
tomap<-as(r,'SpatialPolygonsDataFrame')
tomap$id<-cbind(c(1:dim(tomap)[1]))
threemap<-tomap[counties,]
plot(threemap)
County<-bject2
#obtaining counts and averages based on the cells regions 
ftotal_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
ftotal_observed_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
fv137<-aggregate(x=sdata["v137"],by=County,FUN=mean,na.rm=T)
fv208<-aggregate(x=sdata["v208"],by=County,FUN=mean,na.rm=T)
fbidx<-aggregate(x=sdata["bidx"],by=County,FUN=sum,na.rm=T)
fbord<-aggregate(x=sdata["bord"],by=County,FUN=mean,na.rm=T)
fv501<-aggregate(x=sdata["v501"],by=County,FUN =mean,na.rm=T)
fb4<-aggregate(x=sdata["b4"],by=County,FUN =mean,na.rm=T)



#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,fv137,fv208,fbidx,fbord,fv501,fb4)
#adding id 
bcells$id<-County$id

#variable selection using FWD select 
library(FWDselect)
y<-bcells$total_observed_deaths
x<-bcells@data[,c("v137","v208","bidx","bord","v501","b4")]
my_pvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:5))
# all variables in the model are significant 


#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v137,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v208,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$bidx,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$bord,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v501,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b4,ww,randomisation = T,zero.policy = T,na.action = na.omit)#not significant 
#using inla to model the results 
library(INLA)
#computing the expected valunes 
bcells$expected_deaths<-bcells$total_children*(sum(bcells$total_observed_deaths,na.rm = T)/sum(bcells$total_children,na.rm=T))
bcells$relative_risk<-bcells$total_observed_deaths/bcells$expected_deaths
#adding replicating ID 
bcells$id2<-bcells$id
bcells$tid<-c(1:dim(bcells)[1])
bcells$tid2<-bcells$tid
ww.nb<-poly2nb(bcells,row.names=bcells$tid)
nb2INLA("ww.adj",ww.nb)
kenya<-data.frame(bcells)

#running bayesian poisson model 
formulae.modela<-total_observed_deaths~1+v137+v208+bord+v501
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model with covariates 
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+
  +v137+v208+bord+v501
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
#bym with smooth covariates 
result3<-inla(total_observed_deaths~v137+v208+bord+v501+
                f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))


#obtaining raw and fitted smr 
bcells$fittedsmr<-result3$summary.fitted.values$mean
plot(bcells$relative_risk,bcells$fittedsmr)
library(tmap)
qtm(bcells,fill=c("fittedsmr","relative_risk"))
library(sp)
spplot(bcells,z=c("fittedsmr"))
bcells$diff<-bcells$relative_risk-bcells$fittedsmr
spplot(bcells,z=c("diff"))

