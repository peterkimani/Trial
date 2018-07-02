#importing the dataset 
library(readstata13)
births<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
#extracting the variables that we need 
#reading only the variables that i am interested in 
#b2=year of birth 
#b5= child is alive 
#b6=age at death
#b11=preceeding birth interval (months)
#b12=succeeding birth intervals in months 
#m15= place of delivery 
#m19 = birth weight in kgs 
#m17 = delivery by ceaserian sections 
#m18 = size of child at birth 
#v001 = cluster number 
#v005= sample weight 
births<-births[ ,c( 'v001' , 'v005' , 'b2' , 'b5',"b6" , 'b11',"b12","m15","m19","m18","m17" ) ]

#limiting the sample to only children born between 2009 and 2014
births<-subset(births,b2%in%2009:2014)

#creating an child mortality variable 
births$status<-as.numeric(births$b5%in% "no")
 
#logistic regression for variables associated with children mortality 
library(FWDselect)
births$m15<-as.factor(births$m15)
x<-births[,c("b11","b12","m15","m19","m15","m17")]
y<-births$status
my_variables<-qselection(x,y,qvector=c(1:5),criterion = "aic",method = "glm",family = "binomial")


#the significant variables are b12,m19,b11,m17,m15
#extracting the variables 

library(dplyr)
f.data<-births %>% group_by(births$v001) %>% 
summarise(
  total_children=length(b5),
  total_observed_deaths=sum(b5=="no"),
  succeeding_birth_interval=mean(b12,na.rm=TRUE),
  birth_weight=mean(m19,na.rm=TRUE),
  precceeding_birth_interval=mean(b11,na.rm=TRUE),
  deliverary_by_ceaserian=sum(m17=="yes"),
  sample_weight=mean(v005)
)

#computing expected total number of child deaths 
f.data$expected_deaths<-f.data$total_children*((sum(f.data$total_observed_deaths)/sum(f.data$total_children)))
#estimating relative risk 
f.data$relative_risk<-f.data$total_observed_deaths/f.data$expected_deaths


#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
names(f.data)[1]<-"DHSCLUST"
library(dplyr)
sdata@data<-left_join(sdata@data,f.data,by="DHSCLUST")
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
County<-counties
#obtaining counts and averages based on the cells regions 
total_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
total_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
sbirth_interval<-aggregate(x=sdata["succeeding_birth_interval"],by=County,FUN=mean,na.rm=T)
pbirth_interval<-aggregate(x=sdata["precceeding_birth_interval"],by=County,FUN=mean,na.rm=T)
ceaserian_delivery<-aggregate(x=sdata["deliverary_by_ceaserian"],by=County,FUN=sum,na.rm=T)
birth_weight<-aggregate(x=sdata["birth_weight"],by=County,FUN=mean,na.rm=T)
sample_weight<-aggregate(x=sdata["sample_weight"],by=County,FUN =mean,na.rm=T)
#merging them into one dataset 
bcells<-cbind(total_children,total_deaths,sbirth_interval,pbirth_interval,ceaserian_delivery,
              birth_weight,sample_weight)
#adding id 
bcells$id<-County$id

#variable selection using FWD select 
library(FWDselect)
y<-bcells$total_observed_deaths
x<-bcells@data[,c("succeeding_birth_interval","precceeding_birth_interval","deliverary_by_ceaserian","birth_weight")]
my_pvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:3))
# all variables in the model are significant 


#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)
 
moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$succeeding_birth_interval,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$precceeding_birth_interval,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$deliverary_by_ceaserian,ww,randomisation = T,zero.policy = T,na.action = na.omit)

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
formulae.modela<-total_observed_deaths~1+succeeding_birth_interval+deliverary_by_ceaserian+precceeding_birth_interval
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model with covariates 
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+
   +birth_weight
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
#bym with smooth covariates 
result3<-inla(total_observed_deaths~succeeding_birth_interval+precceeding_birth_interval+deliverary_by_ceaserian+birth_weight+
                  f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))


#obtaining raw and fitted smr 
bcells$fittedsmr<-result3$summary.fitted.values$mean
plot(bcells$relative_risk,bcells$fittedsmr)
library(tmap)
qtm(bcells,fill=c("fittedsmr","relative_risk"))
library(sp)
spplot(bcells,z=c("fittedsmr","relative_risk"))


#
bcells$estimated_deaths<-bcells$fittedsmr*bcells$total_observed_deaths

#interpolating the fitted smr 
library(foreign)
library(prevR)
bounds<-create.boundary("Kenya")
numer<-bcells$fittedsmr
wdenom<-bcells$sample_weight/100000
wnumer<-bcells$fittedsmr*wdenom
mmm<-as.prevR(as.data.frame(),c(id="dhsclust",x="LONGNUM",Y="LATNUM",n="bcells$fittedsmr",wn="wdenom",pos="numer",wpos="wnumer"),bounds)
