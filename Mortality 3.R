
#importing the dataset 
library(readstata13)
household<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEHR71DT\\KEHR71FL.DTA",convert.factors = TRUE)
household<-household[,c("hv001","hv002","hv237","hv228")]


births<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
births<-births[,c("v001","v002","v005","v101","m17","m5","m19","v137","b11","b12","v106","m14","b0","b5","v107","v106","v131","bord","bidx","v208","v228","v501","b4")]

geographical<-read.csv("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEGC71FL\\KEGC71FL.csv")
geographical<-geographical[,c("DHSCLUST","Aridity","All_Population_Density_2010","Travel_Times","Nightlights_Composite","Growing_Season_Length")]
library(plyr)
names(household)[1]<-"v001"
names(household)[2]<-"v002"
datay<-merge(births,household,c("v001","v002"))
names(geographical)[1]<-"v001"
mydata<-merge(datay,geographical,by=c("v001"))
mydata$status<-as.numeric(mydata$b5%in% "no")
#selecting the variables that are significant with the dependent variable after performing chisquare test 
t<-c("v001","v005","Aridity","All_Population_Density_2010","v101","Travel_Times","Nightlights_Composite",
     "Growing_Season_Length","m17","m5","m19","v137","b11","b12","v106","m14","b0","b5","v107","v106","v131","hv237","hv228","bord","bidx","v208","v228","v501","b4","status")
trial<-mydata[,c(t)]
library(FWDselect)
x<-mydata[,c("Aridity","All_Population_Density_2010","v101","Travel_Times","Nightlights_Composite",
             "Growing_Season_Length","m17","m5","m19","v137","b11","b12","v106","b0","v107","v106","v131","hv237","hv228","bord","bidx","v208","v228","v501","b4")]#without m14
y<-mydata$status
ivariables<-qselection(x,y,qvector = c(1:24),criterion = 'aic',method = "glm",family = "binomial")
#the significant variables are b12, m5, v107, b11, v137, hv228
#mymodel<-glm(status~b12+m5+v107+b11+v137+hv228,data=trial,family = "binomial")
library(dplyr)
f.myvariables<-trial %>% group_by(trial$v001) %>% 
  summarise(
    total_children=length(b5),
    total_observed_deaths=sum(b5=="no"),
    v137=mean(v137,na.rm=TRUE),
    b12=mean(b12,na.rm=TRUE),
    m5=mean(m5,na.rm=TRUE),
    v107=mean(v107,na.rm=TRUE),
    b11=mean(b11,na.rm=TRUE),
    Aridity=mean(Aridity,na.rm=TRUE),
    All_Population_Density_2010=mean(All_Population_Density_2010,na.rm=TRUE),
    Travel_Times=mean(Travel_Times,na.rm=TRUE),
    Nightlights_Composite=mean(Nightlights_Composite,na.rm=TRUE),
    Growing_Season_Length=mean(Growing_Season_Length,na.rm=TRUE),
    m5=mean(m5,na.rm=TRUE),
    m19=mean(m19,na.rm=TRUE),
    b12=mean(b12,na.rm=TRUE),
    m14=mean(m14,na.rm=TRUE),
    v107=mean(v107,na.rm=TRUE),
    v106=mean(v106,na.rm=TRUE)

  )

#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
library(dplyr)
names(f.myvariables)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,f.myvariables,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
Division<-readOGR(".","Division")
location<-readOGR(".","Kenya__Administrative_Boundaries__Level_4")
bject1<-readOGR(".","kenya_divisions")
bject2<-readOGR(".","KEN_Adm2")
County<-bject2

#obtaining counts and averages based on the cells regions 
ftotal_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
ftotal_observed_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
fv137<-aggregate(x=sdata["v137"],by=County,FUN=mean,na.rm=T)
fTravel_Time<-aggregate(x=sdata["Travel_Times"],by=County,FUN=mean,na.rm=T)
fAridity<-aggregate(x=sdata["Aridity"],by=County,FUN=sum,na.rm=T)
fpop<-aggregate(x=sdata["All_Population_Density_2010"],by=County,FUN=mean,na.rm=T)
fnight<-aggregate(x=sdata["Nightlights_Composite"],by=County,FUN =mean,na.rm=T)
fgrowing<-aggregate(x=sdata["Growing_Season_Length"],by=County,FUN =mean,na.rm=T)
fm5<-aggregate(x=sdata["m5"],by=County,FUN =mean,na.rm=T)
fm19<-aggregate(x=sdata["m19"],by=County,FUN =mean,na.rm=T)
fm14<-aggregate(x=sdata["m14"],by=County,FUN =mean,na.rm=T)
fb11<-aggregate(x=sdata["b11"],by=County,FUN =mean,na.rm=T)
fb12<-aggregate(x=sdata["b12"],by=County,FUN =mean,na.rm=T)

#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,fv137,fTravel_Time,fAridity,fpop,fnight,
              fgrowing,fm5,fm19,fm14,fb11,fb12)
#adding id 
bcells$id<-County$id


#variable selection using FWD select 
library(FWDselect)
y<-bcells$total_observed_deaths
x<-bcells@data[,c("v137","Travel_Times","Aridity","All_Population_Density_2010","Nightlights_Composite","Growing_Season_Length","m5","m19","m14","b11","b12")]
my_pvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:10))
# all variables in the model are significant 


#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$Aridity,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m5,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b11,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b12,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$All_Population_Density_2010,ww,randomisation = T,zero.policy = T,na.action = na.omit)

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
formulae.modela<-total_observed_deaths~1+Aridity+b11+b12+All_Population_Density_2010
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model with covariates 
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+
  +Aridity+b11+b12+All_Population_Density_2010
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
#bym with smooth covariates 
result3<-inla(total_observed_deaths~Aridity+b11+b12+All_Population_Density_2010+
                f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))

