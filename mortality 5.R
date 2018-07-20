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
trial$v101<-as.numeric(trial$v101)
trial$v131<-as.numeric(trial$v131)
trial$v228<-as.numeric(trial$v228%in% "yes")


#summing up the variables 
library(dplyr)
good.luck<-trial %>% group_by(trial$v001) %>% 
  summarise(
    total_children=length(b5),
    total_observed_deaths=sum(b5=="no"),
    Aridity=mean(Aridity,na.rm = T),
    All_Population_Density_2010=mean(All_Population_Density_2010,na.rm = T),
    Travel_Times=mean(Travel_Times,na.rm = T),
    v101=mean(v101,na.rm = T),
    Growing_Season_Length=mean(Growing_Season_Length,na.rm = T),
    m5=mean(m5,na.rm=T),
    m19=mean(m19,na.rm = T),
    v137=mean(v137,na.rm = T),
    b11=mean(b11,na.rm = T),
    b12=mean(b12,na.rm = T),
    v107=mean(v107,na.rm = T),
    bord=mean(bord,na.rm = T),
    bidx=mean(bidx,na.rm = T),
    v208=mean(v208,na.rm = T),
    v131=mean(v131,na.rm = T),
    ceaserian=sum(m17=="yes"),
    twin=sum(b0=="single birth")/length(b5),
    no_education=sum(v106=="no education")/length(b5),
    access_towater_yes=sum(hv237=="yes")/length(b5),
    disease_environment=sum(hv228=="all children")/length(b5),
    ever_terminated_preganancy=sum(v228,na.rm = T)/length(b5),
    married=sum(v501=="married")/length(b5),
    sex_child=sum(b4=="female")/length(b5))
    
#checking colinearlity 
library(mctest)
x<-good.luck[,-c(1,2,3)]
y<-good.luck$total_observed_deaths
imcdiag(x,y,all=T,method = 'VIF')
#multicorlinearlity in bidx and v208. removing bidx
good.luck1<-good.luck[,-c(16)]

#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
library(dplyr)
names(good.luck1)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,good.luck1,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
bject2<-readOGR(".","KEN_Adm2")
County<-counties


#obtaining counts and averages based on the cells regions 
ftotal_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
ftotal_observed_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
faridity<-aggregate(x=sdata["Aridity"],by=County,FUN=sum,na.rm=T)
fpop<-aggregate(x=sdata["All_Population_Density_2010"],by=County,FUN=sum,na.rm=T)
fv101<-aggregate(x=sdata["v101"],by=County,FUN=sum,na.rm=T)
ftravel<-aggregate(x=sdata["Travel_Times"],by=County,FUN=sum,na.rm=T)
fgrow<-aggregate(x=sdata["Growing_Season_Length"],by=County,FUN=sum,na.rm=T)
fm5<-aggregate(x=sdata["m5"],by=County,FUN =mean,na.rm=T)
fm19<-aggregate(x=sdata["m19"],by=County,FUN =mean,na.rm=T)
fv137<-aggregate(x=sdata["v137"],by=County,FUN=sum,na.rm=T)
fb11<-aggregate(x=sdata["b11"],by=County,FUN =mean,na.rm=T)
fb12<-aggregate(x=sdata["b12"],by=County,FUN =mean,na.rm=T)
fv107<-aggregate(x=sdata["v107"],by=County,FUN =mean,na.rm=T)
fbord<-aggregate(x=sdata["bord"],by=County,FUN =mean,na.rm=T)
fv208<-aggregate(x=sdata["v208"],by=County,FUN =mean,na.rm=T)
fv131<-aggregate(x=sdata["v131"],by=County,FUN =mean,na.rm=T)
fceaserian<-aggregate(x=sdata["ceaserian"],by=County,FUN =mean,na.rm=T)
ftwin<-aggregate(x=sdata["twin"],by=County,FUN =mean,na.rm=T)
feducation<-aggregate(x=sdata["no_education"],by=County,FUN =mean,na.rm=T)
fwater<-aggregate(x=sdata["access_towater_yes"],by=County,FUN =mean,na.rm=T)
fdisease<-aggregate(x=sdata["disease_environment"],by=County,FUN =mean,na.rm=T)
fpregnancy<-aggregate(x=sdata["ever_terminated_preganancy"],by=County,FUN =mean,na.rm=T)
fmarried<-aggregate(x=sdata["married"],by=County,FUN =mean,na.rm=T)
fsex<-aggregate(x=sdata["sex_child"],by=County,FUN =mean,na.rm=T)

#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,faridity,fpop,fv101,ftravel,fgrow,
              fm5,fm19,fv137,fb11,fb12,fv107,fbord,fv208,fv131,fceaserian,ftwin,feducation,
              fwater,fdisease,fpregnancy,fmarried,fsex)
bcells$expected_deaths<-bcells$total_children*(sum(bcells$total_observed_deaths,na.rm = T)/sum(bcells$total_children,na.rm=T))
bcells$relative_risk<-bcells$total_observed_deaths/bcells$expected_deaths

#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$Aridity,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$All_Population_Density_2010,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v101,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$Travel_Times,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$Growing_Season_Length,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m5,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m19,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v137,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b11,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b12,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v107,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$bord,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v208,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v131,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$ceaserian,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$twin,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$no_education,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$access_towater_yes,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$disease_environment,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$ever_terminated_preganancy,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$married,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$sex_child,ww,randomisation = T,zero.policy = T,na.action = na.omit)

#the only one that are not significant are b12 and m5 hence removed 
#choosing the best set of poisson variables 
library(FWDselect)
gbcells<-data.frame(bcells)
x<-gbcells[,c("Aridity",
             "v101", "Travel_Times", "Growing_Season_Length", "m19", 
             "v137", "b11", "bord", "v208", "v131", "ceaserian"
            , "no_education", "access_towater_yes", "disease_environment", 
             "ever_terminated_preganancy", "married")
          ]
y<-gbcells$total_observed_deaths
gvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:15,cluster=T))
myglm<-glm(total_observed_deaths~v137+v131+no_education+v208+b11+Aridity+ever_terminated_preganancy+access_towater_yes+ m19+ disease_environment,data = gbcells,family = "poisson")

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
formulae.modela<-total_observed_deaths~1
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))
summary(modela)

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
summary(result)
#standard bym model with covariates 
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+v137+v131+no_education+v208+b11+Aridity+ever_terminated_preganancy+access_towater_yes+ m19+ disease_environment
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
summary(result2)
#bym with smooth covariates 
result3<-inla(total_observed_deaths~v137+twin+disease_environment+Aridity+v131+Travel_Times+ceaserian+v208+no_education+ever_terminated_preganancy+v107
                +f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))
                       