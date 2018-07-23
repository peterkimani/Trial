#importing the dataset 
library(readstata13)
household<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEHR71DT\\KEHR71FL.DTA",convert.factors = TRUE)
household<-household[,c("hv001","hv002","hv237","hv228")]

births<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
births<-births[,c("v001","v002","v005","v101","m17","m5","m19","m34","v137","b11","b12","v106","m14","b0","b5","v107","v106","v131","bord","v207","v206","v228","v501","b4","v025")]

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
t<-,"v106","v131","hv227","hv228","bord","bidx","v208","v228","v501","b4","status"
t<-c("v001","v005","Aridity","All_Population_Density_2010","v101","Travel_Times","Nightlights_Composite",
     "Growing_Season_Length","m17","m5","m19","m34","v137","b11","b12","m14","b0","b5","v107","v206","v207","v106","v131","hv237","hv228","bord","v228","v501","b4","status")
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
    
    Nightlights_Composite=mean(Nightlights_Composite,na.rm=T),
    Growing_Season_Length=mean(Growing_Season_Length,na.rm = T),
    m5=mean(m5,na.rm=T),
    m19=mean(m19,na.rm = T),
    v137=mean(v137,na.rm = T),
    b11=mean(b11,na.rm = T),
    b12=mean(b12,na.rm = T),
    bord=mean(bord,na.rm = T),
    ceaserian=sum(m17=="yes"),
    twin=sum(b0=="single birth")/length(b5),
    no_education=sum(v106=="no education")/length(b5),
    access_towater_yes=sum(hv237=="yes")/length(b5),
    disease_environment=sum(hv228=="all children")/length(b5),
    ever_terminated_preganancy=sum(v228,na.rm = T)/length(b5),
    married=sum(v501=="married")/length(b5),
    sex_child=sum(b4=="female")/length(b5),
    daughter_dead=mean(v207,na.rm = T),
    son_dead=mean(v206,na.rm=T))

#checking colinearlity 
library(mctest)
x<-good.luck[,-c(1,2,3)]
y<-good.luck$total_observed_deaths
imcdiag(x,y,corr = T,vif = 8)
#multicorlinearlity in travel times and nightlight composite are colinear. removing nightlight composite 
good.luck1<-good.luck[,-c(7)]

#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
library(dplyr)
names(good.luck1)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,good.luck1,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
bject2<-readOGR(".","KEN_Adm2")
County<-bject2


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
fb11<-aggregate(x=sdata["b11"],by=County,FUN =mean,na.rm=T)
fb12<-aggregate(x=sdata["b12"],by=County,FUN =mean,na.rm=T)
fbord<-aggregate(x=sdata["bord"],by=County,FUN =mean,na.rm=T)
fceaserian<-aggregate(x=sdata["ceaserian"],by=County,FUN =mean,na.rm=T)
ftwin<-aggregate(x=sdata["twin"],by=County,FUN =mean,na.rm=T)
feducation<-aggregate(x=sdata["no_education"],by=County,FUN =mean,na.rm=T)
fwater<-aggregate(x=sdata["access_towater_yes"],by=County,FUN =mean,na.rm=T)
fdisease<-aggregate(x=sdata["disease_environment"],by=County,FUN =mean,na.rm=T)
fpregnancy<-aggregate(x=sdata["ever_terminated_preganancy"],by=County,FUN =mean,na.rm=T)
fmarried<-aggregate(x=sdata["married"],by=County,FUN =mean,na.rm=T)
fsex<-aggregate(x=sdata["sex_child"],by=County,FUN =mean,na.rm=T)
fv137<-aggregate(x=sdata["v137"],by=County,FUN =mean,na.rm=T)
fson<-aggregate(x=sdata["son_dead"],by=County,FUN =mean,na.rm=T)
fdaugher<-aggregate(x=sdata["daughter_dead"],by=County,FUN =mean,na.rm=T)
fconstituency<-bject2@data$Adm2Name
fcounty<-bject2@data$Adm1Name
#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,faridity,fpop,ftravel,fgrow,
              fm5,fm19,fb11,fb12,fbord,fceaserian,ftwin,feducation,
              fwater,fdisease,fpregnancy,fmarried,fsex,fv137,fson,fdaugher,fconstituency,fcounty)
bcells$expected_deaths<-bcells$total_children*(sum(bcells$total_observed_deaths,na.rm = T)/sum(bcells$total_children,na.rm=T))
bcells$relative_risk<-bcells$total_observed_deaths/bcells$expected_deaths
Chids_mortality<-bcells
#selecting the best variables for poisson model based on counties 
library(FWDselect)
poibcells<-data.frame(bcells)
x<-poibcells[,-c(1,2,23,24,10,17,21,22)]
y<-poibcells$total_observed_deaths
poivariables<-qselection(x,y,qvector = c(1:15),family = "poisson",method = 'glm',criterion = "aic")
#12 variables significant they are  daughter_dead, Aridity, access_towater_yes, son_dead, ceaserian, b11, disease_environment, ever_terminated_preganancy, no_education, v137, sex_child, twin

#computing moran 1 test for each variable that is significant  
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$Aridity,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b11,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$ceaserian,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$disease_environment,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$bord,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m5,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$no_education,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v137,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$twin,ww,randomisation = T,zero.policy = T,na.action = na.omit)





#variable sex_child and b11 is not clustered


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

#running the poisson model with intercept alone 
formulae.modela<-total_observed_deaths~1
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))
summary(modela)


#running bayesian poisson model with all the covariates from the best subset variables (including v107 and v208) 
formulae.modelb<-total_observed_deaths~1+  Aridity+b11+ceaserian+disease_environment+bord+m5+no_education+v137+twin
modelb<-inla(formulae.modelb,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))
summary(modelb)

#running bayesian poisson model without the non clustered variables 
formulae.modelc<-total_observed_deaths~1+  Aridity+b11+ceaserian+disease_environment+bord+no_education+v137
modelc<-inla(formulae.modelc,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))
summary(modelc)

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
summary(result)

#bym model with all the covariates (including spatial ones that are not clustered)
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+Aridity+b11+ceaserian+disease_environment+bord+m5+no_education+v137+twin
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
summary(result2)
#standard bym model with covariates 
formulae3<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+1+  Aridity+b11+ceaserian+disease_environment+bord+no_education+v137
result3<-inla(formulae3,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
summary(result3)
#bym with smooth covariates 
result4<-inla(total_observed_deaths~Aridity+b11+ceaserian+disease_environment+bord+m5+no_education+v137+twin
              +f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))
summary(result4)                      

#obtaining raw and fitted smr 
bcells$fittedsmr<-result2$summary.fitted.values$mean
plot(bcells$relative_risk,bcells$fittedsmr)

#importing the dataset 
child.data<-bcells[,c(1:3,9,12,16,11,7,14,20,13,24,27,23)]
names(child.data@data)[2]<-"Deaths"
names(child.data@data)[4]<-'Preceeding Birth Interval'
names(child.data@data)[5]<-"Ceaserian Births"
names(child.data@data)[6]<-"Children Sleep Under Mosquito Net"
names(child.data@data)[7]<-"Birth Order"
names(child.data@data)[8]<-"Duration of Breastfeeding"
names(child.data@data)[9]<-"Mothers_Level of Education(No educaion)"
names(child.data@data)[10]<-"Chidlren Under 5 in one Household"
names(child.data@data)[11]<-"Child is not a twin"
names(child.data@data)[12]<-"Raw Relative Risk"
names(child.data@data)[13]<-"Estimated Relative Risk"

#exporting the shp object 
library(maptools)
writeSpatialShape(child.data,"Child Mortality Data")

