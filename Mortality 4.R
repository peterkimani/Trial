
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

#looking for spatial autocorrelation 
#spatial variables 
myspatial<-trial[,c("status","Aridity","All_Population_Density_2010","v101","Travel_Times","Nightlights_Composite",
                    "Growing_Season_Length")]
myspatial$v101<-as.numeric(myspatial$v101)
library(mctest)
imcdiag(y=myspatial$status,x=myspatial[,-c(10)],corr = T,all=T)
otherdataset<-trial[,c("status","v001","v005","m17","m5","m19","v137","b11","b12","b0","v107","v106","v131","hv237","hv228","bord","bidx","v208","v228","v501","b4")]
otherdataset$m17<-as.numeric(otherdataset$m17)
otherdataset$v106<-as.numeric(otherdataset$v106)
otherdataset$b0<-as.numeric(otherdataset$b0)
otherdataset$hv237<-as.numeric(otherdataset$hv237)
otherdataset$hv228<-as.numeric(otherdataset$hv228)
otherdataset$v501<-as.numeric(otherdataset$v501)
otherdataset$b4<-as.numeric(otherdataset$b4)
otherdataset$v131<-as.numeric(otherdataset$v131)
otherdataset$v228<-as.numeric(otherdataset$v228)
imcdiag(y=otherdataset$status,otherdataset[,-c(1)],method="VIF",corr=T)

#removing the variables that are correlated Nighlights_composite 
#performing forward selection 
library(FWDselect)
x<-trial[,c("Aridity","All_Population_Density_2010","v101","Travel_Times", "Growing_Season_Length","v001","v005","m17","m5","m19","v137","b11","b12","b0","v107","v106","v131","hv237","hv228","bord","bidx","v208","v228","v501","b4")]
y<-trial$status
svariables<-qselection(x,y,qvector=c(1:10),criterion = 'aic',method = "glm",family = "binomial")


#the significant variables are b12, m5, v107, b11, v137, hv228, v131, m17, v208, v101, v501, hv237, v228, b4, m19, bidx, Growing_Season_Length, Travel_Times
#fitting the glm model 
myglm<-glm(status~1+b12+ m5+ v107+ b11+ v137+ hv228+ v131+ m17+v208+ v101+ v501+ hv237+ v228+ b4+ m19+ bidx+ Growing_Season_Length+ Travel_Times,family = "binomial",data=trial)





library(dplyr)

f.myvariables<-trial %>% group_by(trial$v001) %>% 
  summarise(
    total_children=length(b5),
    total_observed_deaths=sum(b5=="no"),
    b12=mean(b12,na.rm=TRUE),
    m5=mean(m5,na.rm=TRUE),
    bidx=mean(bidx,na.rm=TRUE),
    m19=mean(m19,na.rm=TRUE),
    v208=mean(v208,na.rm=TRUE),
    v501_married=sum(v501=="married"),
    v501_livingwithpartner=sum(v501=="living with partner"),
    v501_no_livingwithpartner=sum(v501=="no longer living together/separated"),
    hv237_yes=sum(hv237=="yes"),
    b4_female=sum(b4=="female")
    #v228=sum(v228 == "yes"),
    
  )


#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
library(dplyr)
names(ff.myvariables)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,ff.myvariables,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
bject2<-readOGR(".","KEN_Adm2")
County<-bject2

#obtaining counts and averages based on the cells regions 
ftotal_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
ftotal_observed_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
fm5<-aggregate(x=sdata["m5"],by=County,FUN =mean,na.rm=T)
fm19<-aggregate(x=sdata["m19"],by=County,FUN =mean,na.rm=T)
fv208<-aggregate(x=sdata["v208"],by=County,FUN =mean,na.rm=T)
fb12<-aggregate(x=sdata["b12"],by=County,FUN =mean,na.rm=T)
fv501_married<-aggregate(x=sdata["v501_married"],by=County,FUN =mean,na.rm=T)
fv501_nopartner<-aggregate(x=sdata["v501_no_livingwithpartner"],by=County,FUN =mean,na.rm=T)
fv501_partner<-aggregate(x=sdata["v501_livingwithpartner"],by=County,FUN =mean,na.rm=T)
fhv237<-aggregate(x=sdata["hv237_yes"],by=County,FUN =mean,na.rm=T)
fb4_female<-aggregate(x=sdata["b4_female"],by=County,FUN =mean,na.rm=T)
fbidx<-aggregate(x=sdata["bidx"],by=County,FUN =mean,na.rm=T)

#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,fm5,fm19,fv208,fb12,fv501_married,fv501_nopartner,fv501_partner,
              fb4_female,fhv237,fbidx)
#adding id 
bcells$id<-County$id

#colinearlity test 
hgdata<-data.frame(bcells)
x<-hgdata[,-c(1:2)]
y<-hgdata$total_observed_deaths
imcdiag(x,y,corr=T,all = T)
#vriables bidx and v501_living with partner were dropped due to colinearliy
bcells<-bcells[,-c(12,9)]
#variable selection using FWD select 
library(FWDselect)
y<-bcells$total_observed_deaths
x<-bcells@data[,-c(1,2)]
my_pvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:7))
# all variables in the model are significant 


#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v208,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m5,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m19,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b12,ww,randomisation = T,zero.policy = T,na.action = na.omit)

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
formulae.modela<-total_observed_deaths~1+v208+m19
modela<-inla(formulae.modela,family = "poisson",data = kenya,E = expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model without covariates 
#bcells<-cbind(bcells,tid2=bcells$id)
#standard bym model (without covariates)

formulae<-total_observed_deaths~1+f(tid2,model="besag",graph= "ww.adj")+f(tid,model="iid")
result<-inla(formulae,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))

#standard bym model with covariates 
formulae2<-total_observed_deaths~f(tid2,model="besag",graph = "ww.adj")+f(tid,model="iid")+
  +v208+m19
result2<-inla(formulae2,family="poisson",data= kenya,E= expected_deaths,control.compute = list(dic=T,waic=T))
#bym with smooth covariates 
result3<-inla(total_observed_deaths~v208+m19+
                f(tid2,model="besag",graph="ww.adj"),family = "poisson",E=expected_deaths,data=kenya,control.predictor = list(compute = TRUE),control.compute = list(dic=T,waic=T))



















































######################################################################################
#######################################################################################
#############################################################################################
##############################################################################

#mymodel<-glm(status~b12+m5+v107+b11+v137+hv228,data=trial,family = "binomial")
library(dplyr)
f.myvariables<-trial %>% group_by(trial$v001) %>% 
  summarise(
    total_children=length(b5),
    total_observed_deaths=sum(b5=="no"),
    v137=mean(v137,na.rm=TRUE),
    b12=mean(b12,na.rm=TRUE),
    m5=mean(m5,na.rm=TRUE),
    b11=mean(b11,na.rm=TRUE),
    bidx=mean(bidx,na.rm=TRUE),
    Travel_Times=mean(Travel_Times,na.rm=TRUE),
    Growing_Season_Length=mean(Growing_Season_Length,na.rm=TRUE),
    m19=mean(m19,na.rm=TRUE),
    v208=mean(v208,na.rm=TRUE),
    m19=mean(m19,na.rm=TRUE),
    m17=sum(m17=="yes"),
    v501=sum(v501=="married"),
    hv237=sum(hv237=="yes"),
    b4=sum(b4=="male"),
    hv228=sum(hv228=="all children"),
    v106=sum(v106=="no education"),
    v228=sum(v228=="yes")
  )

#importing the spatial object 
library(rgdal)
sdata<-readOGR(".","KEGE71FL")
library(dplyr)
names(f.myvariables)[1]<-"DHSCLUST"
sdata@data<-left_join(sdata@data,f.myvariables,by="DHSCLUST")
#importing counties dataset 
counties<-readOGR(".","County")
bject2<-readOGR(".","KEN_Adm2")
County<-bject2

#obtaining counts and averages based on the cells regions 
ftotal_children<-aggregate(x=sdata["total_children"],by=County,FUN=sum,na.rm=T)
ftotal_observed_deaths<-aggregate(x=sdata["total_observed_deaths"],by=County,FUN=sum,na.rm=T)
fv137<-aggregate(x=sdata["v137"],by=County,FUN=mean,na.rm=T)
fTravel_Time<-aggregate(x=sdata["Travel_Times"],by=County,FUN=mean,na.rm=T)
fgrowing<-aggregate(x=sdata["Growing_Season_Length"],by=County,FUN =mean,na.rm=T)
fm5<-aggregate(x=sdata["m5"],by=County,FUN =mean,na.rm=T)
fm19<-aggregate(x=sdata["m19"],by=County,FUN =mean,na.rm=T)
fv208<-aggregate(x=sdata["v208"],by=County,FUN =mean,na.rm=T)
fb11<-aggregate(x=sdata["b11"],by=County,FUN =mean,na.rm=T)
fb12<-aggregate(x=sdata["b12"],by=County,FUN =mean,na.rm=T)
fv106<-aggregate(x=sdata["v106"],by=County,FUN =mean,na.rm=T)
fm17<-aggregate(x=sdata["m17"],by=County,FUN =mean,na.rm=T)
fv501<-aggregate(x=sdata["v501"],by=County,FUN =mean,na.rm=T)
fhv237<-aggregate(x=sdata["hv237"],by=County,FUN =mean,na.rm=T)
fhv228<-aggregate(x=sdata["hv228"],by=County,FUN =mean,na.rm=T)
fb4<-aggregate(x=sdata["b4"],by=County,FUN =mean,na.rm=T)
fv228<-aggregate(x=sdata["v228"],by=County,FUN =mean,na.rm=T)
fbidx<-aggregate(x=sdata["bidx"],by=County,FUN =mean,na.rm=T)

#merging them into one dataset 
bcells<-cbind(ftotal_children,ftotal_observed_deaths,fb12,fm5,fb11,fv137,fv208,fm19,fbidx,fgrowing,fTravel_Time
              ,fv106,fm17,fv501,fhv228,fhv237,fb4,fv228)
#adding id 
bcells$id<-County$id


#variable selection using FWD select 
library(FWDselect)
y<-bcells$total_observed_deaths
x<-bcells@data[,-c(1,2)]
my_pvariables<-qselection(x,y,criterion = "aic",method = "glm",family = "poisson",qvector = c(1:15))
# all variables in the model are significant 


#computing moran 1 test for each variable 
library(spdep)
w<-poly2nb(bcells)
ww<-nb2listw(w,style = "B",zero.policy = T)

moran.test(bcells$total_observed_deaths,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$v228,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m5,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$m19,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$b12,ww,randomisation = T,zero.policy = T,na.action = na.omit)
moran.test(bcells$hv228,ww,randomisation = T,zero.policy = T,na.action = na.omit)

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
formulae.modela<-total_observed_deaths~1+bidx+v228
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

