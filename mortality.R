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

limiting the sample to only children born between 2009 and 2014
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
sdata@data<-join(sdata@data,f.data,by="DHSCLUST")

#computing moran 1 test for each variable 
#library(spdep)
#w<-poly2nb(findata,row.names = findata$DHSCLUST)
