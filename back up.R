library(rgdal)
library(spdep)
library(tmap)
library(FWDselect)
library(INLA)
library(dplyr)
library(plyr)
library(diseasemapping)
#reading the kenya counties shape file 
counties<-readOGR(".", "County")
View(data.frame(counties))
#importing the counties HIV dataset 
hiv<-read.csv("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\Trial1\\kenyahivdata.csv")
#selecting the variables we are interested in 
hiv<-hiv[,c(1,2,3,5,6,7,15)]
#changing hiv county names to small letters for merging 
hiv$COUNTY<-tolower(hiv$COUNTY)
hiv<-hiv[order(hiv$COUNTY),]
counties<-counties[order(counties$COUNTY),]
#joining the dataset 
counties$COUNTY<-tolower(counties$COUNTY)
counties@data<-join(counties@data,hiv,by="COUNTY")


#selecting significant variables 
x<-hiv[,c(2,4,5,6,7)]
y<-hiv[,3]
sigvariables<-qselection(x,y,qvector=c(1:4),method="glm",family = "poisson")
sigvariables
#computing expected cases 
counties$expected<-counties$total_population*((sum(counties$new_hiv_infections_adults_15))/sum(counties$total_population))


#fitting inla 
#creating adjacency matrix 
g<-poly2nb(counties)
nb2INLA("g.adj",g)
counties$myid<-c(1:47)
counties$myid2<-c(1:47)
counties$county4_ID<-counties$COUNTY3_ID
formulae<-counties$new_hiv_infections_adults_15~f(counties$myid,model="besag",graph.file = "g.adj")+f(counties$myid2,model="iid")+f(counties$art_coverage,model="rw2")
data1<-as.data.frame(counties)
results<-inla(formulae,family = "poisson",data = as.data.frame(counties),E =counties$expected,control.predictor=list(compute=TRUE))
#adding the computed relative risk 
counties$relative_risk<-results$summary.fitted.values[,1]

#ploting the new hiv cases 
qtm(counties,fill ="new_hiv_infections_adults_15",fill.pallete="Greens")
qtm(counties,fill="relative_risk",fill.pallete="Greens")