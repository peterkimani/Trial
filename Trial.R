#importing dataset 
library(readstata13)
births.recode<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
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
#b7= age at death (months)
#v001 = cluster number 
#v005= sample weight 
my_data<-births.recode[,c("b2","b5","b6","b11","b12","m15","m19","m17","m18","b7","v001","v005")]
#limiting the sample to only children born between 2010 and 2014
my_data<-subset(my_data,b2 %in% 2010:2014)
#creating weight variable 
my_data$weight<-my_data$v005/100000
#creating an infant mortality variable 
my_data$im_variable<-as.numeric(my_data$b5 %in% "no")
#creaing variable for ceaserian section 
my_data$cs<-as.numeric(my_data$m17 %in% "yes")
#selecting rows that have only preceeding and succeeding birth intervals 
cases<-my_data[complete.cases(my_data[,c(5:4)]),]

#calculating four statistics 
library(sqldf)
deaths <- sqldf( "select v001 as dhsclust , count(*) as children , sum( im_variable ) as total_deaths , Avg( b11 ) as preceeding_birthinterval, Avg(b12) as succeeding_birthinterval, sum(cs) as ceaserian_births, sum( weight ) as sum_weight, sum( im_variable * weight ) as weighted_deaths from cases group by v001" )

#manipulating data \
#changing b11 and b12 from numeric to factor 

#library(dplyr)
#extracting data for only children who are dead 
#f.data<-my_data %>% group_by(my_data$v001) %>% 
  #summarise(child_is_alive =sum(b5=="yes"),
            #child_is_dead=sum(b5=="no"),
            #preceeding_birth_interval=mean(b11,na.rm=TRUE),
            #succeeding_birth_interval=mean(b12,na.rm=TRUE),
            #birth_weight=mean(as.numeric(m19),na.rm=TRUE))

#modelling the counts 
library(FWDselect)


