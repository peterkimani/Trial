#importing dataset 
library(readstata13)
births.recode<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
#reading only the variables that i am interested in 
#b2=year of birth 
#b5= child is alive 
#b6=age at birth 
#b11=preceeding birth interval (months)
#b12=succeeding birth intervals in months 
#m15= place of delivery 
#m19 = birth weight in kgs 
#m17 = delivery by ceaserian sections 
#m18 = size of child at birth 
#b7= age at death (months)
#v001 = cluster number 
#v005= sample weight 
my.data<-births.recode[,c("b2","b5","b11","b12","m15","m19","m17","m18","b7","v001","v005")]
#manipulating data 
library(dplyr)
my.data %>% group_by(my.data$v001) %>% 
  summarise(child_is_alive =sum(b5=="yes"),
            child_is_dead=sum(b5=="no"))
