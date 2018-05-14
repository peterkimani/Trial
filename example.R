#importing dataset 
library(readstata13)
library(foreign)
library(sqldf)
births.recode<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)
# retain only the columns necessary for the analysis
# v001 cluster number
# v005 Sample weight
# b2 year of birth
# b5 child is alive
# b6 age at death (1xx in days, 2xx in months, 3xx in years)
my.data<-births.recode[,c("v001","v005","b2","b5","b6")]
#limiting the data to children only born between 2010 and 2014 with non missing age at death 
my.data<-subset(my.data,b2%in% 2010:2014 & !(b6 %in%))
my.data<-