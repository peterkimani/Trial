
# # step 2: calculate interview cluster-specific values # #

library(foreign)
library(sqldf)
library(readstata13)
# load the children's recode file as a data.frame `ch`
ch<-read.dta13("C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEKR71DT\\KEKR71FL.DTA",convert.factors = TRUE)

# retain only the columns necessary for the analysis
# v001 cluster number
# v005 Sample weight
# b2 year of birth
# b5 child is alive
# b6 age at death (1xx in days, 2xx in months, 3xx in years)
ch <- ch[ , c( 'v001' , 'v005' , 'b2' , 'b5' , 'b6' ) ]

# limit the sample to only children born between 2004 and 2008
# with a non-missing age at death
ch <- subset( ch , b2 %in% 2010:2014 & !( b6 %in% 997:999 ) )
# that is, in the past three years.

# create a binary infant mortality variable
ch$im <- as.numeric( ch$b5 %in% 'no' & ch$b6 %in% 100:301 )

# create weigth variable
ch$w <- ch$v005 / 1000000

# note that this is very close to (but not exactly the same as)
# the nationwide egyptian infant mortality rate given on their report
# table 10.1 http://dhsprogram.com/pubs/pdf/FR220/FR220.pdf#page=148
weighted.mean( ch$im , ch$w ) * 1000
# their report says 24.5 per 1,000.
# the current microdata shows 24.2 per 1,000.  big whoop.

# calculate four statistics, grouped by survey cluster
# count, weighted count, infant deaths, weighted infant deaths
cl <- sqldf( "select v001 as dhsclust , count(*) as denom , sum( im ) as numer , sum( w ) as wdenom , sum( im * w ) as wnumer from ch group by v001" )

# that was easy, huh?  want to look at your resultant cluster-level information?
head( cl )

tail( cl )

# # end of step 2 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # #
# # step 3: prepare your prevR object # #

library(prevR)
library(foreign)

# what country are you mapping?  egypt?
# oh, okay.  well then let's grab egypt's boundaries.
bounds <- create.boundary( "Kenya" )
# that was it?  that was too easy.

# import the longitude and latitude data from the
# geographical information system (gis) files (this is a special request)
longlat <- read.dbf( "C:\\Users\\sam-pc\\Documents\\pitz\\Bsc Biostatistics\\BBS 4.2\\Mapping excersie\\DHS Child mortality kenya\\KE_2014_DHS_04262018_458_120021\\KEGE71FL\\KEGE71FL.dbf" )

# convert all column names to lowercase
names( longlat ) <- tolower( names( longlat ) )

# merge this cluster information onto the cluster-level results data.frame
x <- merge( cl , longlat[ , c( 'dhsclust' , 'longnum' , 'latnum' , 'source' ) ] )

# confirm that every cluster that you have infant mortality information for
# also has a longitude & latitude variable now
stopifnot( nrow( x ) == nrow( cl ) )

# check how many clusters are missing coordinates
miss.coord <- nrow( subset( x , source == 'MIS' ) )

# discard records with missing longitudes & latitudes
x <- subset( x , source != 'MIS' )

# confirm you've tossed the correct number of records
stopifnot( nrow( x ) + miss.coord == nrow( cl ) )

# identify which columns are integer types
ic <- sapply( x , is.integer )

# coerce every integer column to numeric
x[ , ic ] <- sapply( x[ , ic ] , as.numeric )

# create a prevR object like a professional.
pro <- 
  as.prevR(
    x , 
    c( id = "dhsclust" , x = "longnum" , y = "latnum" , n = "denom" , wn = "wdenom" , pos = "numer" , wpos = "wnumer" ) , 
    bounds
  )

# want to take a first glance at the sampling clusters
# of the 2008 egypt demographic and health surveys?
plot( pro )
# woah.

# # end of step 3 # #
# # # # # # # # # # #


# # # # # # # # # # # # # # # # #
# # step 4: make a simple map # #

# compute bandwidths
pro <- rings( pro , N = 1000 )

# compute surfaces
pro.map <- kde( pro , N = 1000 , nb.cells = 250 )

# plot a simple map comparing
# weighted and unweighted surfaces
spplot( pro.map )

# re-create a simple weighted surface,
# but with a prevR palette
spplot( 
  pro.map, 
  'k.wprev.N1000.RInf' , 
  cuts = 100 , 
  col.regions = prevR.colors.red( 101 ) , 
  main = "regional trends of infant mortality"
)

