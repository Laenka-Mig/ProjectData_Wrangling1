# Data cleaning before processesing.
# Ladda ner 'packages' och 'libraries'
#  ==> Has the newly introduced applications  method been successful?
# If yes, we expect to see a continuous increase in No. of Method_A applications.

install.packages("tidyverse")

# NOTE! loading tidyverse library may show some warning messages, that's Okej.
library(tidyverse)

# b/c we maybe transforming dates and so on, we load "lubridate"
library(lubridate)

# b/c we are gonna be transforming strings we, load this:
library(stringr)

#load data in a tibble, we give it a unique vector so that we can reuse it in case we make 
# an error along our coding after overriding the name - takes long to (re-)download.

PN_data <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv") 
# credit to Mike Capple.

# click "view" on the "Environment" pane to have a "feel" of the raw data or
# view
glimpse(PN_data)

# Having seen to be a WIDE data set, having more than 1 observation per row, therefore
# it will be easier to handle if it were a LONG data set, so:
# LONG <==== WIDE

long_PN <- gather(PN_data, månad, ansökningar, -Fiscal_Year)

# study the månad column
unique(long_PN$månad)

# breakdown to months & application types
sep.long_PN<- separate(long_PN, månad, c("månad", "ansöknings_metod"),sep="_")

# view the outcome
 print(sep.long_PN, n=20)
 
# rename column with swedish
 colnames(long_PN)[1] <- "Beskattningsår"
 
# view
 print(long_PN, n=20)
 
 sep.long_PN<- separate(long_PN, månad, c("månad", "ansöknings_metod"), sep="_")
 
# view
 print(sep.long_PN, n=20)
 
# rename all månad columns with only 1st-3 letters,
sep.long_PN$månad <-substr(sep.long_PN$månad, 1, 3)

# view
 print(sep.long_PN,  n=50)
 
# adjust the Beskattningsår datatype 
 sep.long_PN$Beskattningsår <- str_replace(sep.long_PN$Beskattningsår,"FY","20")
 
# view
 print(sep.long_PN,  n=50)
 
# For complete date: assume 01 of each month & combine Beskattningsår with månd
 paste("01", sep.long_PN$månad, sep.long_PN$Beskattningsår)
 
# Change date datatype to numeric datetype
 paste("01", sep.long_PN$månad, sep.long_PN$Beskattningsår)

# store to make it easy to call
 sep.long_PN$date <- dmy(paste("01", sep.long_PN$månad, sep.long_PN$Beskattningsår))

# view
 print(sep.long_PN$date)

# convert from fiscal year to calenda year by + or - 1. 
# in this case, (Oct., Nov., & Dec.,) -1.
 
# First, lets locate all of the months requiring -1 reduction.
 convert <- which(month(sep.long_PN$date) >=10)

# subtract 1 and assign it a vector
year(sep.long_PN$date[convert]) <- year(sep.long_PN$date[which(month(sep.long_PN$date) >=10)])-1
 
# view
print(sep.long_PN)

# vi behöver inte Beskattningsår kolumnen mer, så annulera vi det!
sep.long_PN$Beskattningsår <- NULL
sep.long_PN$månad <- NULL

# kolla
print(sep.long_PN)

# ansöknings_metod har bara två värden
sep.long_PN$ansöknings_metod <- as.factor(sep.long_PN$ansöknings_metod)

# kolla
print(sep.long_PN)

# Äntligen, konvertera oss våran data från LÅNG till BRED.
PersonNummer <- spread(sep.long_PN, ansöknings_metod, ansökningar)

# Dags att visualisera och får svara till hypotesen. 
# Att "Anledningen till att ansökningar för socialbidrag via internet för att 
# minska kostnaden har lyckats"

PersonNummer$viaInternet <- (PersonNummer$Internet /PersonNummer$Total)*100 

ggplot(data=PersonNummer, mapping = aes(x=date, y=viaInternet)) + geom_point()



   
