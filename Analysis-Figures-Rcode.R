
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF THE GEOGRAPHY OF EDITORIAL BOARDS
#This is a clone of the code in the Github Repo for analaysis of Gender and Editorial Boards (https://github.com/embruna/Editorial-Board-Gender).


#Please insert this line of code. Once you do be sure to commit.



#Set WD and load packages you need. Not all of which you need after all.
setwd("-------")
library(countrycode)
library(dplyr)
library(gdata)
library(ggplot2)
library(maps)
library(RColorBrewer)
library(reshape2)
require(rworldmap)
library(tidyr)
library(WDI)

#CLear out everything from the environment 
rm(list=ls())

##################
#################
###DATA ENTRY AND CLEANUP
##################
#################
#Step 1: load the individual CSV files and save them as dataframes

#THESE ARE THE DATA FROM CHO ETAL
BITR<-read.csv("Biotropica_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
BIOCON<-read.csv("Biocon_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
ARES<-read.csv("ARES_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
AGRON<-read.csv("Agronomy_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
NAJFM<-read.csv("NAJFM_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
AJB<-read.csv("AJB_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
CONBIO<-read.csv("ConBio_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
ECOLOGY<-read.csv("Ecology_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
JECOL<-read.csv("JEcol_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
JTE<-read.csv("JTE_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

#THESE WERE COLLECTED BY THE 2015 EDITION OF THE COURSE
AGRON2<-read.csv("AGRON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
ARES2<-read.csv("ARES2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
BIOCON2<-read.csv("BIOCON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
BITR2<-read.csv("BITR2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
EVOL<-read.csv("EVOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #Still need to ID what an Editor vs EIC does when they transitoned to EIC
FEM<-read.csv("FEM.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
JAPE<-read.csv("JAPE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
MARECOL<-read.csv("MARECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
NAJFM2<-read.csv("NAJFM2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
OIKOS<-read.csv("OIKOS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #5 are missing country
AMNAT<-read.csv("AMNAT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
BIOG<-read.csv("BIOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
ECOG<-read.csv("ECOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
FUNECOL<-read.csv("FUNECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
JANE<-read.csv("JANE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
PLANTECO<-read.csv("PLANTECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
JTE2<-read.csv("JTE2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
OECOL<-read.csv("OECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

#STILL MISSING SOME DATA 
GCB<-read.csv("---.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#Still missing years and putting eds into cats
LECO<-read.csv("LECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#LE is missing 2004, 2011-2014
NEWPHYT<-read.csv("NEWPHYT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
#Need to define as EIC, SE, AE, Other
MEPS<-read.csv("MEPS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#Need to define as EIC, SE, AE, Other
JZOOL<-read.csv("JZOOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
#INCOMPLETE


#step 2: bind the dataframes of all journals together
ALLJOURNALS_CHO<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE) #Bind the data from Cho
ALLJOURNALS_2015<-rbind(AGRON2, ARES2, BIOCON2, BITR2, EVOL, FEM, JAPE, MARECOL, NAJFM2, OIKOS, AMNAT, BIOG, ECOG, FUNECOL, JANE, PLANTECO, JTE2, OECOL) #Bind the data from 2015 workshop
ALLJOURNALS<-rbind (ALLJOURNALS_CHO, ALLJOURNALS_2015[,1:10]) #bind the two together

#step3: change all the country names to the codes used in mapping

#Add a column with the 3 letter country codes to be consistent with the other datasets
#Maptools uses the ISO 3166 three letter codes: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#The packahge countrycode will take your column of country names and convert them to ISO3166-3 Codes
#I began by checking the values of COUNTRY to see if there are any mistakes. To do so I just created a vector 
#called CODECHECK
ALLJOURNALS$CODECHECK<-countrycode(ALLJOURNALS$COUNTRY, "country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
#You can correct these as follows in the dataframe with all the data, then add a new column to the dataframe with the country codes

ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "USA "]  <- "USA" #One of the datasets in Cho et al had a space after USA so needs to be corrected
ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "lndonesia"]  <- "Indonesia" #One of the datasets in Cho et al had Indonesia mispelled somewhere
ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "Scotland"]  <- "UK" #With apologies to Scots everywhere
ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "Wales"]  <- "UK"
ALLJOURNALS$COUNTRY[ALLJOURNALS$COUNTRY == "England"]  <- "UK"


ALLJOURNALS$COUNTRY.CODE<-countrycode(ALLJOURNALS$COUNTRY, "country.name", "iso3c", warn = TRUE)   #create new column with country ISO code

#step 4: chose the temporal coverage
#use only 1985 to 2013 yeara
ALLJOURNALS<-ALLJOURNALS[ALLJOURNALS$YEAR>=1985 & ALLJOURNALS$YEAR<=2013,]

#step 5: 2x that it all looks ok
summary(ALLJOURNALS)

#2x check - are there any with country missing?
MISSING=subset(ALLJOURNALS, subset=(COUNTRY=="?"))
MISSING


#######################
#EDITORIAL BOARD SIZE
#Number of EIC, SE, AE, by year by journal
#######################

#Group dataframe by CATEGORY, JOURNAL AND YEAR
countSIZE <- dplyr::group_by(ALLJOURNALS, CATEGORY, JOURNAL, YEAR)
#Count the number of unique editors by category by country
countSIZE_SUMMARY = summarize (countSIZE,
                               number = length(NAME))

#Plot of editorial size over years, SE EXAMPLE
ggplot(countSIZE_SUMMARY[countSIZE_SUMMARY$CATEGORY == 'SE',], 
       aes(x = YEAR, y = number, group = JOURNAL, colour = JOURNAL)) +
  geom_line()  +
  ylab("Size od Editorial Board (SE)")


#######################
# TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY (ALL JOURNALS, ALL YEARS)
#Number of (Editor-in-Chief (EIC), Subject Editor (SE), and Associate Editor (AE))
#by country:
#######################
#Group dataframe by COUNTRY.CODE and  Editor CATEGORY
countCOUNTRY <- dplyr::group_by(ALLJOURNALS, COUNTRY.CODE, CATEGORY)
#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each EIC only once
countCOUNTRY <- unique( countCOUNTRY[ , c('NAME', 'COUNTRY.CODE', 'JOURNAL', 'CATEGORY') ] )

#Count the number of unique editors by category by country
countCOUNTRY_SUMMARY = summarize (countCOUNTRY,
                                  number = length(unique(NAME)))
#Reshape the table from long to wide
countCOUNTRY_SUMMARY <- dcast(data = countCOUNTRY_SUMMARY,
                              formula = COUNTRY.CODE ~ CATEGORY, 
                              value.var = 'number')

print(countCOUNTRY_SUMMARY)


########################
# BAR PLOTS OF TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY 
# (ALL JOURNALS, ALL YEARS)
#######################
#Subset to EIC column and remove NAs
EICsubset <- subset(countCOUNTRY_SUMMARY, EIC > 0)[, c("COUNTRY.CODE", "EIC")]
#Sort descending EIC count
EICsubset$COUNTRY.CODE <-factor(EICsubset$COUNTRY.CODE, levels=EICsubset[order(-EICsubset$EIC), "COUNTRY.CODE"])

#Plot of EIC numbers by country in decreasing number
ggplot(data=EICsubset, aes(x=COUNTRY.CODE, y=EIC)) +
  geom_bar(stat="identity")

#######################
#Subset to AE column and remove NAs
AEsubset <- subset(countCOUNTRY_SUMMARY, AE > 0)[, c("COUNTRY.CODE", "AE")]
#Sort descending AE count
AEsubset$COUNTRY.CODE <-factor(AEsubset$COUNTRY.CODE, levels=AEsubset[order(-AEsubset$AE), "COUNTRY.CODE"])

#Plot of EIC numbers by country in decreasing number
ggplot(data=AEsubset, aes(x=COUNTRY.CODE, y=AE)) +
  geom_bar(stat="identity")

#######################
#Subset to SE column and remove NAs
SEsubset <- subset(countCOUNTRY_SUMMARY, SE > 0)[, c("COUNTRY.CODE", "SE")]
#Sort descending AE count
SEsubset$COUNTRY.CODE <-factor(SEsubset$COUNTRY.CODE, levels=SEsubset[order(-SEsubset$SE), "COUNTRY.CODE"])

#Plot of EIC numbers by country in decreasing number
ggplot(data=SEsubset, aes(x=COUNTRY.CODE, y=SE)) +
  geom_bar(stat="identity")



#######################
# MAPS FOR TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY 
#######################
#Create an map object using our COUNTRY.CODE 
sPDF <- joinCountryData2Map( countCOUNTRY_SUMMARY
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "COUNTRY.CODE")
#mapDevice() #create world map shaped window
#Editor in Chief Map
mapCountryData(sPDF,
               nameColumnToPlot='EIC',
               catMethod = 'pretty')
#Associate Editor Map
mapCountryData(sPDF,
               nameColumnToPlot='AE',
               catMethod = 'pretty')
#Subject Editor Map
mapCountryData(sPDF,
               nameColumnToPlot='SE',
               catMethod = 'pretty')

#######################
# Number Countries represented by YEAR by JOURNAL by CATEGORY
#######################
#Group dataframe by CATEGORY, JOURNAL AND YEAR
countCOUNTRYYEAR <- dplyr::group_by(ALLJOURNALS, CATEGORY, JOURNAL, YEAR)
#Count the number of unique editors by category by country
countCOUNTRYYEAR_SUMMARY <- summarize (countCOUNTRYYEAR,
                               COUNTRIES = length(unique(COUNTRY.CODE)))
#PLOT
ggplot(countCOUNTRYYEAR_SUMMARY[countCOUNTRYYEAR_SUMMARY$CATEGORY == 'SE',], 
       aes(x = YEAR, y = COUNTRIES, group = JOURNAL, colour = JOURNAL)) +
  geom_line()  +
  ylab("Number of Countries represented in SE")

