
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF THE GEOGRAPHY OF EDITORIAL BOARDS
#This is a clone of the code in the Github Repo for analaysis of Gender and Editorial Boards (https://github.com/embruna/Editorial-Board-Gender).


#Please insert this line of code. Once you do be sure to commit.



#Set WD and load packages you need. Not all of which you need after all.
#setwd("-------")
library(countrycode)
library(dplyr)
#library(gdata)
library(ggplot2)
library(grid)
library(gridExtra)
library(maps)
library(RColorBrewer)
library(reshape2)
require(rworldmap)
#library(tidyr)
library(WDI)

source("helpers.R")    #Code to plot all journals in one figure

#CLear out everything from the environment 
rm(list=ls())

##################
#################
###DATA ENTRY AND CLEANUP
##################
#################
#Step 1: load the individual CSV files and save them as dataframes

#IMPORT WORLD BANK INDICATORS (downloaded 2/Dec/2015)
WDI_data<-read.csv("WDI_data.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
row.names(WDI_data) <- WDI_data$iso3c     #Assigning row names in table for later search

#list of categories of income, useful for analysis
INCOMES <- c(  'High income: OECD', 'High income: nonOECD',
               'Upper middle income','Lower middle income','Low income')

#list of geographical regions, useful for analysis
REGIONS <- c('North America', 'Europe & Central Asia','Sub-Saharan Africa',
             'East Asia & Pacific','Latin America & Caribbean',
             'South Asia','Middle East & North Africa')


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
GCB<-read.csv("GCB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
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

#This line adds a column of country codes based on the country name
#some countries may not be correctly coded
ALLJOURNALS$COUNTRY.CODE<-countrycode(ALLJOURNALS$COUNTRY, "country.name", "iso3c", warn = TRUE)   #create new column with country ISO code

#These lines add the income level and region level based on the editor country
ALLJOURNALS$INCOME_LEVEL <- WDI_data[ALLJOURNALS$COUNTRY.CODE, 'income']  #Making a new column of income level by country
ALLJOURNALS$REGION <- WDI_data[ALLJOURNALS$COUNTRY.CODE, 'region']  #Making a new column of income level by country

#subsetting data to only EIC, AE and SE classifications
ALLJOURNALS <- ALLJOURNALS[ALLJOURNALS$CATEGORY %in% c('EIC', 'AE', 'SE'),]

#step 4: chose the temporal coverage
#use only 1985 to 2013 yeara
ALLJOURNALS<-ALLJOURNALS[ALLJOURNALS$YEAR>=1985 & ALLJOURNALS$YEAR<=2013,]

#step 5: 2x that it all looks ok
summary(ALLJOURNALS)

#2x check - are there any with country missing?
MISSING=subset(ALLJOURNALS, subset=(COUNTRY=="?"))
MISSING

##############################################
#EDITORIAL BOARD SIZE
#Number of EIC, SE, AE, by year by journal
##############################################

#Group dataframe by CATEGORY, JOURNAL AND YEAR
countSIZE <- dplyr::group_by(ALLJOURNALS, CATEGORY, JOURNAL, YEAR)
#Count the editorial size over years
countSIZE_SUMMARY = summarize (countSIZE,
                               number = length(NAME))

#Plot of editorial size over years, SE EXAMPLE
ggplot(countSIZE_SUMMARY[countSIZE_SUMMARY$CATEGORY == 'SE',], 
       aes(x = YEAR, y = number, group = JOURNAL, colour = JOURNAL)) +
  geom_line()  +
  ylab("Size of Editorial Board (SE)")


##############################################
# TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY (ALL JOURNALS, ALL YEARS)
##############################################
#Group dataframe by COUNTRY.CODE and  Editor CATEGORY
byCOUNTRY <- dplyr::group_by(ALLJOURNALS, COUNTRY.CODE, CATEGORY)

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each EIC only once
byCOUNTRY <- unique( byCOUNTRY[ , c('NAME', 'COUNTRY.CODE', 'JOURNAL', 'CATEGORY') ] )

#Count the number of unique editors by category by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(NAME)))

#Reshape the table from long to wide
byCOUNTRY <- dcast(data = byCOUNTRY,
                   formula = COUNTRY.CODE ~ CATEGORY, 
                   value.var = 'number')

print(byCOUNTRY)


#########MANUAL INPUT
category <- 'SE'   #'EIC', 'AE' or 'SE'

#Subset to EIC column and remove NAs
byCOUNTRYsubset <- subset(byCOUNTRY, get(category) > 0)[, c("COUNTRY.CODE", category)]
#Sort descending EIC count
byCOUNTRYsubset$COUNTRY.CODE <-factor(byCOUNTRYsubset$COUNTRY.CODE, levels=byCOUNTRYsubset[order(-byCOUNTRYsubset[category]), "COUNTRY.CODE"])

#Plot of EIC numbers by country in decreasing number
ggplot(data=byCOUNTRYsubset, aes(x=COUNTRY.CODE, y=get(category))) +
  geom_bar(stat="identity") + 
  ggtitle(category) +
  ylab('Editors')
  

##############################################
# MAPS FOR  EDITORIAL MEMBERS BY COUNTRY BY CATEGORY ALL YEARS
##############################################
#Create an map object using our COUNTRY.CODE 
sPDF <- joinCountryData2Map( byCOUNTRY
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
breaksSE<-seq(0,2900, by=150)
mapCountryData(sPDF,
               nameColumnToPlot='SE',
               catMethod =breaksSE)

##############################################
# MAPS FOR EDITORIAL MEMBERS BY CATEGORY BY YEAR
##############################################
#Group dataframe by COUNTRY.CODE and  Editor CATEGORY
COUNTRY_YEAR <- dplyr::group_by(ALLJOURNALS, COUNTRY.CODE, CATEGORY, YEAR)

#Count the number of unique editors by category by country
COUNTRY_YEAR = summarize (COUNTRY_YEAR,
                          number = length(NAME))

#Reshape the table from long to wide
COUNTRY_YEAR <- dcast(data = COUNTRY_YEAR,
                      formula = COUNTRY.CODE + YEAR  ~ CATEGORY, 
                      value.var = 'number')

#VALUES TO ANALYZE################ MANUAL INPUT
YEAR <- 1990               #YEAR TO ANALYZE (1985 to 2013)
category <- 'AE'           #'EIC', 'AE', or 'SE'
breaks<- 'pretty'  #'pretty' for AUTO or seq(0,2000, by=1) for more control
percentage <- FALSE     #Scale the map to percentage of editors by year
  
#Create an map object using our COUNTRY.CODE 
COUNTRY_YEAR_subset <-
  COUNTRY_YEAR[COUNTRY_YEAR$YEAR == YEAR,c('COUNTRY.CODE', category)]

map.title <- paste(category, YEAR, sep = ' ')

if (percentage){
  COUNTRY_YEAR_subset[,2] <-  COUNTRY_YEAR_subset[,2]/max(COUNTRY_YEAR_subset[,2], na.rm = TRUE)
  map.title <- paste('%', map.title, sep =' ')
}

sPDF <- joinCountryData2Map( COUNTRY_YEAR_subset
                             ,joinCode = "ISO3"
                             ,nameJoinColumn = "COUNTRY.CODE")
#EIC Map 1985
mapCountryData(sPDF,
               nameColumnToPlot=category,
               catMethod = breaks,
               colourPalette = 'white2Black',
               mapTitle = map.title)


##############################################
# PLOT NUMBER OF COUNTRIES REPRESENTED BY YEAR BY JOURNAL BY CATEGORY
##############################################
#Group dataframe by CATEGORY, JOURNAL AND YEAR
countCOUNTRYYEAR <- dplyr::group_by(ALLJOURNALS, CATEGORY, JOURNAL, YEAR)

#Table of number of countries represented by editor category, by journal by country
countCOUNTRYYEAR_SUMMARY <- summarize (countCOUNTRYYEAR,
                                       COUNTRIES = length(unique(COUNTRY.CODE)))

#length(unique(countCOUNTRYYEAR$COUNTRY.CODE))
#PLOTS FOR EACH JOURNAL OF THE NUMBER OF COUNTRIES REPRESENTED IN
#SUBJECT EDITORS BY YEAR
for (i in unique(countCOUNTRYYEAR_SUMMARY$JOURNAL)){
  subset_JOURNAL_SE <- countCOUNTRYYEAR_SUMMARY[countCOUNTRYYEAR_SUMMARY$CATEGORY == 'SE' &
                                                  countCOUNTRYYEAR_SUMMARY$JOURNAL == i,]
  assign(paste(i, 'plot_COUNTRIES', sep = ''), ggplot(subset_JOURNAL_SE, 
         aes(x = YEAR, y = COUNTRIES))
         + geom_line(size = 1.5)
         + ylab("Countries in Board")
         + xlab("")
         + scale_y_continuous(limits=c(0, 25))
         + scale_x_continuous(limits=c(1985, 2013),
                              breaks=c(1985, 2000, 2013))
         + ggtitle(i)
  )
}


multiplot(AJBplot_COUNTRIES,
          AREESplot_COUNTRIES,
          BIOCONplot_COUNTRIES,
          BITRplot_COUNTRIES,
          CONBIOplot_COUNTRIES,
          ECOGRAPHYplot_COUNTRIES,
          ECOLOGYplot_COUNTRIES,
          Evolutionplot_COUNTRIES,
          FEMplot_COUNTRIES,
          FUNECOLplot_COUNTRIES,
          JANEplot_COUNTRIES,
          JAPEplot_COUNTRIES,
          JECOLplot_COUNTRIES,
          `JOURNAL OF BIOGEOGRAPHYplot_COUNTRIES`,
          JTEplot_COUNTRIES,
          MARECOLplot_COUNTRIES,
          NAJFMplot_COUNTRIES,
          OECOLplot_COUNTRIES,
          OIKOSplot_COUNTRIES,
          PLANTECOplot_COUNTRIES,
          #AMNATplot_COUNTRIES,    #Something wrong with this graph
          cols = 5) 


##############################################
# PERCENTAGE OF EDITORIAL MEMBERS ADDING ALL JOURNALS
# BY WDI INCOME CLASS AND BY REGION
##############################################
#Group dataframe by iNCOME CLASS, CATEGORY, YEAR 
INCOME_byYEAR <- dplyr::group_by(ALLJOURNALS, INCOME_LEVEL, CATEGORY, YEAR)
#Group dataframe by REGION, CATEGORY, YEAR 
REGION_byYEAR <- dplyr::group_by(ALLJOURNALS, REGION, CATEGORY, YEAR)

#Count the number of unique editors by category by country BY year by INCOME
INCOME_byYEAR = summarize (INCOME_byYEAR,
                           number = length(unique(NAME)))
#Count the number of unique editors by category by country BY year by REGION
REGION_byYEAR = summarize (REGION_byYEAR,
                           number = length(unique(NAME)))

#Converting from long format to wide for easy sum below
INCOME_byYEAR <- dcast(data = INCOME_byYEAR,
                       formula = CATEGORY + YEAR ~ INCOME_LEVEL, 
                       value.var = 'number')

#Converting from long format to wide for easy sum below
REGION_byYEAR <- dcast(data = REGION_byYEAR,
                       formula = CATEGORY + YEAR ~ REGION, 
                       value.var = 'number')

#Combine tables for easier analysis
byYEAR <- merge(INCOME_byYEAR, REGION_byYEAR,by = c("CATEGORY","YEAR"))

#Changing all NA values to zero, otherwise sum will return NA
byYEAR[is.na(byYEAR)] <- 0

#Size of editorial board ALL JOURNALS by year
byYEAR['CATEGORY_size'] <- byYEAR['High income: OECD'] + 
  byYEAR['High income: nonOECD'] + 
  byYEAR['Upper middle income'] +
  byYEAR['Lower middle income'] +
  byYEAR['Low income']


#Estimating percentage of editorial board for each categorty
for (i in c(INCOMES, REGIONS)){
  byYEAR[paste(i, '_perc', sep = '')] <- byYEAR[i] / byYEAR$CATEGORY_size
}

#Changing all NA values to zero, otherwise sum will return NA
byYEAR[is.na(byYEAR)] <- 0

#VALUES TO ANALYZE################ MANUAL INPUT
class <- REGIONS    #REGIONS OR INCOMES
percentage <- TRUE    #ANALYSIS BY PERCENTAGE
category <- 'SE'        #CATEGORY OF EDITORS: 'EIC', 'AE' or 'SE'

if (percentage){
  class = paste(class, '_perc', sep = '')
}

#IT REDUCES DATABASE TO ONLY USED VARIABLES
byYEAR_subset <- byYEAR[,c('CATEGORY','YEAR', class)]
  
#CONVERTING BACK TO LONG FORMAT TO EASY PLOTTING IN ggplot
byYEAR_subset <- melt(byYEAR_subset,
                      id.vars = c('CATEGORY', 'YEAR'))

#SUBJECT EDITORS
ggplot(byYEAR_subset[byYEAR_subset$CATEGORY == category,], 
       aes(x = YEAR, y = value, 
           colour = variable)) + geom_line(size = 1.5)  + 
  ylab(paste ("Number of ", category))
#+ scale_colour_grey(na.value = "white")  + theme_bw()


##############################################
# PERCENTAGE OF EDITORIAL BOARD by JOURNAL
# BY THE DIFFERENT INCOME LEVELS AND REGION 
##############################################
INCOME_byYEARJOURNAL <- dplyr::group_by(ALLJOURNALS, 
                                        JOURNAL, YEAR, INCOME_LEVEL, CATEGORY)
REGION_byYEARJOURNAL <- dplyr::group_by(ALLJOURNALS, 
                                        JOURNAL, YEAR, REGION, CATEGORY)

#Count the percentage of editors by category by income level
INCOME_byYEARJOURNAL <- summarize (INCOME_byYEARJOURNAL,
                                   number = length(NAME))
#Count the percentage of editors by category by REGION
REGION_byYEARJOURNAL <- summarize (REGION_byYEARJOURNAL,
                                   number = length(NAME))

#Converting from long format to wide for easy sum below for income
INCOME_byYEARJOURNAL <- dcast(data = INCOME_byYEARJOURNAL,
                                   formula = JOURNAL + YEAR + CATEGORY ~ INCOME_LEVEL, 
                                   value.var = 'number')
#Converting from long format to wide for easy sum below for region
REGION_byYEARJOURNAL <- dcast(data = REGION_byYEARJOURNAL,
                              formula = JOURNAL + YEAR + CATEGORY ~ REGION, 
                              value.var = 'number')

#Combine tables for easier analysis
byYEARJOURNAL <- merge(INCOME_byYEARJOURNAL,REGION_byYEARJOURNAL,
                       by = c("JOURNAL","CATEGORY","YEAR"))

#Changing all NA values to zero, otherwise sum will return NA
byYEARJOURNAL[is.na(byYEARJOURNAL)] <- 0

#Size of SE editorial board sum by journal by year
byYEARJOURNAL['CATEGORY_size'] <- byYEARJOURNAL['High income: OECD'] + 
  byYEARJOURNAL['High income: nonOECD'] + 
  byYEARJOURNAL['Upper middle income'] +
  byYEARJOURNAL['Lower middle income'] +
  byYEARJOURNAL['Low income']


#Estimating percentage of editorial board for each categorty
for (i in c(INCOMES, REGIONS)){
  byYEARJOURNAL[paste(i, '_perc', sep = '')] <- byYEARJOURNAL[i] / byYEARJOURNAL$CATEGORY_size
}


#VALUES TO ANALYZE################ MANUAL INPUT
class <- REGIONS    #REGIONS OR INCOMES
percentage <- TRUE    #ANALYSIS BY PERCENTAGE
category <- 'AE'        #CATEGORY OF EDITORS

#Use percentage columns if
if (percentage){
  class = paste(class, '_perc', sep = '')
}

#CONVERTING BACK TO LONG FORMAT TO EASY PLOTTING IN ggplot
byYEARJOURNAL_subset <- melt(byYEARJOURNAL[,c('JOURNAL', 'CATEGORY','YEAR', class)],
                             id.vars = c('JOURNAL', 'CATEGORY', 'YEAR'))
#this for loops create graphs per journal and saves each one
for (i in unique(byYEARJOURNAL_subset$JOURNAL)){
  #Subsetting data to the journal i
  byYEAR_subset_i <- byYEARJOURNAL_subset[byYEARJOURNAL_subset$JOURNAL == i &
                                            byYEARJOURNAL_subset$CATEGORY == category,]
  
    assign(paste(i, 'plot_byYEAR', sep = ''), 
         ggplot(byYEAR_subset_i,
                aes(x = YEAR, y = value,    #x and y values
                    colour = variable))     #color and group lines by income category
         + geom_line(size = 1.5)      #Lines of width 1.5
         + ylab(paste("Number of", category))
         + xlab("")
         + scale_x_continuous(limits=c(1985, 2013),
                              breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                              labels=c('1985', '', '', '2000', '', '2010'))
         + ggtitle(i)
         #+ theme(legend.position="none")   #no legend for plots, 
         #+ scale_colour_grey(na.value = "white")   #Convert to gray scale
         #+ theme_bw() 
  )
}

grid_arrange_shared_legend(AJBplot_byYEAR,
                           AREESplot_byYEAR,
                           BIOCONplot_byYEAR,
                           BITRplot_byYEAR,
                           CONBIOplot_byYEAR,
                           ECOGRAPHYplot_byYEAR,
                           ECOLOGYplot_byYEAR,
                           Evolutionplot_byYEAR,
                           FEMplot_byYEAR,
                           FUNECOLplot_byYEAR,
                           JANEplot_byYEAR,
                           JAPEplot_byYEAR,
                           JECOLplot_byYEAR,
                           `JOURNAL OF BIOGEOGRAPHYplot_byYEAR`,
                           JTEplot_byYEAR,
                           MARECOLplot_byYEAR,
                           NAJFMplot_byYEAR,
                           OECOLplot_byYEAR,
                           OIKOSplot_byYEAR,
                           PLANTECOplot_byYEAR)

