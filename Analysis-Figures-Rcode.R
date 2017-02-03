
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF THE GEOGRAPHY OF EDITORIAL BOARDS
#This is a clone of the code in the Github Repo for analaysis of Gender and Editorial Boards (https://github.com/embruna/Editorial-Board-Gender).

#Set WD and load packages you need. Not all of which you need after all.
#setwd("-------")

library(tidyverse)
library(RecordLinkage)
library(stringdist)
#library(gdata)
library(grid)
library(gridExtra)
library(maps)
library(RColorBrewer)
library(reshape2)
require(rworldmap)
library(vegan)
library(WDI)

source("helpers.R")    #Code to plot all journals in one figure


  
  #CLear out everything from the environment 
  rm(list=ls())
  
  
  ######################################################
  # DATA UPLOAD 
  ######################################################
  
  # : load the individual CSV files and save them as dataframes
  
  # IMPORT WORLD BANK INDICATORS (downloaded 2/Dec/2015)
  WDI_data<-read.csv("WDI_data.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  row.names(WDI_data) <- WDI_data$iso3c     #Assigning row names in table for later search
  
  #list of categories of income, useful for analysis
  INCOMES <- c(  'High income: OECD', 'High income: nonOECD',
                 'Upper middle income','Lower middle income','Low income')
  
  #list of geographical regions, useful for analysis
  REGIONS <- c('North America', 'Europe & Central Asia','Sub-Saharan Africa',
               'East Asia & Pacific','Latin America & Caribbean',
               'South Asia','Middle East & North Africa')
  
  
  
  # IMPORT JOURNAL DATA
  
  # Import data from Cho et al 2014 PeerJ
  BITR<-read.csv("./ChoData/Biotropica_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BIOCON<-read.csv("./ChoData/Biocon_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ARES<-read.csv("./ChoData/ARES_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  AGRON<-read.csv("./ChoData/Agronomy_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  NAJFM<-read.csv("./ChoData/NAJFM_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  AJB<-read.csv("./ChoData/AJB_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  CONBIO<-read.csv("./ChoData/ConBio_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ECOLOGY<-read.csv("./ChoData/Ecology_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JECOL<-read.csv("./ChoData/JEcol_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JTE<-read.csv("./ChoData/JTE_EB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  
  # Import Data collected by 2015 UF Scientific Publishing Seminar 
  AGRON2<-read.csv("./Data2015/AGRON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  AMNAT<-read.csv("./Data2015/AMNAT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ARES2<-read.csv("./Data2015/ARES2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BIOCON2<-read.csv("./Data2015/BIOCON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BIOG<-read.csv("./Data2015/BIOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BITR2<-read.csv("./Data2015/BITR2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ECOG<-read.csv("./Data2015/ECOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  EVOL<-read.csv("./Data2015/EVOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #Still need to ID what an Editor vs EIC does when they transitoned to EIC
  FEM<-read.csv("./Data2015/FEM.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  FUNECOL<-read.csv("./Data2015/FUNECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JANE<-read.csv("./Data2015/JANE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JAPE<-read.csv("./Data2015/JAPE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JTE2<-read.csv("./Data2015/JTE2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JZOOL<-read.csv("./Data2015/JZOOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
  MARECOL<-read.csv("./Data2015/MARECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  NAJFM2<-read.csv("./Data2015/NAJFM2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  NEWPHYT<-read.csv("./Data2015/NEWPHYT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
  OECOL<-read.csv("./Data2015/OECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  OIKOS<-read.csv("./Data2015/OIKOS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #5 are missing country
  PLANTECOL<-read.csv("./Data2015/PLANTECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  
  # STILL MISSING SOME DATA 
  GCB<-read.csv("./Data2015/GCB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  # ONLY HAS 1995-2007. 2007-2008 in dropbox. Wiley Journal
  
  LECO<-read.csv("./Data2015/LECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  # #LE is missing 1985-1987 (started 1987), 2004, 2011-2014, 2015 Springer
  
  MEPS<-read.csv("./Data2015/MEPS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  # ONLY HAS 1989-1997. Have in folder 2010, 2011-2013, 2014-2015. what looks like 88,87,1985
  
  
  ######################################################
  # DATA CLEANUP AND ORGANIZATION: CHODATA
  ######################################################
  
  #Bind the data from Cho
  ChoData<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE) 
  
  source("Cho.Fix.R")
  ChoData_clean<-Cho.Fix(ChoData)
  ChoData_clean
  # write.csv(ChoData_clean, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ChoData_clean.csv", row.names = T) #export it as a csv file
  
  #Don't Need the original files or Messy ChoData cluttering up the Env't so lets delete
  rm(ChoData, BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE)
  
  
  ############################################################
  # DATA CLEANUP AND ORGANIZATION: CLASSDATA  
  ############################################################
  
  #Bind the data from 2015 workshop
  ClassData<-rbind(AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, EVOL, FEM, FUNECOL, 
                   JANE, JAPE, JTE2, JZOOL, LECO, MARECOL, NAJFM2, NEWPHYT, OECOL, OIKOS, PLANTECOL) 
  source("Class.Fix.R")
  ClassData_clean<-Class.Fix(ClassData)
  # write.csv(ClassData_clean, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData_clean.csv", row.names = T) #export it as a csv file
  
  # Don't Need the original files or Messy ClassData cluttering up the Env't so lets delete
  rm(ClassData,GCB, MEPS,AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, EVOL, FEM, FUNECOL, 
     JANE, JAPE, JTE2, JZOOL, LECO, MARECOL, NAJFM2, NEWPHYT, OECOL, OIKOS, PLANTECOL)
  
  # NEED TO Double check if tansley review, book review editors, IFE, also act as subject editors. 
  # OIKOS ADVISOR PANEL - are they also handling MSS? 
  # Evolution - the same TITLE (Editor) is often allocated to different categories (AE, SE, EIC)
  # 2x all special editors
  ##DOUBLE CHECK WHICH THESE ARE IN. IF THEY ARE IN NEW DATA CAN CORRECT!!!!!
  # 1) SYSTEMATIZE OTHER, SPECIAL, PRODUCTION in CATEGORY COLUMN
  # 2) EVOL: several titles missing 
  # 3) AMNAT: 1985-1992 has two volumes for each year. use oone? both? 
  # 4) AMNAT: some missing volume and issue data
  # 5) AMNAT: Need to correct AE for Editor
  # 6) Oecologia has several EIC's (plants, animals, etc)
  # 7 One name missing in Oecologia due to blurry pic
  #8) Removed MEPS, GCB because so many years missing.
  #Don't Need the original files or Messy ClassData cluttering up the Env't so lets delete
  
  
  str(ClassData_clean)
  summary(ClassData_clean)
  levels(ClassData_clean$CATEGORY)
  
  # THIS REMOVEA A FEW WITH BLANKS IN THE NAMES
  ClassData_clean <-filter(ClassData_clean, ClassData_clean$FIRST_NAME!="" & ClassData_clean$LAST_NAME!="")
  # Error Correction
  ####FIX THIS
  # ClassData[which(ClassData$JOURNAL==""),] #are there any with no journal?
  # ClassData[which(ClassData$FIRST_NAME==""),] #are there any with no 1st name?
  # ClassData[which(ClassData$LAST_NAME==""),] #are there any with no 1st name?
  
  
  #############################################################
  #
  # Function to determine the years missing in your dataset
  # yrs.missing(dataset,first year of interest,last year of interest)
  source("yrs.missing.R")
  yrs.missing<-yrs.missing(ClassData_clean,1985,2014)
  write.csv(yrs.missing, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData_missingYrs.csv", row.names = T) #export it as a csv file
  #
  #############################################################
  
  
  #############################################################
  # WHAT DATASETS WILL YOU DO ANALYSES WITH? BIND THEM TOGETHER 
  ##############################################################
  
  # Add an identifier for each dataset
  ChoData_clean$DATASET<-"Cho"
  ClassData_clean$DATASET<-"Class"
  #bind them together
  ALLDATA<-rbind(ChoData_clean,ClassData_clean)
  # convert your dataset identifier to a factor
  ALLDATA$DATASET<-as.factor(ALLDATA$DATASET)
  
  
  #############################################################
  # DO YOU WANT TO SUBSET TO CERTAIN GROUPS?

  # filter our the production staff
  ALLDATA <-filter(ALLDATA, ALLDATA$CATEGORY!="production")
  ALLDATA<-droplevels(ALLDATA)
  str(ALLDATA)
  #############################################################
  
  #############################################################
  # ADD AN INDEX TO SUBSET OF DATASET YOU WANT TO ANALYZE BASED
  # ON ANY CATEGORY OF INTEREST 

  # Add index based on NAME
  # First convert name to a factor
  ALLDATA<-arrange(ALLDATA,FirstInitialLast)
  ALLDATA$FirstInitialLast<-as.factor(ALLDATA$FirstInitialLast)
  ALLDATA <- transform(ALLDATA,author_id=as.numeric(FirstInitialLast))

  # Now make sure all names, cases, categories, etc. are consistent
  source("Name.check.R")
  NameSimilarityDF<-Name.check(ALLDATA,ALLDATA$FirstMiddleLast)
  write.csv(NameSimilarityDF, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/NameCheck_ALLDATA_ALLYRS.csv", row.names = T) #export it as a csv file
  # 
  
  
  # AFER YOU HAVE CHECKED THE NAMES FOR CONSISTENCY, NEED TO DISAMBIGUATE
  # The best way to disambiguate is as follows: 
  # 1. assign a different index to entries with different First Initial+Last Name (there aren't too many of there)
  # 2. Search for all that have same index BUT different first name
  
  
  source("Name.disambig.R")
  DisambigFile<-Name.disambig(ALLDATA)
  DisambigFile<-select(DisambigFile,-VOLUME,-ISSUE,-NOTES)
  write.csv(DisambigFile, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/DisambigList.csv", row.names = T) #export it as a csv file

  # Look over the DisambigFile and identify those that should have different author_id numbers.  
  # Delete the author_id from the one that needs a new one (ie Ånurag Agrawal and Aneil Agrawal have
  # author_id "2".  Keep to for Anurage and leave a blank cell for Aneil's author_id). Renumber the first column
  # from 1:nrows. call that column index then Save that as a csv file called FixList.csv
  # all columns must have a name
#   
#  FixList<-read.csv(file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/FixList.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
# rm(foo,foo2)
#  foo2<-ALLDATA
#  ALLDATA$FirstMiddleLast<-as.character(ALLDATA$FirstMiddleLast)
#  for (i in 1:nrow(FixList)) {
#    name_to_id<-slice(FixList,i)
#    name_to_id<-name_to_id$FirstMiddleLast
#    name_to_id<-as.character(name_to_id)
#    foo<-ALLDATA%>%filter(FirstMiddleLast==name_to_id) %>% mutate(author_id, max(ALLDATA$author_id)+1)
#    foo2<-rbind(foo,foo2)
#  }
#  
# 
#    
#    
#     if (ALLDATA$FirstMiddleLast==name_to_id){
#      ALLDATA$author_id<- max(ALLDATA$author_id)+1 #WORKING?
#    }
#  }
#    
#  
#     # name_to_id$FirstMiddleLast<-as.character(name_to_id$FirstMiddleLast)
#    
#    if (filter(ALLDATA$FirstMiddleLast==name_to_id$FirstMiddleLast)){
#       slice_(ALLDATA$author_id<-max(ALLDATA$author_id)+1)
#       }
#  } 
##########################################################
##########################################################
## End of section cleaning up the data and putting it
## in similar format for comparison and analysis 
##########################################################
##########################################################


  
  
######################################################
#
# STANDARDINZING THE COUNTRY CODES ON CLEAN DATASETS
# Make this a function
#
######################################################

# DATASET<-ChoData #OR 
# DATASET<-ClassData #OR
DATASET<-ALLDATA #OR 
str(DATASET)

#2x check - are there any with country missing?
MISSING=subset(DATASET, COUNTRY=="Unknown")
MISSING

source("Country.Codes.R")
DATASET<-Country.Codes(DATASET)
str(DATASET)
levels(DATASET$geo.code)
#we need to change yugoslavia to what?
#we need to add french guiana wold bank classficiation






# 
# #This line adds a column of country codes based on the country name
# #some countries may not be correctly coded
# DATASET$COUNTRY.CODE<-countrycode(DATASET$COUNTRY, "country.name", "iso3c", warn = TRUE)   #create new column with country ISO code
# 

#These lines add the income level and region level based on the editor country
DATASET$INCOME_LEVEL <- WDI_data[DATASET$geo.code, 'income']  #Making a new column of income level by country
DATASET$REGION <- WDI_data[DATASET$geo.code, 'region']  #Making a new column of income level by country

#subsetting data to only EIC, AE and SE classifications
DATASET <- DATASET[DATASET$CATEGORY %in% c('EIC', 'AE', 'SE'),]

#step 4: choose the temporal coverage
#use only 1985 to 2013 
DATASET<-DATASET[DATASET$YEAR>=1985 & DATASET$YEAR<=2013,]

#step 5: 2x that it all looks ok
summary(DATASET)
str(DATASET)

############################################################################
#
# BIND THEM UP AND ANALYZE!
#
############################################################################

# str(ChoData)
# str(ClassData)

AnalysisData<-DATASET %>% 
  select(-INSTITUTION,-NOTES,-GENDER, -VOLUME, -ISSUE, -TITLE, -INSTITUTION)


str(AnalysisData)
############################################################################################
# SHANNON DIVERSITY INDEX
############################################################################################

#subset to only 2014 data (with most journals with complete data)
AllJournals2014 <- AllJournals[AllJournals$YEAR == 2014,]

#cast data to the format accepted by the 'diversity' function
AllJournals2014cast <- dcast(data = AllJournals2014, JOURNAL ~ COUNTRY)

#Save journals list for using in the table
AllJournals2014JOURNAL.LIST <- AllJournals2014cast$JOURNAL 

#deleting journal column because 'diversity' function will fail if present
AllJournals2014cast <- AllJournals2014cast %>%  select(-JOURNAL)

#computing diversity
AllJournals2014Shannon <- diversity(AllJournals2014cast)

#Table with Results and Journals
AllJournals2014ShannonTable <- data.frame(AllJournals2014Shannon, row.names = AllJournals2014JOURNAL.LIST)




############################################################################################
# BAR PLOT TOTAL EDITORIAL MEMBERS BY COUNTRY (ALL JOURNALS, ALL YEARS)
# GROUPED COUNTRIES WITH SMALL SIZES
############################################################################################
#Group dataframe by geo.code
byCOUNTRY <- dplyr::group_by(AnalysisData, geo.code)

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each EIC only once
byCOUNTRY <- unique( byCOUNTRY[ , c('FirstMiddleLast', 'geo.code', 'JOURNAL') ] )

#Count the number of unique editors by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(FirstMiddleLast)))

#See countries with highest representations
byCOUNTRY[order(byCOUNTRY$number,decreasing = TRUE),][1:10,]

#Block countries from the n country to the lowest
n <- 10

#Getting a dataframe of the highest n
highest_n <- byCOUNTRY[order(byCOUNTRY$number,decreasing = TRUE),][1:n,]

#Getting the size of the grouped countries
grouped_number <- sum(byCOUNTRY$number) - sum(highest_n$number)

#appending the value to the table
highest_n[n + 1,] <- c('Others', grouped_number)
highest_n$number <- strtoi(highest_n$number)

#order countries in a factor mode
highest_n$geo.code <- factor(x = highest_n$geo.code,
                                levels = highest_n$geo.code)

highest_n$total=sum(highest_n$number) #this will allow you to calclulate % and plot that way
highest_n$percent=highest_n$number/highest_n$total*100

tiff(file = "Plots/COUNTRY_Editors.tiff",
     width = 500,
     height = 500)
#Plot of EIC numbers by country in decreasing number

country_plot<-ggplot(data=highest_n, aes(x=geo.code, y=percent)) +   #changed this to % instead of absolute #
  geom_bar(stat="identity") + 
  ylab('Editors (%)') +
  xlab('Country')+
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +        #sets the y axis breaks and range
  geom_bar(stat="identity", fill="gray60", colour="black") +              #sets the color and outline of the bar
  ggtitle("C")                                                            #title (identifies which panel it is)


country_plot<-country_plot + theme_classic() + theme(axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),        #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                     axis.title.y=element_text(colour="black", size = 18, vjust=2),           #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                     axis.text=element_text(colour="black", size = 16),                       #sets size and style of labels on axes
                                                     plot.title = element_text(hjust=0.02, vjust=-3, face="bold", size=22), #sets the position of the title 
                                                     plot.margin =unit(c(1,1,1,1.5), "lines"))                                #plot margin - top, right, bottom, left
  

country_plot

dev.off()






##############################################
# TABLE OF TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY BY INCOME OR REGION
# (ALL JOURNALS, ALL YEARS)
##############################################
#Group dataframe by geo.code
byCOUNTRY <- dplyr::group_by(AnalysisData, geo.code, CATEGORY)

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each editor only once
byCOUNTRY <- unique( byCOUNTRY[ , c('FirstMiddleLast', 'geo.code', 'JOURNAL', 'CATEGORY') ] )

#Count the number of unique editors by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(FirstMiddleLast)))

#Reshape the table from long to wide
byCOUNTRY <- dcast(data = byCOUNTRY,
                   formula = geo.code ~ CATEGORY, 
                   value.var = 'number')

#changing NA to 0
byCOUNTRY[is.na(byCOUNTRY)] <- 0

#Converting to proportion
byCOUNTRY['EIC_perc'] <-  byCOUNTRY$EIC / sum(byCOUNTRY$EIC)
byCOUNTRY['AE_perc'] <-  byCOUNTRY$AE / sum(byCOUNTRY$AE)
byCOUNTRY['SE_perc'] <-  byCOUNTRY$SE / sum(byCOUNTRY$SE)

#Assign each country a WDI income and region
byCOUNTRY$INCOME_LEVEL <- WDI_data[byCOUNTRY$geo.code, 'income']  #Making a new column of income level by country
byCOUNTRY$REGION <- WDI_data[byCOUNTRY$geo.code, 'region']  #Making a new column of income level by country

#Analyze by income all the countries
byCOUNTRY_income <- dplyr::group_by(byCOUNTRY, INCOME_LEVEL)
#sum the proportions of unique editors by category by country by INCOME
byCOUNTRY_income_table = summarize (byCOUNTRY_income,
                                    EIC = sum(EIC_perc),
                                    AE = sum(AE_perc),
                                    SE = sum(SE_perc))

#Analyze by region all the countries
byCOUNTRY_region <- dplyr::group_by(byCOUNTRY, REGION)
#sum the proportions of unique editors by category by country by REGION
byCOUNTRY_region_table = summarize (byCOUNTRY_region,
                                    EIC = sum(EIC_perc),
                                    AE = sum(AE_perc),
                                    SE = sum(SE_perc))

#rounding the percentages
byCOUNTRY_income_table[,2:4] <- round(byCOUNTRY_income_table[,2:4], 3) * 100
byCOUNTRY_region_table[,2:4] <- round(byCOUNTRY_region_table[,2:4], 3) * 100


#Constructing table to print

#Paste total number of editors to the dataframe column names
categorySizeNames <- paste(colnames(byCOUNTRY_region_table)[2:4],' (',
      c(sum(byCOUNTRY$EIC), sum(byCOUNTRY$AE), sum(byCOUNTRY$SE)),
      ')', sep = '')

colnames(byCOUNTRY_income_table)[2:4] <- categorySizeNames
colnames(byCOUNTRY_region_table)[2:4] <- categorySizeNames

#add a column of number of countres. This table may cause error after
#correcting countries
byCOUNTRY_income_table['n'] <- c(array(table(byCOUNTRY$INCOME_LEVEL)), 1)
byCOUNTRY_region_table['n'] <- c(array(table(byCOUNTRY$REGION)), 1)

#sorting by order of Subject editors
byCOUNTRY_income_table <- byCOUNTRY_income_table[order(unlist(byCOUNTRY_income_table[,4]), decreasing = TRUE),]
byCOUNTRY_region_table <- byCOUNTRY_region_table[order(unlist(byCOUNTRY_region_table[,4]), decreasing = TRUE),]

#Just changing the order of columns to make it easier to interpret
byCOUNTRY_income_table <- byCOUNTRY_income_table[,c(1,5,2,3,4)]
byCOUNTRY_region_table <-byCOUNTRY_region_table[,c(1,5,2,3,4)]

#printing tables
byCOUNTRY_income_table
byCOUNTRY_region_table

#printing USA and GBR values to add to table
round(100*byCOUNTRY[byCOUNTRY$geo.code == 'USA',5:7], 1)
round(100*byCOUNTRY[byCOUNTRY$geo.code == 'GBR',5:7], 1)


##############################################
# PLOT NUMBER OF COUNTRIES REPRESENTED BY YEAR BY JOURNAL ALL CATEGORIES
# WITH LINE ADDING HIGH INCOME COUNTRIES (OECD AND NON-OECD)
##############################################
#Group dataframe by CATEGORY, JOURNAL AND YEAR
COUNTRYYEAR <- dplyr::group_by(AnalysisData, JOURNAL, YEAR)

#Getting lists of high, med, low countries
for (i in unique(WDI_data$income)){
  assign(paste(i, 'list'), WDI_data$iso3c[WDI_data$income == i])
  print (i)
  print (WDI_data$iso3c[WDI_data$income == i])
}

head(COUNTRYYEAR)

# Table of number of countries represented by journal by country in all categories
# It also estimates the number of high income countries represented by year
COUNTRYYEAR_SUMMARY <- summarize (COUNTRYYEAR,
                                  COUNTRIES = length(unique(geo.code)),
                                  HIGHINCOME = sum(unique(geo.code) %in% 
                                                     c(array(`High income: OECD list`)))
)

#Converting to long format to easy plotting in ggplot
COUNTRYYEAR_SUMMARY <- melt(COUNTRYYEAR_SUMMARY,
                            id.vars = c('JOURNAL', 'YEAR'))


#PLOTS FOR EACH JOURNAL OF THE NUMBER OF COUNTRIES REPRESENTED IN
#SUBJECT EDITORS BY YEAR
for (i in unique(COUNTRYYEAR_SUMMARY$JOURNAL)){
  subset_JOURNAL_SE <- COUNTRYYEAR_SUMMARY[COUNTRYYEAR_SUMMARY$JOURNAL == i,]
  assign(paste(i, 'plot_COUNTRIES', sep = ''), ggplot(subset_JOURNAL_SE, 
                                                      aes(x = YEAR, y = value))
         + geom_line(size = 1.5, aes(colour = variable))
         + ylab("")
         + xlab("")
         + scale_y_continuous(limits=c(0, 25))
         + scale_x_continuous(limits=c(1985, 2013),
                              breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                              labels=c('1985', '', '', '2000', '', '2010'))
         + scale_color_manual(values=c("#000000", "#969696"),
                              name="",
                              labels=c('Total', 'High Income OECD'))
         + ggtitle(i)
  )
}

tiff(file = "Plots/COUNTRIES_byJOURNAL.tiff",
     width = 800,
     height = 700)
grid_arrange_shared_legend(bottom = 'Fig 2. Number of countries represented in the editorial board',
                           AJBplot_COUNTRIES,
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
                           JBIOGplot_COUNTRIES,
                           JECOLplot_COUNTRIES,
                           JTEplot_COUNTRIES,
                           JZOOLplot_COUNTRIES,
                           LECOplot_COUNTRIES,
                           MARECOLplot_COUNTRIES,
                           NAJFMplot_COUNTRIES,
                           NEWPHYTplot_COUNTRIES,
                           OECOLplot_COUNTRIES,
                           OIKOSplot_COUNTRIES,
                           PLANTECOplot_COUNTRIES
                           #AMNATplot_COUNTRIES,    #Something wrong with this graph
) 
dev.off()

##############################################
# PLOT EDITORIAL MEMBERS BY WDI INCOME CLASS OR REGION ADDING ALL JOURNALS
# CATEGORIES COMBINED
##############################################
#Group dataframe by iNCOME CLASS, CATEGORY, YEAR 
INCOME_byYEAR <- dplyr::group_by(AnalysisData, INCOME_LEVEL, YEAR)
#Group dataframe by REGION, CATEGORY, YEAR 
REGION_byYEAR <- dplyr::group_by(AnalysisData, REGION, YEAR)

#Count the number of unique editors by category by country BY year by INCOME
INCOME_byYEAR = summarize (INCOME_byYEAR,
                           number = length(unique(NAME)))
#Count the number of unique editors by category by country BY year by REGION
REGION_byYEAR = summarize (REGION_byYEAR,
                           number = length(unique(NAME)))

#Converting from long format to wide for easy sum below
INCOME_byYEAR <- dcast(data = INCOME_byYEAR,
                       formula = YEAR ~ INCOME_LEVEL, 
                       value.var = 'number')

#Converting from long format to wide for easy sum below
REGION_byYEAR <- dcast(data = REGION_byYEAR,
                       formula = YEAR ~ REGION, 
                       value.var = 'number')

#Combine tables for easier analysis
byYEAR <- merge(INCOME_byYEAR, REGION_byYEAR,by = c("YEAR"))

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
#First GRAPH OF REGIONS
class <- REGIONS    #REGIONS OR INCOMES
percentage <- TRUE    #ANALYSIS BY PERCENTAGE

if (percentage){
  class = paste(class, '_perc', sep = '')
}

#IT REDUCES DATABASE TO ONLY USED VARIABLES
byYEAR_subset <- byYEAR[,c('YEAR', class)]

#CONVERTING BACK TO LONG FORMAT TO EASY PLOTTING IN ggplot
byYEAR_subset <- melt(byYEAR_subset,
                      id.vars = c('YEAR'))

#Reordering factors to make a better plot
byYEAR_subset$variable <- factor(x = byYEAR_subset$variable,
                                 levels = c('North America_perc',
                                            'Europe & Central Asia_perc' ,
                                            'East Asia & Pacific_perc',
                                            'Latin America & Caribbean_perc',
                                            'Sub-Saharan Africa_perc' ,
                                            'Middle East & North Africa_perc',
                                            'South Asia_perc'))

# Creating and Saving plot
tiff(file = "Plots/REGION_AnalysisData.tiff",
     width = 500,
     height = 400)
ggplot(byYEAR_subset, 
       aes(x = YEAR, y = value, 
           colour = variable)) + geom_line(size = 1.5)  + 
  ylab(paste ("Proportion of Editors")) + 
  scale_colour_manual(labels=c("North America", 
                                 "Europe & Central Asia", 
                                 "East Asia & Pacific", 
                                 "Latin America & Caribbean", 
                                 "Sub-Saharan Africa",
                                 "Middle East & North Africa",
                                 "South Asia"),
                        values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                 "#F0E442", "#0072B2", "#D55E00"),
                      name = '') +
  theme(legend.position="bottom")  +
  guides(col = guide_legend(nrow = 3))
dev.off()


#VALUES TO ANALYZE################ MANUAL INPUT
#Second GRAPH OF INCOMES
class <- INCOMES    #REGIONS OR INCOMES
percentage <- TRUE    #ANALYSIS BY PERCENTAGE

if (percentage){
  class = paste(class, '_perc', sep = '')
}

#IT REDUCES DATABASE TO ONLY USED VARIABLES
byYEAR_subset <- byYEAR[,c('YEAR', class)]

#CONVERTING BACK TO LONG FORMAT TO EASY PLOTTING IN ggplot
byYEAR_subset <- melt(byYEAR_subset,
                      id.vars = c('YEAR'))

# Creating and Saving plot
tiff(file = "Plots/INCOME_AnalysisData.tiff",
     width = 500,
     height = 400)
ggplot(byYEAR_subset, 
       aes(x = YEAR, y = value, 
           colour = variable)) + geom_line(size = 1.5)  + 
  ylab(paste ("Proportion of Editors")) + 
  scale_colour_manual(labels=c("High OECD", 
                               "High Non-OECD", 
                               "Upper-Middle", 
                               "Lower-Middle", 
                               "Low"),
                      values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442"),
                      name = '') +
  theme(legend.position="bottom")  +
  guides(col = guide_legend(nrow = 2))
dev.off()

##############################################
# PLOTS OF EDITORIAL BOARD BY INCOME OR REGION BY YEAR BY JOURNAL
# EDITORIAL CATEGORIES COMBINED
##############################################
INCOME_byYEARJOURNAL <- dplyr::group_by(AnalysisData, 
                                        JOURNAL, YEAR, INCOME_LEVEL)
REGION_byYEARJOURNAL <- dplyr::group_by(AnalysisData, 
                                        JOURNAL, YEAR, REGION)

#Count the percentage of editors by category by income level
INCOME_byYEARJOURNAL <- summarize (INCOME_byYEARJOURNAL,
                                   number = length(NAME))
#Count the percentage of editors by category by REGION
REGION_byYEARJOURNAL <- summarize (REGION_byYEARJOURNAL,
                                   number = length(NAME))

#Converting from long format to wide for easy sum below for income
INCOME_byYEARJOURNAL <- dcast(data = INCOME_byYEARJOURNAL,
                              formula = JOURNAL + YEAR ~ INCOME_LEVEL, 
                              value.var = 'number')
#Converting from long format to wide for easy sum below for region
REGION_byYEARJOURNAL <- dcast(data = REGION_byYEARJOURNAL,
                              formula = JOURNAL + YEAR ~ REGION, 
                              value.var = 'number')

#Combine tables for easier analysis
byYEARJOURNAL <- merge(INCOME_byYEARJOURNAL,REGION_byYEARJOURNAL,
                       by = c("JOURNAL","YEAR"))

#Changing all NA values to zero, otherwise sum will return NA
byYEARJOURNAL[is.na(byYEARJOURNAL)] <- 0

#Size of SE editorial board sum by journal by year
byYEARJOURNAL['CATEGORY_size'] <- byYEARJOURNAL['High income: OECD'] + 
  byYEARJOURNAL['High income: nonOECD'] + 
  byYEARJOURNAL['Upper middle income'] +
  byYEARJOURNAL['Lower middle income'] +
  byYEARJOURNAL['Low income']


#Estimating percentage of editorial board for each year
for (i in c(INCOMES, REGIONS)){
  byYEARJOURNAL[paste(i, '_perc', sep = '')] <- byYEARJOURNAL[i] / byYEARJOURNAL$CATEGORY_size
}

#VALUES TO ANALYZE################ MANUAL INPUT
class <- REGIONS    #REGIONS OR INCOMES
percentage <- TRUE    #ANALYSIS BY PERCENTAGE

#making a Label for y axis
y.label <- 'Size of board
'
#Use percentage columns if
if (percentage){
  class = paste(class, '_perc', sep = '')
  y.label = 'Proportion of board'
}

#CONVERTING BACK TO LONG FORMAT TO EASY PLOTTING IN ggplot
byYEARJOURNAL_subset <- melt(byYEARJOURNAL[,c('JOURNAL','YEAR', class)],
                             id.vars = c('JOURNAL', 'YEAR'))

#Ordering factors to make a prettier color and to
# coincide colors with previous plots
byYEARJOURNAL_subset$variable <- factor(x = byYEARJOURNAL_subset$variable ,
                                 levels = c('North America_perc',
                                            'Europe & Central Asia_perc' ,
                                            'East Asia & Pacific_perc',
                                            'Latin America & Caribbean_perc',
                                            'Sub-Saharan Africa_perc' ,
                                            'Middle East & North Africa_perc',
                                            'South Asia_perc'  ) )



#this for loops create graphs per journal and saves each one
for (i in unique(byYEARJOURNAL_subset$JOURNAL)){
  #Subsetting data to the journal i
  byYEAR_subset_i <- byYEARJOURNAL_subset[byYEARJOURNAL_subset$JOURNAL == i,]
  
  assign(paste(i, 'plot_byYEAR', sep = ''), 
         ggplot(byYEAR_subset_i,
                aes(x = YEAR, y = value,    #x and y values
                    colour = variable))     #color and group lines by income category
         + geom_line(size = 1.1)      #Lines of width 1.5
         + ylab("")
         + xlab("")
         + scale_y_continuous(limits=c(0, 1),
                              breaks=c(0, 0.5, 1))
         + scale_x_continuous(limits=c(1985, 2013),
                              breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                              labels=c('1985', '', '', '2000', '', '2010'))
         + scale_colour_manual(labels=c("North America", 
                                        "Europe & Central Asia", 
                                        "East Asia & Pacific", 
                                        "Latin America & Caribbean", 
                                        "Sub-Saharan Africa",
                                        "Middle East & North Africa",
                                        "South Asia"),
                               values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                                        "#F0E442", "#0072B2", "#D55E00"),
                               name = 'Region')
         + ggtitle(i)
         #+ theme(legend.position="none")   #no legend for plots, 
         #+ scale_colour_grey(na.value = "white")   #Convert to gray scale
         #+ theme_bw() 
  )
}

tiff(file = "Plots/REGION_byJOURNAL.tiff",
     width = 800,
     height = 700)
grid_arrange_shared_legend(bottom = 'Fig 1. Proportion of editors according to geographic region',
                           AJBplot_byYEAR,
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
                           JBIOGplot_byYEAR,
                           JECOLplot_byYEAR,
                           JTEplot_byYEAR,
                           JZOOLplot_byYEAR,
                           LECOplot_byYEAR,
                           MARECOLplot_byYEAR,
                           NAJFMplot_byYEAR,
                           NEWPHYTplot_byYEAR,
                           OECOLplot_byYEAR,
                           OIKOSplot_byYEAR,
                           PLANTECOplot_byYEAR)
dev.off()

