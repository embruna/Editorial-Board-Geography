
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
  ######################################################
  #
  # DATA UPLOAD 
  #
  ######################################################
  ######################################################
  # load the individual CSV files and save them as dataframes
  
  # IMPORT WORLD BANK INDICATORS (downloaded 2/Dec/2015)
  WDI_data<-read.csv("WDI_data.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  row.names(WDI_data) <- WDI_data$iso3c     #Assigning row names in table for later search
  
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
  # DATA CLEANUP AND ORGANIZATION: CHO et al DATA
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
  # DATA CLEANUP AND ORGANIZATION: 2015 CLASS  
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
  
  # THIS REMOVEA A FEW WITH BLANKS IN THE NAMES
  ClassData_clean <-filter(ClassData_clean, ClassData_clean$FIRST_NAME!="" & ClassData_clean$LAST_NAME!="")
  # Error Correction
  
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
  # CHOSE DATASETS TO ANALYSE AND BIND THEM TOGETHER 
  ##############################################################

  # Add an identifier for each dataset
  ChoData_clean$DATASET<-"Cho"
  ClassData_clean$DATASET<-"Class"
  #bind them together
  ALLDATA<-rbind(ChoData_clean,ClassData_clean)
  # convert your dataset identifier to a factor
  ALLDATA$DATASET<-as.factor(ALLDATA$DATASET)
  
  #SImplify by removing the original datasets from the environment
  rm(ChoData_clean,ClassData_clean)
  
  #############################################################
  # DELETE PRODUCTION STAFF
  #############################################################
  
  ALLDATA<-filter(ALLDATA, ALLDATA$CATEGORY!="production")
  ALLDATA<-droplevels(ALLDATA)
  str(ALLDATA)
  
  #############################################################
  # ADD AN INDEX TO SUBSET OF DATASET YOU WANT TO ANALYZE BASED
  # ON ANY CATEGORY OF INTEREST 
  #############################################################
  # Add index based on NAME, to do so First convert name to a factor
  ALLDATA<-arrange(ALLDATA,FirstInitialLast)
  ALLDATA$FirstInitialLast<-as.factor(ALLDATA$FirstInitialLast)
  ALLDATA <- transform(ALLDATA,editor_id=as.numeric(FirstInitialLast))
  
  
  ######################################################
  ######################################################
  #
  # NAME CORRECTION AND DISAMIGUATION
  #
  ######################################################
  ######################################################
 
  
  
  #############################################################
  # NAME COMPARISON AND SPELL CHECK
  ##############################################################
  # This function will compare all names  to each other to help ID 
  # spelling mistakes, cases where names are similar enough to warrant
  # 2x, middle initials, etc. This will makeit easier to assign a ID 
  # number to each editor for disambiguation
  
  
  source("Name.check.R")
  NameSimilarityDF<-Name.check(ALLDATA,ALLDATA$FirstMiddleLast)
  write.csv(NameSimilarityDF, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/NameCheck_ALLDATA_ALLYRS.csv", row.names = T) #export it as a csv file

  
  # AFER YOU HAVE CHECKED THE NAMES FOR CONSISTENCY, NEED TO DISAMBIGUATE
  # The best way to disambiguate is as follows: 
  # 1. assign a different index to entries with different First Initial+Last Name (there aren't too many of there)
  # 2. Search for all that have same index BUT different first name
  
  
  #############################################################
  # NAME DISAMBIGUATION AND ASSIGNING ID NUMBER TO EACH EDITOR 
  ##############################################################
  
  source("Name.disambig.R")
  DisambigFile<-Name.disambig(ALLDATA)
  DisambigFile<-select(DisambigFile,-VOLUME,-ISSUE,-NOTES)
  write.csv(DisambigFile, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/DisambigList.csv", row.names = T) #export it as a csv file

  # Look over the DisambigFile and identify those that should have different editor_id numbers.  
  # Delete the editor_id from the one that needs a new one (ie Ånurag Agrawal and Aneil Agrawal have
  # editor_id "2".  Keep to for Anurage and leave a blank cell for Aneil's editor_id). Renumber the first column
  # from 1:nrows. call that column index then Save that as a csv file called FixList.csv
  # all columns must have a name
#   
 FixList<-read.csv(file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/FixList.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
 
 # ALLDATA$FirstMiddleLast<-as.character(ALLDATA$FirstMiddleLast)
 # 
# foo2$editor_id<-FixList[match(foo2$FirstMiddleLast, FixList$FirstMiddleLast),17]

for (i in 1:nrow(FixList)){
  newid=max(ALLDATA$editor_id)+1
  for (j in 1:nrow(ALLDATA)){
if(FixList$FirstMiddleLast[i]==ALLDATA$FirstMiddleLast[j]) {
  ALLDATA$editor_id[j] <- newid
}
}
}
ALLDATA$editor_id<-as.factor(ALLDATA$editor_id) #COnvert author id to factor to make it easier to count

rm(FixList)

######################################################
######################################################
#
# ADDING + STANDARDINZING 3-digit COUNTRY CODES FOR EACH EDITOR
#
######################################################
######################################################

#2x check - are there any with country missing?
MISSING=subset(ALLDATA, COUNTRY=="Unknown")
MISSING

source("Country.Codes.R")
ALLDATA<-Country.Codes(ALLDATA)
levels(ALLDATA$geo.code)
#we need to change yugoslavia to what?
#we need to add french guiana wold bank classficiation


######################################################
######################################################
#
# ADDING COUNTRY INCOME LEVEL AND REGION FOR EACH EDITOR
#
######################################################
######################################################

#These lines add the income level and region level based on the editor country
ALLDATA$INCOME_LEVEL <- WDI_data[as.character(ALLDATA$geo.code), 'income']  #Making a new column of income level by country
ALLDATA$REGION <- WDI_data[as.character(ALLDATA$geo.code), 'region']  #Making a new column of income level by country

#step 4: Changing the order of CATEGORY, INCOME_LEVEL, REGION and JOURNAL factors.
#This is then used to have always the same order of the lines in future plots and tables
INCOMES.ORDERED.LIST <- c(  'High income: OECD', 'High income: nonOECD',
                            'Upper middle income','Lower middle income','Low income')

#list of geographical regions, useful for analysis and to give them an order in plots
REGIONS.ORDERED.LIST <- c('North America', 'Europe & Central Asia','East Asia & Pacific',
                          'Latin America & Caribbean', 'Sub-Saharan Africa',
                          'South Asia','Middle East & North Africa')

ALLDATA$CATEGORY <- factor(x = ALLDATA$CATEGORY, levels = c('EIC', 'AE', 'SE'))
ALLDATA$INCOME_LEVEL <-  factor(x =  ALLDATA$INCOME_LEVEL, levels = INCOMES.ORDERED.LIST)
ALLDATA$REGION <-  factor(x =  ALLDATA$REGION, levels = REGIONS.ORDERED.LIST)
ALLDATA$JOURNAL <- factor (x = ALLDATA$JOURNAL, levels = sort(levels(ALLDATA$JOURNAL)))  #Sorted Alphabetically

rm(WDI_data)

######################################################
######################################################
#
# ANALYSES
#
######################################################
######################################################

# SELECT temporal coverage for analyses
AnalysisData<-ALLDATA[ALLDATA$YEAR>=1985 & ALLDATA$YEAR<=2015,]


#subsetting data to only EIC, AE and SE classifications
AnalysisData <- AnalysisData[AnalysisData$CATEGORY %in% c('EIC', 'AE', 'SE'),]


# DELETE unecessary columns 
AnalysisData<-AnalysisData %>% 
  select(-INSTITUTION,-NOTES,-GENDER, -VOLUME, -ISSUE, -TITLE, -INSTITUTION)


#############################################################
# TOTAL NUMBER OR COUNTRIES &  EDITORS
##############################################################
# 
# # 1985-2015
# str(AnalysisData) #levels of geo.code
# 
# # TOTAL COUNTRIES 1985-1995
# Countries8595<-filter(AnalysisData, YEAR >1984 & YEAR< 1996)
# Countries8595<-droplevels(Countries8595)
# str(Countries8595$geo.code)
# str(Countries8595$editor_id)
# 
# # TOTAL COUNTRIES  & Editors 1996-2005
# Countries9605<-filter(AnalysisData, YEAR >1995 & YEAR< 2006)
# Countries9605<-droplevels(Countries9605)
# str(Countries9605$geo.code)
# str(Countries9605$editor_id)
# 
# # TOTAL COUNTRIES  & Editors 2006-2015
# Countries0615<-filter(AnalysisData, YEAR >2005 & YEAR< 2016)
# Countries0615<-droplevels(Countries0615)
# str(Countries0615$geo.code)
# str(Countries0615$editor_id)
# 


##############################################################
# DIVERSITY (SHANNON) OF COUNTRIES ON ED BOARD IN EACH YEAR  
##############################################################

library(vegan)
#subset to only 2014 data (with most journals with complete data)

# What year do you want to anlayze?
# div_year<-2000
# what do you want to measure diversity of? 
# indicator=geo.code 
# indicator=INCOME_LEVEL
# indicator=REGION

AnalysisDiv <-AnalysisData


#cast data to the format accepted by the 'diversity' function
# AnalysisData2014cast <- dcast(data = AnalysisData2014, JOURNAL ~ geo.code)
# 
# # OR using tidyr
# AnalysisData2014long<-count(AnalysisData2014, JOURNAL, country = geo.code)
# AnalysisData2014wide<-spread(AnalysisData2014long, country, n)
# AnalysisData2014wide[is.na(AnalysisData2014wide)] <- 0

# more efficient to pipe:
  
AnalysisDivwide<-AnalysisDiv %>% count(JOURNAL, YEAR, divmetric = geo.code) %>% spread(divmetric, n)

AnalysisDivwide[is.na(AnalysisDivwide)] <- 0
AnalysisDivwide<-as.data.frame(AnalysisDivwide)

#Save journals list for using in the table
AnalysisDivJOURNAL.LIST <- AnalysisDivwide$JOURNAL 
AnalysisDivYEAR.LIST <- AnalysisDivwide$YEAR 
#deleting journal column because 'diversity' function will fail if present
# AnalysisDivcast <- AnalysisDivcast %>%  select(-JOURNAL)
AnalysisDivwide<-as.data.frame(AnalysisDivwide)
# AnalysisDivwide <-select(AnalysisDivwide,-JOURNAL, -YEAR)
# colnames(AnalysisDivwide)

#computing diversity
ShannonDiv <- diversity(AnalysisDivwide %>% select(-JOURNAL, -YEAR)) #Need to strip away the journal and year columns for vegan to do the analysis


# Table DIVERSITY with Results and Journals
ShannonDivTable <- data.frame(ShannonDiv)
ShannonDivTable$JOURNAL <-AnalysisDivJOURNAL.LIST  #Add journal name as a column
ShannonDivTable$YEAR <-AnalysisDivYEAR.LIST #Add year as a column
ShannonDivTable<-rename(ShannonDivTable, ShannonDiv=ShannonDiv) #rename the columns
ShannonDivTable <- ShannonDivTable[c("JOURNAL","YEAR","ShannonDiv")] #reorder the columns
# ShannonDivTable<-arrange(ShannonDivTable, YEAR, desc(ShannonDiv)) # sort in descending order
# ShannonDivTable

##############################################################
# COUNT OF NO. OF EDITORS AND COUNTRIES EACH BOARD IN EACH YEAR
##############################################################

# Count by how many countries on each board each year for each journal 
N_Countries<-AnalysisDivwide %>% gather("COUNTRY", "N_Editors", 3:ncol(AnalysisDivwide)) %>% 
  group_by(JOURNAL, YEAR) %>% 
  tally(N_Editors>=1)  %>% rename(N_countries=n)
N_Countries<-as.data.frame(N_Countries)

# Count by how many people on the Ed Board each year for each journal 
N_Editors<-mutate(AnalysisDivwide, N_editors = rowSums(select(AnalysisDivwide,3:ncol(AnalysisDivwide)))) %>% select(JOURNAL, YEAR, N_editors)

# Put them together in 1 file dataframe 
ED.GEO.COUNTS<-full_join(N_Countries, N_Editors,by = c("YEAR" = "YEAR", "JOURNAL" = "JOURNAL")) 

ED.GEO.COUNTS<-full_join(ED.GEO.COUNTS, ShannonDivTable,by = c("YEAR" = "YEAR", "JOURNAL" = "JOURNAL")) 
# Pielou’s evenness J = H0/log(S) is easily found as: H/log(specnumber(BCI))
# where specnumber is a simple vegan function to find the numbers of species.

ED.GEO.COUNTS<-mutate(ED.GEO.COUNTS, ratio = N_countries/N_editors)
ED.GEO.COUNTS<-mutate(ED.GEO.COUNTS, Geo.Eveness = ShannonDiv/log(N_countries))



######################################################
######################################################
#
# FIGURES AND TABLES
#
######################################################
######################################################


############################################################################################
# MEDIAN, MIN AND MAX NUMBER OF COUNTRIES REPRESENTED IN EDITORIAL BOARDS
############################################################################################
#list of unique countries by journal by year
RepresentedCountries <- unique( AnalysisData[ , c('JOURNAL', 'YEAR', 'geo.code') ] )

#count unique countries by journal by year
RepresentedCountriesCount <- as.data.frame(RepresentedCountries %>% count(YEAR, JOURNAL)) 

# WE SHOULD REALLY BE CONSIDERING COUNTRIES PER NUMEBR OF EDITORS (Ratio above)
# I THINK WE HAVE SOME REDUNDANT DATA ORGANIZATION HERE 

plotA <- ggplot(data = RepresentedCountriesCount, aes(x = YEAR, y = n)) + 
  stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", alpha=0.3, colour = NA) +
  stat_summary(fun.y = median, geom='line', size = 1.1) + 
  ggtitle('A') + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '1990', '1995', '2000', '2005', '2010')) + 
  ylab("Countries on Ed. Board (median)") + 
  xlab("Year") + 
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00"), name = '') +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00"), name = '') + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  guides(col = guide_legend(ncol = 1))

  plotA <-plotA + theme_classic()+
    theme(axis.text=element_text(colour="black", size = 14),
            axis.title.x=element_text(colour="black", size = 18),           #Sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
            axis.title.y=element_text(colour="black", size = 18))

plotA



############################################################################################
# MEDIAN, MIN AND MAX SHANNON IN EDITORIAL BOARDS
############################################################################################


plotB <- ggplot(data = ShannonDivTable, aes(x = YEAR, y = ShannonDiv)) + 
  stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", alpha=0.3, colour = NA) +
  stat_summary(fun.y = median, geom='line', size = 1.1) + 
  ggtitle('B') + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '1990', '1995', '2000', '2005', '2010')) + 
  ylab("Shannon Div. Index") + 
  xlab("Year") + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  guides(col = guide_legend(ncol = 1)) + 
  theme(panel.grid.minor = element_blank())


plotB <-plotB + theme_classic()+
  theme(axis.text=element_text(colour="black", size = 14),
        axis.title.x=element_text(colour="black", size = 18),           #Sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18))


plotB


############################################################################################
# BAR PLOT OF EDITORIAL MEMBERS OF TOP 8 COUNTRIES. COMPLETE POOL OF EDITORS
# GROUPED COUNTRIES WITH SMALL SIZES
############################################################################################

#Getting a unique authors list (Authors can be in >1 Year and >1 Journal)
#Should sum to 3895
UniqueAuthors <- unique( AnalysisData[ , c('editor_id', 'geo.code') ] )

#Count geo.code based on authors 
CountryEditorsCount <- as.data.frame(UniqueAuthors %>% count(geo.code = geo.code))

#Group dataframe by geo.code
byCOUNTRY <- group_by(AnalysisData, editor_id)
byCOUNTRY<-as.data.frame(byCOUNTRY)

# THIS IS STILL COUNTING THE SAME EDITOR MULTIPLE TIMES

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each EIC only once
byCOUNTRY <- unique( byCOUNTRY[ , c('editor_id', 'geo.code', 'JOURNAL') ] ) 
byCOUNTRY<- arrange(byCOUNTRY, editor_id)
distinct_df = byCOUNTRY %>% first(editor_id)
foo<-byCOUNTRY %>% count(editor_id)
foo<-byCOUNTRY %>% count(geo.code)
#Count the number of unique editors by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(editor_id)))

#See countries with highest representations
CountryEditorsCount[order(CountryEditorsCount$n,decreasing = TRUE),][1:10,]

#Change factor to character for easier management
CountryEditorsCount$geo.code <- as.character(CountryEditorsCount$geo.code )

#Block countries from the n country to the lowest
n <- 10

#Getting a dataframe of the highest n
highest_n <- CountryEditorsCount[order(CountryEditorsCount$n,decreasing = TRUE),][1:n,]

#Getting the size of the grouped countries
grouped_n <- sum(CountryEditorsCount$n) - sum(highest_n$n)

#appending the value to the table

highest_n<-add_row(highest_n, geo.code = "Others", number = grouped_number)
# highest_n$number <- strtoi(highest_n$number)

#order countries in a factor mode
# highest_n$geo.code <- factor(x = highest_n$geo.code,
#                                 levels = highest_n$geo.code)
# 
# highest_n$total=sum(highest_n$number) #this will allow you to calclulate % and plot that way
highest_n$percent=highest_n$number/sum(highest_n$number)*100

sum(highest_n[2])
tiff(file = "Plots/COUNTRY_Editors.tiff",
     width = 500,
     height = 500)
#Plot of EIC numbers by country in decreasing number


#Final Plot to be pasted in multipanel plot
plotC<-ggplot(data=highest_n, aes(x=geo.code, y=n)) +
  theme_minimal() + 
  geom_bar(stat="identity") + 
  ylab('Editors') +
  xlab('Country') +
  scale_x_continuous(breaks = NA) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  ggtitle("C")

plotC

##############################################
# MAKING MULTIPLE PLOT FIGURE 1, A, B, C AND D?
##############################################

tiff(file = "Plots/Fig1.tiff",
     width = 2200,
     height = 1800,
     res = 300,
     compression = 'lzw')
multiplot(plotA, plotC, plotB, cols = 2)
dev.off()



##############################################
# TABLE OF TOTAL EDITORIAL MEMBERS BY COUNTRY BY CATEGORY BY INCOME OR REGION
# (ALL JOURNALS, ALL YEARS) TOTAL POOL OF EDITORS
##############################################
#Table of unique authors by country and categories. Editors may have worked as EIC and SE in some journals
UniqueAuthorsIncome <- unique(AnalysisData[ , c('FirstMiddleLast', 'INCOME_LEVEL', 'CATEGORY') ] )
UniqueAuthorsRegion <- unique(AnalysisData[ , c('FirstMiddleLast', 'REGION', 'CATEGORY') ] )

#Count geo.code based on authors 
CountryCategoryEditorsIncomeCount <- as.data.frame(UniqueAuthorsIncome %>% count(CATEGORY, INCOME_LEVEL = INCOME_LEVEL))  %>% spread(CATEGORY, n)
CountryCategoryEditorsRegionCount <- as.data.frame(UniqueAuthorsRegion %>% count(CATEGORY, REGION = REGION))  %>% spread(CATEGORY, n)

#Convert NA to 0
CountryCategoryEditorsIncomeCount[is.na.data.frame(CountryCategoryEditorsIncomeCount)] <- 0
CountryCategoryEditorsRegionCount[is.na.data.frame(CountryCategoryEditorsRegionCount)] <- 0

#Finding countries represented in AnalysisData
WDI_data['inAnalysisData'] <- WDI_data$iso3c %in% AnalysisData$geo.code

#Finding total for each category. Shouldn't it be the same as in regions?
TotalEIC <- sum(CountryCategoryEditorsIncomeCount$EIC)
TotalAE <- sum(CountryCategoryEditorsIncomeCount$AE)
TotalSE <- sum(CountryCategoryEditorsIncomeCount$SE)

#Converting to proportion
CountryCategoryEditorsIncomeCount$EIC <- CountryCategoryEditorsIncomeCount$EIC / TotalEIC
CountryCategoryEditorsIncomeCount$AE <- CountryCategoryEditorsIncomeCount$AE / TotalAE
CountryCategoryEditorsIncomeCount$SE <- CountryCategoryEditorsIncomeCount$SE / TotalSE

CountryCategoryEditorsRegionCount$EIC <- CountryCategoryEditorsRegionCount$EIC / TotalEIC
CountryCategoryEditorsRegionCount$AE <- CountryCategoryEditorsRegionCount$AE / TotalAE
CountryCategoryEditorsRegionCount$SE <- CountryCategoryEditorsRegionCount$SE / TotalSE

#rounding the percentages
CountryCategoryEditorsIncomeCount[,2:4] <- round(CountryCategoryEditorsIncomeCount[,2:4], 3) * 100
CountryCategoryEditorsRegionCount[,2:4] <- round(CountryCategoryEditorsRegionCount[,2:4], 3) * 100

#Finding EIC, AE, and SE editors for US and UK
UniqueCategoryCountryEditors <- unique( AnalysisData[ , c('FirstMiddleLast', 'CATEGORY', 'geo.code') ] )
UniqueCategoryCountryEditors <-UniqueCategoryCountryEditors  %>% count(geo.code, divmetric = CATEGORY) %>% spread(divmetric, n)
UniqueCategoryCountryEditors <- as.data.frame(UniqueCategoryCountryEditors )
UniqueCategoryCountryEditors$EIC <- round(UniqueCategoryCountryEditors$EIC / TotalEIC, 3) * 100
UniqueCategoryCountryEditors$AE <- round(UniqueCategoryCountryEditors$AE / TotalAE, 3) * 100
UniqueCategoryCountryEditors$SE <- round(UniqueCategoryCountryEditors$SE / TotalSE, 3) * 100
UniqueCategoryCountryEditors[is.na.data.frame(UniqueCategoryCountryEditors)] <- 0  #NA to 0

UniqueCategoryCountryEditors <- UniqueCategoryCountryEditors[order(UniqueCategoryCountryEditors$SE, decreasing = TRUE),]  #Sorting table


#printing tables
CountryCategoryEditorsIncomeCount
CountryCategoryEditorsRegionCount

#printing total number of editors
paste0 ('Total EIC: ', TotalEIC)
paste0 ('Total AE: ', TotalAE)
paste0 ('Total SE: ', TotalSE)

#printing total number of countries represented by INCOME and by REGION
summary(WDI_data$income[WDI_data$inAnalysisData])
summary(WDI_data$region[WDI_data$inAnalysisData])

#printing USA and GBR (ir any country) values to add to table
UniqueCategoryCountryEditors [1:5,]


##############################################
# PLOT NUMBER OF COUNTRIES REPRESENTED BY YEAR BY JOURNAL ALL CATEGORIES
# WITH LINE ADDING HIGH INCOME COUNTRIES (OECD AND NON-OECD)
##############################################

head(N_Countries)

p <- ggplot(data = N_Countries, aes(x = YEAR, y = n)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~ JOURNAL, ncol = 5) + 
  theme_minimal() +
  xlab('Number of Countries Represented in Editorial Board') + 
  scale_y_continuous(limits=c(0, 26),
                     breaks=c(0, 10, 20)) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '', '', '2000', '', '2010')) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  theme(panel.grid.minor = element_blank())


#+ theme(#panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   strip.background = element_blank())

tiff(file = "Plots/N_Countries_byJOURNAL.tiff",
     width = 2200,
     height = 1800,
     res = 300,
     compression = 'lzw')
p
dev.off()



##############################################
# PLOT % ALL EDITORS BY YEAR BY WDI INCOME_LEVEL by JOURNAL
# PLOT % ALL EDITORS BY YEAR BY REGIONS by JOURNAL
##############################################
#Table of unique authors by country and categories. Editors may have worked as EIC and SE in some journals
UniqueAuthorsIncome <- unique(AnalysisData[ , c('FirstMiddleLast', 'JOURNAL', 'INCOME_LEVEL', 'YEAR') ] )
UniqueAuthorsRegion <- unique(AnalysisData[ , c('FirstMiddleLast', 'JOURNAL', 'REGION', 'YEAR') ] )

#Count geo.code based on authors 
CountryCategoryEditorsIncomeCount <- as.data.frame(UniqueAuthorsIncome %>% count(JOURNAL, YEAR, INCOME_LEVEL = INCOME_LEVEL) %>% mutate(percent = n/sum(n)))
CountryCategoryEditorsRegionCount <- as.data.frame(UniqueAuthorsRegion %>% count(JOURNAL, YEAR, REGION = REGION) %>% mutate(percent = n/sum(n)))

p <- ggplot(data = CountryCategoryEditorsIncomeCount, aes(x = YEAR, y = percent, colour = INCOME_LEVEL)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~ JOURNAL, ncol = 5) + 
  theme_minimal() +
  scale_y_continuous(limits=c(0, 1),
                     breaks=c(0, 0.5, 1)) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '', '', '2000', '', '2010')) + 
  ylab(paste ("Proportion of Editorial Board")) + 
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00"),
                      name = '') +
  theme(legend.position="bottom")  +
  guides(col = guide_legend(nrow = 2)) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  theme(panel.grid.minor = element_blank())

tiff(file = "Plots/INCOME_byJOURNAL.tiff",
     width = 2200,
     height = 1800,
     res = 300,
     compression = 'lzw')
p
dev.off()


p <- ggplot(data = CountryCategoryEditorsRegionCount, aes(x = YEAR, y = percent, colour = REGION)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~ JOURNAL, ncol = 5) + 
  theme_minimal() +
  scale_y_continuous(limits=c(0, 1),
                     breaks=c(0, 0.5, 1)) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '', '', '2000', '', '2010')) + 
  ylab(paste ("Proportion of Editorial Board")) + 
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00"),
                      name = '') +
  theme(legend.position="bottom")  +
  guides(col = guide_legend(nrow = 2)) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  theme(panel.grid.minor = element_blank())

tiff(file = "Plots/REGION_byJOURNAL.tiff",
     width = 2200,
     height = 1800,
     res = 300,
     compression = 'lzw')
p
dev.off()

##############################################
# PLOT MEAN AND SD OF % EDITORIAL BOARDS BY YEAR BY WDI INCOME_LEVEL
# PLOT MEAN AND SD OF % EDITORIAL BOARDS BY YEAR BY REGIONS
##############################################

CountryCategoryEditorsIncomeCount 
CountryCategoryEditorsRegionCount 


p <- ggplot(data = CountryCategoryEditorsIncomeCount, aes(x = YEAR, y = percent, colour = INCOME_LEVEL)) + 
  stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", aes(fill=INCOME_LEVEL), alpha=0.3, colour = NA) +
  stat_summary(fun.y = mean, geom='line', size = 1.1) + 
  ggtitle('A') + 
  scale_y_continuous(limits=c(0, 1),
                     breaks=c(0, 0.25, 0.5, 0.75, 1),
                     labels = c('0', '', '0.5', '', '1.0')) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010)) + 
  ylab("Proportion of Editorial Board") + 
  xlab("") + 
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00"), name = '') +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00"), name = '') + 
  theme_minimal() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  guides(col = guide_legend(ncol = 1)) + 
  theme(panel.grid.minor = element_blank())

q <- ggplot(data = CountryCategoryEditorsRegionCount, aes(x = YEAR, y = percent, colour = REGION)) + 
  stat_summary(geom="ribbon", fun.ymin="min", fun.ymax="max", aes(fill=REGION), alpha=0.3, colour = NA) +
  stat_summary(fun.y = mean, geom='line', size = 1.1) + 
  ggtitle('B') + 
  theme_minimal() +
  scale_y_continuous(limits=c(0, 1),
                     breaks=c(0, 0.25, 0.5, 0.75, 1),
                     labels = c('0', '', '0.5', '', '1.0')) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010)) + 
  ylab("Proportion of Editorial Board") + 
  #ylab("Proportion of Editorial Board") + 
  scale_colour_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                               "#F0E442", "#0072B2", "#D55E00"), name = '') +
  scale_fill_manual(values=c("#000000", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442", "#0072B2", "#D55E00"), name = '') + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  guides(col = guide_legend(ncol = 1)) + 
  theme(panel.grid.minor = element_blank())

tiff(file = "Plots/Fig2.tiff",
     width = 2000,
     height = 2100,
     res = 300,
     compression = 'lzw')
multiplot(p, q, cols = 1)
dev.off()

##############################################
# PLOTS OF SHANNON INDEX BY YEAR BY JOURNAL
##############################################
#this for loops create graphs per journal and saves each one

p <- ggplot(data = ShannonDivTable, aes(x = YEAR, y = ShannonDiv)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~ JOURNAL, ncol = 5) + 
  theme_minimal() +
  ylab("Shannon Diversity Index") + 
  scale_y_continuous(limits=c(0, 3),
                       breaks=c(0, 1, 2, 3)) + 
  scale_x_continuous(limits=c(1985, 2013),
                     breaks=c(1985, 1990, 1995, 2000, 2005, 2010),
                     labels=c('1985', '', '', '2000', '', '2010')) + 
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) + 
  theme(panel.grid.minor = element_blank())


#+ theme(#panel.grid.major = element_blank(),
#   panel.grid.minor = element_blank(),
#   strip.background = element_blank())

tiff(file = "Plots/SHANNON_byJOURNAL.tiff",
     width = 2200,
     height = 1800,
     res = 300,
     compression = 'lzw')
p
dev.off()


