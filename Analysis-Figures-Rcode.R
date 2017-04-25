
# R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF THE GEOGRAPHY OF EDITORIAL BOARDS
# This is a clone of the code in the Github Repo for analaysis of Gender and Editorial Boards (https://github.com/embruna/Editorial-Board-Gender).
# Package and R versions:

# stringdist = 0.9.4.4
# RecordLinkage = 0.4.10
# vegan = 2.4.2
# tidyverse = 1.0
# MuMIn = 1.15.6
# nlme = 3.1.128
# WDI = 2.4
# R version = 3.3.1

library(tidyverse)
library(RecordLinkage)
library(stringdist)
library(vegan)
library(WDI)
library(nlme)
library(MuMIn)

  # Clear out everything from the environment 
  rm(list=ls())

  ##############################################################
  ##############################################################
  #
  # SET UP THE TEMPORAL FRAME OF YOUR STUDY
  # This avoids mistakes, esnures consistent analyses and figures
  #
  FirstYear=1985
  LastYear=2014
  #
  ###############################################################
  ##############################################################

  
  
  
  
  ##############################################################
  ##############################################################
  #
  # DATA UPLOAD & ORGANIZATION
  # load the individual CSV files and save them as dataframes
  #
  ##############################################################
  ##############################################################

  ##############################################################
  # DATA CLEANUP & ORGANIZATION: Data from Cho et al. 2014
  ##############################################################
  
  # Import data on Editorial Boards from Cho et al 2014 PeerJ
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
  
  
  #Bind the data from Cho
  ChoData<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE) 

  source("Cho.Fix.R")
  ChoData_clean<-Cho.Fix(ChoData)
  ChoData_clean
  write.csv(ChoData_clean, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ChoData_clean.csv", row.names = T) #export it as a csv file
  
  #Don't Need the original files or Messy ChoData cluttering up the Env't so lets delete
  rm(ChoData, BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE)
  
  ############################################################
  # DATA CLEANUP & ORGANIZATION: Data from 2015 UF CLass  
  ############################################################
  
  
  # Import Data on Editorial Boards collected by 2015 UF Scientific Publishing Seminar 
  AGRON2<-read.csv("./Data2015/AGRON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  AMNAT<-read.csv("./Data2015/AMNAT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ARES2<-read.csv("./Data2015/ARES2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BIOCON2<-read.csv("./Data2015/BIOCON2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BIOG<-read.csv("./Data2015/BIOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  BITR2<-read.csv("./Data2015/BITR2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ECOG<-read.csv("./Data2015/ECOG.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  ECOLOGY2<-read.csv("./Data2015/Ecology2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  EVOL<-read.csv("./Data2015/EVOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #Still need to ID what an Editor vs EIC does when they transitoned to EIC
  FEM<-read.csv("./Data2015/FEM.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  FUNECOL<-read.csv("./Data2015/FUNECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JANE<-read.csv("./Data2015/JANE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JAPE<-read.csv("./Data2015/JAPE.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JTE2<-read.csv("./Data2015/JTE2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  NAJFM2<-read.csv("./Data2015/NAJFM2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  NEWPHYT<-read.csv("./Data2015/NEWPHYT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
  OECOL<-read.csv("./Data2015/OECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  OIKOS<-read.csv("./Data2015/OIKOS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #5 are missing country
  LECO<-read.csv("./Data2015/LECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  PLANTECOL<-read.csv("./Data2015/PLANTECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  JZOOL<-read.csv("./Data2015/JZOOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
  MIX<-read.csv("./Data2015/MAU_EB_MIX.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #Agronomy 1985 1986, JTE 1986, JZOOL 1985, LECO 1987 2014, PLANTECO 2014  
  MIX$SUFFIX<-as.logical(MIX$SUFFIX)
  MIX$GENDER<-as.factor(MIX$GENDER)
  
  
  # MISSING TOO MUCH DATA TO INCLUDE IN THIS STUDY 
  GCB<-read.csv("./Data2015/GCB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  # ONLY HAS 1995-2007. 2007-2008 in dropbox. Wiley Journal
  
  MARECOL<-read.csv("./Data2015/MARECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  
  MEPS<-read.csv("./Data2015/MEPS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  # ONLY HAS 1989-1997. Have in folder 2010, 2011-2013, 2014-2015. what looks like 88,87,1985
  

  
  #Bind the data from 2015 workshop to be used in this paper
  ClassData<-rbind(AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, ECOLOGY2, EVOL, FEM, FUNECOL, 
                   JANE, JAPE, JTE2, JZOOL, LECO, NAJFM2, NEWPHYT, OECOL, OIKOS, PLANTECOL, MIX) 
  source("Class.Fix.R")
  ClassData_clean<-Class.Fix(ClassData)
  write.csv(ClassData_clean, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData_clean.csv", row.names = T) #export it as a csv file
  
  # Don't Need the original files or Messy ClassData cluttering up the Env't so lets delete
  rm(ClassData,GCB, MEPS,AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, ECOLOGY2, EVOL, FEM, FUNECOL, 
     JANE, JAPE, JTE2, JZOOL, LECO, MARECOL, NAJFM2, NEWPHYT, OECOL, OIKOS, PLANTECOL, MIX)
  
  # THIS REMOVEA A FEW WITH BLANKS IN THE NAMES
  ClassData_clean <-filter(ClassData_clean, ClassData_clean$FIRST_NAME!="" & ClassData_clean$LAST_NAME!="")
  
  #NOTE: In this paper all "Special Editors" will be included (book review, data, stats, etc.)
  # AGRONOMY<-As Per https://dl.sciencesocieties.org/files/publications/editor-handbook/editors-handbook.pdf
    #Technical Editors = AE, Associate Editors<-SE
  
  
  


  ##############################################################
  #
  # CHOOSE DATASETS TO ANALYSE AND BIND THEM TOGETHER 
  #
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
  
  
  
  
  
  # Final 2x and Error Correction
  # 7) One name missing in Oecologia due to blurry pic

  
  
amnat.summary<-ALLDATA %>% filter(JOURNAL=="AMNAT") %>% distinct(YEAR,VOLUME,ISSUE) %>% arrange(YEAR)
write.csv(amnat.summary, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/amnat.summary.csv", row.names = F) #export it as a csv file
  
oikos.summary<-ALLDATA %>% filter(JOURNAL=="OIKOS") %>% distinct(TITLE,CATEGORY)
write.csv(oikos.summary, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/oikos.summary.csv", row.names = F) #export it as a csv file
  
evolution.summary<-ALLDATA %>% filter(JOURNAL=="EVOL") %>% distinct(TITLE,CATEGORY)
write.csv(evolution.summary, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/evolution.summary.csv", row.names = F) #export it as a csv file


amnat2.summary<-ALLDATA %>% filter(JOURNAL=="AMNAT") %>% distinct(TITLE,CATEGORY)
write.csv(amnat2.summary, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/amnat2.summary.csv", row.names = F) #export it as a csv file



VolsPerYr.summary<-ALLDATA %>%  distinct(JOURNAL,YEAR,VOLUME) %>% arrange(JOURNAL, YEAR, VOLUME)
write.csv(VolsPerYr.summary, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/VolsPerYr.summary.csv", row.names = F) #export it as a csv file

AGRONOMY.titles<-ALLDATA %>% filter(JOURNAL=="AGRONOMY") %>% distinct(TITLE,CATEGORY)

AGRONOMY.2x<-ALLDATA %>%  filter(JOURNAL=="AGRONOMY") %>% filter(YEAR==2014) %>% arrange(FIRST_NAME, LAST_NAME)
write.csv(AGRONOMY.2x, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/AGRONOMY.2x.csv", row.names = F) #export it as a csv file



  #############################################################
  # Function to determine the years missing in your dataset
  # yrs.missing(dataset,first year of interest,last year of interest)
  #############################################################
  source("yrs.missing.R")
  yrs.missing<-yrs.missing(ALLDATA,FirstYear,LastYear)
  write.csv(yrs.missing, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData_missingYrs.csv", row.names = T) #export it as a csv file
  
  
  #############################################################
  # DELETE PRODUCTION STAFF
  #############################################################
  
  AnalysisCategories <-c("AE","EIC","SE","SPECIAL")
  ALLDATA <- ALLDATA %>% filter(CATEGORY %in% AnalysisCategories)
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
  # NAME DISAMBIGUATION & ASSIGNING UNIQUE ID NUMBER TO EACH EDITOR 
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
 FixList<-select(FixList,FirstMiddleLast)
 FixList$editor_id<-seq(max(ALLDATA$editor_id+1), length.out=nrow(FixList), by = 1)
 FixList$FirstMiddleLast<-as.character(FixList$FirstMiddleLast)
 
# join the two together, if there is a real number (i.e., not NA) in the 2nd editor id column just replace it)
# HT: http://stackoverflow.com/questions/36433757/conditionally-replace-values-in-data-frame-from-separate-data-frame-in-r
# This saved me from the gnarly nested loop below
ALLDATA <- full_join(ALLDATA, FixList, by = "FirstMiddleLast", all = T) %>% mutate(editor_id = ifelse(is.na(editor_id.y), editor_id.x, editor_id.y)) %>% select(-editor_id.x,-editor_id.y)
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
# we need to change yugoslavia to what?
# we need to add french guiana wold bank classficiation


######################################################
######################################################
#
# ADDING COUNTRY INCOME LEVEL AND REGION FOR EACH EDITOR
#
######################################################
######################################################
source("AddIncomeRegion.R")
ALLDATA<-AddIncomeRegion(ALLDATA)
# ALLDATA





######################################################
######################################################

#Delete the AMNATS with two volumes in a single year
ALLDATA$VOLUME<-as.numeric(ALLDATA$VOLUME)
Vols2Go<-c('126','128','130','132','134','136','138','140','142','144','146','148')

AmNat2Go<-ALLDATA %>% filter((JOURNAL=="AMNAT" & VOLUME %in% Vols2Go))
ALLDATA<-setdiff(ALLDATA, AmNat2Go)
rm(AmNat2Go,Vols2Go)


# Delete the 2014 agronomy from cho and replace with class

Agronomy2Go<-ALLDATA %>% filter((JOURNAL=="AGRONOMY" & DATASET=="Cho" & YEAR==2014))
ALLDATA<-setdiff(ALLDATA, Agronomy2Go)
ALLDATA$DATASET[ALLDATA$JOURNAL == "AGRONOMY" & ALLDATA$YEAR == 2014 ] <- "Cho"
rm(Agronomy2Go)



# EXPORT FDOR DRYAD

Drayd.Cho.v2<-ALLDATA %>% filter(DATASET=="Cho") %>% select(-FirstLast,-FirstMiddleLast,-FirstInitialLast,-INSTITUTION,-editor_id,-geo.code,-INCOME_LEVEL,-REGION)
Drayd.Espin.v1<-ALLDATA %>% filter(DATASET=="Class") %>% select(-FirstLast,-FirstMiddleLast,-FirstInitialLast,-INSTITUTION,-editor_id,-GENDER)


######################################################
######################################################

# ANALYSES

# FIRST: Select temporal coverage for analyses
AnalysisData<-ALLDATA[ALLDATA$YEAR>=FirstYear & ALLDATA$YEAR<=LastYear,]

# AND subsett data to only EIC, AE, SE, and Special classifications
AnalysisData <- AnalysisData[AnalysisData$CATEGORY %in% c('EIC', 'AE', 'SE', 'SPECIAL'),]

# AND delete unecessary columns 
AnalysisData<-AnalysisData %>% 
  select(-INSTITUTION,-NOTES,-GENDER, -VOLUME, -ISSUE, -TITLE, -INSTITUTION)

# Convert editor ID to a factor
AnalysisData$editor_id<-as.factor(AnalysisData$editor_id)


# Convert YEAR to a NUmeric
AnalysisData$YEAR<-as.numeric(AnalysisData$YEAR)


# Convert Journal Codes to Journal Names
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="BITR"] <- "Biotropica"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="PLANTECOL"] <- "Plant Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="AGRONOMY"] <- "Agronomy Journal"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="AJB"] <- "American J. Botany"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="CONBIO"] <- "Conservation Biology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="ECOLOGY"] <- "Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="BIOCON"] <- "Biological Conservation"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JECOL"] <- "J. of Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JTE"] <- "J. Tropical Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="AMNAT"] <- "American Naturalist"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JBIOG"] <- "J. Biogeography"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="ECOGRAPHY"] <- "Ecography"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="EVOL"] <- "Evolution"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="FEM"] <- "Forest Ecology & Managment"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="FUNECOL"] <- "Functional Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="LECO"] <- "Landscape Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JZOOL"] <- "J. Zoology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JAPE"] <- "J. Applied Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="JANE"] <- "J. Animal Ecology"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="NEWPHYT"] <- "New Phytologist"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="OECOL"] <- "Oecologia"
levels(AnalysisData$JOURNAL)[levels(AnalysisData$JOURNAL)=="OIKOS"] <- "Oikos"
#############################################################


##############################################################
# 1: Total Number of (Unique) Editors in the Community
eds<-AnalysisData %>% summarise(n_distinct(editor_id))
eds
##############################################################

##############################################################
# 2: Total number of countries represented by Editors 
countries<-AnalysisData %>% summarise(n_distinct(geo.code))
countries
##############################################################

##############################################################
# 2: Cumulative Number of countries per year (Cumulative Geographic Richness)
# Use Rarefaction curves generated by vegan then convett back to tibble
editorAcum<-AnalysisData %>% group_by(YEAR, geo.code) %>% summarize(yr_tot = n_distinct(geo.code))
editorAcum<-spread(editorAcum, geo.code,yr_tot) 
editorAcum[is.na(editorAcum)] <- 0
editorAcum<-ungroup(editorAcum)
editorAcum<-select(editorAcum,-YEAR)
editorAcum<-specaccum(editorAcum, "collector")
editorAcum<-as_tibble(editorAcum$richness)
names(editorAcum)[1] <- "CumulativeRichness"
editorAcum$YEAR<-seq(1985,2014,1)
editorAcum
##############################################################

##############################################################
# 3: Number of countries represented in each year (Annual Geographic Richness)
GEOperYR<-AnalysisData %>% group_by(YEAR) %>% summarize(AnnualRichness = n_distinct(geo.code)) 
GEOperYR
##############################################################

##############################################################
# 4: Number and Pcnt of Editors from Each Country (all journals pooled, all years pooled (Used for Fig 2A)
Editor.Geo<-AnalysisData %>%  group_by(geo.code) %>% 
  summarize(N_editors = n_distinct(editor_id)) %>% 
  mutate(Pcnt_editors= (N_editors/sum(N_editors)*100)) %>% 
  arrange(desc(Pcnt_editors))
Editor.Geo
##############################################################

##############################################################
# 4: Geographic Diversity (all journals pooled) 
# 5: Geographic Evenness (all journals pooled) 

DivDataPooled<-AnalysisData %>% group_by(YEAR, geo.code) %>% summarize(Total = n_distinct(editor_id)) 
# DivDataPooled<-as.data.frame(EdsPerCountryPerJrnlPerYr.LONG)
DivDataPooled<-DivDataPooled %>% group_by(YEAR, geo.code) %>% summarise(Total_Eds=sum(Total))
DivDataPooled<-spread(DivDataPooled, geo.code, Total_Eds) 
DivDataPooled[is.na(DivDataPooled)] <- 0
DivDataPooled<-ungroup(DivDataPooled)
# 4: Geo Diverisity using Simpson's Inverse
IsimpDivTable <- diversity((DivDataPooled %>% select(-YEAR)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
IsimpDivTable <- data.frame(IsimpDivTable)
IsimpDivTable$YEAR <-DivDataPooled$YEAR #Add year as a column
IsimpDivTable<-rename(IsimpDivTable, InvSimpson=IsimpDivTable) #rename the columns
IsimpDivTable <- IsimpDivTable[c("YEAR","InvSimpson")] #reorder the columns
IsimpDivTable<-as_tibble(IsimpDivTable)

# 4: Geographic Evenness (all journals pooled) 
IsimpDivTable<-full_join(GEOperYR,IsimpDivTable, by="YEAR")
IsimpDivTable<-mutate(IsimpDivTable, Geo.Evenness = InvSimpson/AnnualRichness)

IsimpDivTable
##############################################################

######################################################
# 5: Total Number of Editors (all jrnls pooled)  vs. Year
######################################################
EdsPerYr<-AnalysisData %>% group_by(YEAR) %>% summarize(TotalEditors = n_distinct(editor_id))
EdsPerYr
##############################################################

######################################################
# 6: Number / Percentage of Editors from Different Regions, all journals pooled (Data for Fig. 2b)
RegionPlot<-AnalysisData %>% select(YEAR,editor_id,REGION,CATEGORY) %>% group_by(editor_id) 
RegionPlot<-distinct(RegionPlot, editor_id,YEAR, .keep_all = TRUE)
RegionPlot<-RegionPlot %>% group_by(YEAR,REGION) %>% count(YEAR,REGION)
RegionPlot<-RegionPlot %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
RegionPlot
#RegionPlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) checks that add up to 100%
##############################################################

######################################################
# 7: Number / Percentage of Editors from Different Income Levels, all journals pooled (Data for Fig. 2c)
IncomePlot<-AnalysisData %>% select(YEAR,editor_id,INCOME_LEVEL,CATEGORY) %>% group_by(editor_id) 
IncomePlot<-distinct(IncomePlot, editor_id,YEAR, .keep_all = TRUE)
IncomePlot<-IncomePlot %>% group_by(YEAR,INCOME_LEVEL) %>% count(YEAR,INCOME_LEVEL)
IncomePlot<-IncomePlot %>% group_by(YEAR) %>% mutate(yr_tot=sum(n)) %>% mutate(Percent=n/yr_tot*100) 
IncomePlot
#IncomePlot %>%  group_by(YEAR) %>% mutate(sum=sum(Percent)) checks that add up to 100%
##############################################################

######################################################
######################################################
#
# FIGURES AND TABLES
#
######################################################
######################################################

######################################################
# TABLE 1
# Total by Income
EdsByIncome<-AnalysisData %>% group_by(INCOME_LEVEL) %>% summarize(Total = n_distinct(editor_id))
# Total by Region
EdsByRegion<-AnalysisData %>% group_by(REGION) %>% summarize(Total = n_distinct(editor_id))

# 8: Number / Percentage of Editor Types from Different Regions, all years pooled (Data for Table 1)
EdCat.region<-AnalysisData %>% group_by(CATEGORY, REGION)  %>%  summarize(N=n_distinct(editor_id))  %>% mutate(Pcnt=N/sum(N)*100) 
EdCat.region$Pcnt<-round(EdCat.region$Pcnt, digits=2)
EdCat.region<-EdCat.region %>% select(-N) %>% spread(CATEGORY, Pcnt)
EdCat.region[is.na(EdCat.region)] <- 0
EdCat.region
# 9: Number / Percentage of Editor Types from Different Income Levels, all years pooled (Data for Table 1)
EdCat.income<-AnalysisData %>% group_by(CATEGORY, INCOME_LEVEL)  %>%  summarize(N=n_distinct(editor_id))  %>% mutate(Pcnt=N/sum(N)*100) 
EdCat.income$Pcnt<-round(EdCat.income$Pcnt, digits=2)
EdCat.income<-EdCat.income %>% select(-N) %>% spread(CATEGORY, Pcnt)
EdCat.income[is.na(EdCat.income)] <- 0

EdsByIncome
EdsByRegion
EdCat.income
EdCat.region
######################################################

######################################################
# Fig. 1A: Cumulative Geo Richness
######################################################
# PUT THE NECESSARY DATA IN ONE DATAFRAME 
jointGEOperYR<-GEOperYR
jointedAccDF<-editorAcum

jointGEOperYR<-rename(jointGEOperYR,Countries=AnnualRichness)
jointRichness<-full_join(jointGEOperYR, jointedAccDF, by = "YEAR")
jointRichness<-gather(jointRichness, "Richness","N", 2:3)
jointRichness[jointRichness=="Countries"]<-"Annual"
jointRichness[jointRichness=="CumulativeRichness"]<-"Cumulative"
rm(jointGEOperYR,jointedAccDF)

#plot cumulative and annual richness same plot
jointRichnessPlot<-ggplot(jointRichness, aes(x=YEAR, y=N, group = Richness, colour = Richness)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue", "red"))+
  geom_text(data = jointRichness[jointRichness$YEAR=="2012" & jointRichness$Richness=="Annual",], aes(label = Richness), hjust = 1, vjust = -1, size=5) +
  geom_text(data = jointRichness[jointRichness$YEAR=="2012" & jointRichness$Richness=="Cumulative",], aes(label = Richness), hjust = 1, vjust = -1, size=5) +
  #ylab("Number of Countries") +
  xlab("Year")+
  ggtitle('(A) Geographic Richness')+
  geom_point(color="black", shape=1)+
  # scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_y_continuous(limits = c(25, 75))+
  scale_x_continuous(breaks=seq(1985, 2015, 5))

jointRichnessPlot<-jointRichnessPlot+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_blank(),
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        # legend.position = c(0.9,0.8),
        legend.position = ("none"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
jointRichnessPlot


######################################################
# Fig 1B: Total Number of Editors (all jrnls pooled)  vs. Year
######################################################
plotTOTALEDSvYear<-ggplot(EdsPerYr, aes(x=YEAR, y=TotalEditors)) +
  #ylab("Number of Editors") +
  xlab("Year")+
  geom_line(size=1, color="blue")+
  ggtitle('(B) Number of Editors') + 
  geom_point(color="black", shape=1)+
  scale_y_continuous(breaks=seq(0, 1500, 150))+
  scale_x_continuous(breaks=seq(1985, 2015, 5))
plotTOTALEDSvYear<-plotTOTALEDSvYear+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_blank(),
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
plotTOTALEDSvYear

##############################################################
# Plot 1C: COMMUNITY (POOLED JOURNALS) LEVEL DIVERSITY
##############################################################

plotPOOLEDsimpdiv<-ggplot(IsimpDivTable, aes(x=YEAR, y=InvSimpson)) +
  geom_line(size=1, color="blue") + # Use hollow circles
  #ylab("Geographic Diversity") +
  xlab("Year")+
  ggtitle('(C) Geographic Diversity')+
  geom_point(color="black", shape=1)+
  # scale_y_continuous(breaks=seq(2, 5.5, 0.5))+
  scale_y_continuous(limits=c(1,max(IsimpDivTable$AnnualRichness)))+
  scale_x_continuous(breaks=seq(1985, 2015, 5))
plotPOOLEDsimpdiv<-plotPOOLEDsimpdiv+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_blank(),
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
plotPOOLEDsimpdiv

##############################################################
# Plot 1D: COMMUNITY (POOLED JOURNALS) LEVEL EVENNESS
##############################################################
plotPOOLEDevenness<-ggplot(IsimpDivTable, aes(x=YEAR, y=Geo.Evenness)) +
  geom_line(size=1, color="blue") + # Use hollow circles
  #ylab("Geographic Evenness") +
  xlab("Year")+
  ggtitle('(D) Geographic Evenness')+
  geom_point(color="black", shape=1)+
  # scale_y_continuous(breaks=seq(0, 1, 0.1))+
  scale_y_continuous(limits = c(0, 1))+
  scale_x_continuous(breaks=seq(1985, 2015, 5))
plotPOOLEDevenness<-plotPOOLEDevenness+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        #axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_blank(),
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
plotPOOLEDevenness
######################################################
# Binding these up to make Fig. 1
######################################################
source("multiplot.R")
# if you jiust want to take a quick look at the results...
Fig1<-multiplot(jointRichnessPlot, plotPOOLEDsimpdiv, plotTOTALEDSvYear, plotPOOLEDevenness, cols=2)
# to save the figure in format for submission
# for an explanation of why you need to do multiplot INSIDe of ggsave see: http://stackoverflow.com/questions/11721401/r-save-multiplot-to-file
ggsave("Fig1.eps", plot = multiplot(jointRichnessPlot, plotPOOLEDsimpdiv, plotTOTALEDSvYear, plotPOOLEDevenness, cols=2), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)


######################################################


######################################################
# Fig 2A: bar chart of countries with the most unique editors  
######################################################
cutoff = 9 # This is how many countries you want on the chart, all the rest will be in "OTHER"
editor.Geo<-arrange(Editor.Geo, desc(Pcnt_editors)) %>% select(geo.code,N_editors,Pcnt_editors)
most.common.editors<-slice(editor.Geo, 1:cutoff)
least.common.editors<-slice(editor.Geo, (cutoff+1):nrow(editor.Geo)) 
least.common.editors$geo.code<-"OTHER"
least.common.editors<-least.common.editors %>% 
  mutate(sum(N_editors)) %>%
  mutate(sum(Pcnt_editors)) %>% 
  select(-N_editors) %>% 
  select(-Pcnt_editors) %>% 
  rename(N_editors = `sum(N_editors)`) %>% 
  rename(Pcnt_editors = `sum(Pcnt_editors)`) %>% 
  slice(1:1)
most.common.editors<-bind_rows(most.common.editors, least.common.editors)
most.common.editors$geo.code<-as.factor(most.common.editors$geo.code)
most.common.editors
#Bar  chart editors
# #If you needed to reorder in descending order you would do this.
# arrange(most.common.editors) %>%  ggplot(aes(x=reorder(geo.code,-N_editors), y=Pcnt_editors)) +
#   geom_bar(colour="black", stat="identity")

# This is needed to put them in order in the plot with OTHER at the end of the graph
order<-seq(1:nrow(most.common.editors))
most.common.editors$geo.code <- factor(most.common.editors$geo.code,most.common.editors$geo.code[levels = order])
# levels(most.common.editors$geo.code)
rm(order,editor.Geo,least.common.editors)

CountriesED<-arrange(most.common.editors) %>%  ggplot(aes(x=geo.code, y=Pcnt_editors)) +
  geom_bar(colour="black", stat="identity")+
  ylab("Percent") +
  xlab("Country")+
  ggtitle('(A) Editor Home Country')+
  scale_y_continuous(breaks=seq(0, 70, 5))
CountriesED<-CountriesED+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        plot.margin=unit(c(1,1,2,1),"lines"),
        #aspect.ratio=1,
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
CountriesED

######################################################
# Fig 2B: Prop of the EDITOR POOL in EACH YEAR by REGION
# Note: thisis not "the average proportion of each editorial board". 
# THis takes the list of people serving as editors in a year, makes sure
# that each person is listed only once (i.e., if an editor is on 2 boards in one
# year they are counted only once), and then calculates the proportion 
# of that pool from each Region
######################################################
RegionFig<-ggplot(data=RegionPlot, aes(x=YEAR, y=Percent, group=REGION, colour=REGION)) +
  geom_line(size=1)+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(B) Editor Region')+
  scale_y_continuous(limit = c(0, 100))+
  scale_x_continuous(breaks=seq(1984, 2014, 5))
RegionFig<-RegionFig+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10), 
        # legend.position = c(0.9,0.8),
        legend.position = "right",
        plot.margin=unit(c(1,1,2,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
RegionFig


######################################################
# Fig 2C: Prop of the EDITORS in EACH YEAR by COUNTRY INCOME
# Note: thisis not "the average proportion of each editorial board". 
# THis takes the list of people serving as editors in a year, makes sure
# that each person is listed only once (i.e., if an editor is on 2 boards in one
# year they are counted only once), and then calculates 
# the proportion of that pool from each country income category 
######################################################

IncomeFig<-ggplot(data=IncomePlot, aes(x=YEAR, y=Percent, group=INCOME_LEVEL, colour=INCOME_LEVEL)) +
  geom_line(size=1)+
  ylab("Percent") +
  xlab("Year")+
  ggtitle('(C) Editor National Income Category')+
  scale_y_continuous(limit = c(0, 100))+
  # scale_y_continuous(breaks=seq(0, 100, 10))+
  scale_x_continuous(breaks=seq(1984, 2014, 5))
IncomeFig<-IncomeFig+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10), 
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10), 
        # legend.position = c(0.9,0.8), 
        legend.position = "right",
        plot.margin=unit(c(1,1,2,1),"lines"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
IncomeFig

######################################################
# BINDING THESE UP TO MAKE FIGURE 2
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")
Fig2<-multiplot(CountriesED, RegionFig, IncomeFig, cols=1)
ggsave("Fig2.eps", plot = multiplot(CountriesED, RegionFig, IncomeFig, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

######################################################

######################################################
######################################################
# SUPPLEMENT
######################################################
######################################################

######################################################
#Table S1
######################################################

EdsFirstYr<-AnalysisData %>% 
  filter(YEAR == FirstYear) %>%  
  rbind(AnalysisData %>% filter(YEAR == 1987) %>% filter(JOURNAL%in% c("Conservation Biology" , "Functional Ecology", "Landscape Ecology"))) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(EditorsFirstYr = n_distinct(editor_id)) %>% 
  arrange(JOURNAL)

CountriesFirstYr<-AnalysisData %>% 
  filter(YEAR == FirstYear) %>%  
  rbind(AnalysisData %>% filter(YEAR == 1987) %>% filter(JOURNAL%in% c("Conservation Biology" , "Functional Ecology", "Landscape Ecology"))) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(CountriesFirstYr = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

EdsLastYr<-AnalysisData %>% 
  filter(YEAR == LastYear) %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(EditorsLastYr = n_distinct(editor_id))%>% 
  arrange(JOURNAL)

CountriesLastYr<-AnalysisData %>% 
  filter(YEAR == LastYear) %>%  
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(CountriesLastYr = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

EdsTotal<-AnalysisData %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(TotalEditors = n_distinct(editor_id)) %>% 
  arrange(JOURNAL)

CountriesTotal<-AnalysisData %>% 
  group_by(JOURNAL) %>%   #collect the data into groups by country
  summarize(TotalCountries = n_distinct(geo.code)) %>% 
  arrange(JOURNAL)

TABLE1<-full_join(EdsFirstYr,CountriesFirstYr, by = "JOURNAL")
TABLE1<-full_join(TABLE1,EdsLastYr, by = "JOURNAL")  
TABLE1<-full_join(TABLE1,CountriesLastYr, by = "JOURNAL") 
TABLE1<-full_join(TABLE1,EdsTotal, by = "JOURNAL") 
TABLE1<-full_join(TABLE1,CountriesTotal, by = "JOURNAL")
TABLE1<-mutate(TABLE1, CEratio=TotalEditors/TotalCountries)
TABLE1$Pcnt<-round(TABLE1$CEratio, digits=2)
TABLE1
rm(EdsFirstYr,CountriesFirstYr,EdsLastYr,CountriesLastYr,EdsTotal,CountriesTotal)
write.csv(TABLE1, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/TableS1.csv", row.names = F) #export it as a csv file


######################################################
#chi-sq test: are there differences in frequency by region and income level?

Chi.region<-AnalysisData %>% group_by(REGION)  %>%  summarize(Editors=n_distinct(editor_id)) %>% mutate(Pcnt=Editors/sum(Editors)*100)  
chisq.test(Chi.region$Editors)

Chi.income<-AnalysisData %>% group_by(INCOME_LEVEL)  %>%  summarize(Editors=n_distinct(editor_id))%>% mutate(Pcnt=Editors/sum(Editors)*100)
chisq.test(Chi.income$Editors)
######################################################



######################################################
######################################################
# GLS with Temp Autocorrleation
######################################################
######################################################
#Bind up the data for the analyses
GLS.data<-full_join(IsimpDivTable, EdsPerYr,by="YEAR")
GLS.data<-rename(GLS.data, Countries=AnnualRichness, Editors=TotalEditors) #Shorter
# Preliminary: is there evidence for Autocorrelation 
# GR: Yes
acf(GLS.data$Countries,lag.max=10) 
plot(acf(GLS.data$Countries,type="p")) #partial autocorrelation

# GD YES
acf(GLS.data$InvSimpson,lag.max=10) 
plot(acf(GLS.data$InvSimpson,type="p"))

# GE YES
acf(GLS.data$Geo.Evenness,lag.max=10) 
plot(acf(GLS.data$Geo.Evenness,type="p"))

# RESPONSE<-"Countries"
# RESPONSE<-"InvSimpson"
# RESPONSE<-"Geo.Evenness"


# library(nlme)
# https://stats.stackexchange.com/questions/13859/finding-overall-p-value-for-gls-model
# REML to test the utility of corARMA term
# TESTING THE EFFECT OF THE CORRELATION STRUCTURE
mAC1.1 <- gls(Geo.Evenness ~ 1, data = GLS.data,  na.action = na.omit) 
mAC1.2 <- gls(Geo.Evenness ~ 1, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC2.1 <- gls(Geo.Evenness ~ Editors, data = GLS.data,  na.action = na.omit) 
mAC2.2 <- gls(Geo.Evenness ~ Editors, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC3.1 <- gls(Geo.Evenness ~ YEAR, data = GLS.data,  na.action = na.omit)
mAC3.2 <- gls(Geo.Evenness ~ YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC4.1 <- gls(Geo.Evenness ~ Editors+YEAR, data = GLS.data, na.action = na.omit)
mAC4.2 <- gls(Geo.Evenness ~ Editors+YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
mAC5.1 <- gls(Geo.Evenness ~ Editors*YEAR, data = GLS.data, na.action = na.omit)
mAC5.2 <- gls(Geo.Evenness ~ Editors*YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit)
# 
summary(mAC1.2)
model.sel(mAC1.1,mAC1.2,mAC2.1,mAC2.2,mAC3.1,mAC3.2,mAC4.1,mAC4.2,mAC5.1,mAC5.2)


# ML to test main effects
m1.MAIN <- gls(Countries ~ 1, data = GLS.data, correlation = corARMA(p = 1),na.action = na.omit,method = "ML")
m2.MAIN <- gls(Countries ~ YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")
m3.MAIN <- gls(Countries ~ Editors,  data = GLS.data,correlation = corARMA(p = 1),na.action = na.omit,method = "ML")
m4.MAIN <- gls(Countries ~ YEAR + Editors, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")
m5.MAIN <- gls(Countries ~ Editors*YEAR, data = GLS.data, correlation = corARMA(p = 1), na.action = na.omit,method = "ML")

summary(m1.MAIN)
summary(m2.MAIN)
summary(m3.MAIN)
summary(m4.MAIN)
summary(m5.MAIN)
# https://stats.stackexchange.com/questions/13859/finding-overall-p-value-for-gls-model
anova(m1.MAIN,m2.MAIN)
anova(m1.MAIN, m3.MAIN)
anova(m2.MAIN,m4.MAIN)
anova(m3.MAIN,m4.MAIN)
anova(m3.MAIN,m5.MAIN)
anova(m4.MAIN,m5.MAIN)
# library(MuMIn)
model.sel(m1.MAIN,m2.MAIN,m3.MAIN,m4.MAIN,m5.MAIN)

######################################################

######################################################
# Supplement Fig S1: Countries in a Year vs. No of Editors in a Year (all journals pooled) 
######################################################
TotalEdsVGeo<-full_join(EdsPerYr,GEOperYR, by="YEAR")
plotTOTALedsVgeo<-ggplot(TotalEdsVGeo, aes(x=TotalEditors, y=AnnualRichness)) +
  ggtitle('(D) Geographic Richness')+
  # ylab("Geographic Richness") +
  xlab("Total No. of Editors")+
  # geom_line(size=1, color="blue")+
  geom_point(color="black", shape=1,size=1)+
  geom_smooth(method='lm', se=FALSE)+
  scale_y_continuous(breaks=seq(30, 60, 5))
plotTOTALedsVgeo<-plotTOTALedsVgeo+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
plotTOTALedsVgeo
######################################################

######################################################
# Supplement Fig S2: Cumlative Editors vs Cumulative Authors
######################################################

# # #WHY ONLY RADINGIN 30 LINES?!?!??!?!
# # PROBLEM IS in the nrows statement trying to delete the last two lines. doesn't know which is the thing to measure length of'
# #BASIC
# FileNames <- list.files("./SupplementaryData/AuthorDiv", full.names = T)
# AuthorCountries = lapply(FileNames, function(x) {
#     dat = read.table(x, sep = "\t", header=FALSE,skip = 1,nrows=--------)
#   # Add column names
#   names(dat) = c("COUNTRY", "Articles", "Percent")
#   # Add a column with the year
#   dat$YEAR = substr(x,33,36)
#   return(dat)
# })
# #This is returned as a list, when binding below converts to dataframe with country as chr
# AuthorCountries<-bind_rows(AuthorCountries)
# AuthorCountries$YEAR<-as.numeric(AuthorCountries$YEAR)
# #Delete the WOS percentage, add a column in whihc you generate it yourself
# AuthorCountries<-AuthorCountries %>% select(-Percent) %>% group_by(YEAR) %>% mutate(Pcnt_Pubs= (Articles/sum(Articles)*100)) %>% rename(N_Articles = Articles)
# AuthorCountries
# AuthorCountries$YEAR<-as.numeric(AuthorCountries$YEAR)
# 


# HERE IS THE TIDYVERSE WAY
FileNames <- list.files("./SupplementaryData/AuthorDiv", full.names = T)
AuthorCountries = lapply(FileNames, function(x) {
  dat = read_tsv(x, col_names = TRUE,skip = 0, comment="(") %>% select(-(3)) 
  # Add column names
  names(dat) = c("COUNTRY", "Articles")
  # Add a column with the year
  dat$YEAR = substr(x,33,36)
  return(dat)
})

#This is returned as a list, when binding below converts to dataframe with country as chr
AuthorCountries<-bind_rows(AuthorCountries)
AuthorCountries$YEAR<-as.numeric(AuthorCountries$YEAR)
#Delete the WOS percentage, add a column in whihc you generate it yourself
AuthorCountries<-AuthorCountries %>%  group_by(YEAR) %>% mutate(Pcnt_Pubs= (Articles/sum(Articles)*100)) %>% rename(N_Articles = Articles)
AuthorCountries
AuthorCountries<-AuthorCountries[AuthorCountries$YEAR>=FirstYear & AuthorCountries$YEAR<=LastYear,]

#sum(AuthorCountries$N_Articles)

#add country codes
source("Country.Codes.R")
AuthorCountries<-Country.Codes(AuthorCountries)
levels(AuthorCountries$geo.code)
### GENERATED NAs need to fund out whihc ones

AuPerCountryPerYr.LONG<-AuthorCountries %>% group_by(YEAR, geo.code) %>% summarize(Total = n_distinct(geo.code))
# AuPerCountryPerYr.LONG[is.na(AuPerCountryPerYr.LONG)] <- 0

AuCumulative<-AuthorCountries %>% ungroup() %>%  select(-Pcnt_Pubs, COUNTRY) %>% group_by(YEAR,geo.code) %>% summarize(yr_tot=sum(N_Articles))
# AuCumulativem$YEAR<-as.numeric(AuCumulativem$YEAR)
AuCumulative<-spread(AuCumulative, geo.code,yr_tot)
AuCumulative[is.na(AuCumulative)] <- 0
AuCumulative<-as_tibble(AuCumulative)
AuCumulativePlot<-specaccum(AuCumulative, "collector")

AuCumulativePlot<-as.data.frame(AuCumulativePlot$richness)
AuCumulativePlot$richness<-as.vector(AuCumulativePlot$richness)
names(AuCumulativePlot)[1] <- "CumulativeRichness"
AuCumulativePlot$YEAR<-seq(1985,2014,1)

EDvAuCumRich<-full_join(AuCumulativePlot, editorAcum, by = "YEAR")
EDvAuCumRich = EDvAuCumRich %>% select(YEAR, CumulativeRichness.x, CumulativeRichness.y) #reorder columns
EDvAuCumRich<-gather(EDvAuCumRich, "CumulativeRichness.x","CumulativeRichness.x", 2:3) 
EDvAuCumRich[EDvAuCumRich=="CumulativeRichness.x"]<-"Authors"
EDvAuCumRich[EDvAuCumRich=="CumulativeRichness.y"]<-"Editors"
names(EDvAuCumRich)[2] <- "Category"
names(EDvAuCumRich)[3] <- "N"
rm(AuPerCountryPerYr.LONG,AuthorCountries,AuCumulative,AuCumulativePlot)
#plot cumulative and annual richness same plot
EDvAuCumRichPlot<-ggplot(EDvAuCumRich, aes(x=YEAR, y=N, group = Category, colour = Category)) +
  geom_line(size=1) +
  scale_color_manual(values=c("blue", "red"))+
  geom_text(data = EDvAuCumRich[EDvAuCumRich$YEAR=="2012" & EDvAuCumRich$Category=="Editors",], aes(label = Category), hjust = 1, vjust = -1, size=5) +
  geom_text(data = EDvAuCumRich[EDvAuCumRich$YEAR=="2012" & EDvAuCumRich$Category=="Authors",], aes(label = Category), hjust = 1, vjust = -1, size=5) +
  ylab("Number of Countries") +
  xlab("Year")+
  ggtitle('Fig S2. Cumulative Geographic Richness from 1985-2014 of the editors for N = 24 environmental biology journals \n            and of the authors publishing in those journals during the same time perior (N = 113,816 articles).
')+
  geom_point(color="black", shape=1)+
  #scale_y_continuous(breaks = seq(20, 220,20))+
  scale_y_continuous(limits=c(1,max(EDvAuCumRich$N)+20))+
  scale_x_continuous(breaks=seq(1985, 2015, 5))

EDvAuCumRichPlot<-EDvAuCumRichPlot+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=10),  
        # legend.position = c(0.9,0.8),
        legend.position = ("none"),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1,1.5,1,1), "cm")) #+  #plot margin - top, right, bottom, left
EDvAuCumRichPlot
ggsave("FigS2.eps", plot = EDvAuCumRichPlot, device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)


######################################################
# Appendinx Fig 1: Geo Richness of EDITORS EACH YEAR 
######################################################
# No. of editors on each journal's board in each year
EdsJrnlYr<-AnalysisData %>% group_by(JOURNAL, YEAR) %>% summarize(TotalEditors = n_distinct(editor_id))
# No. of countries on each journal's board in each year
CountriesJrnlYr<-AnalysisData %>% group_by(JOURNAL, YEAR) %>% summarize(TotalCountries = n_distinct(geo.code))
# bound into a single dataframe
JrnlData<-full_join(EdsJrnlYr,CountriesJrnlYr, by=c("JOURNAL", "YEAR"))
rm(EdsJrnlYr,CountriesJrnlYr)
JrnlData<-JrnlData %>% select(JOURNAL, YEAR,TotalCountries)

JrnlRichnessFig<-ggplot(data=JrnlData, aes(x=YEAR, y=TotalCountries)) +
geom_line(size=1, color="blue")+
facet_wrap(~JOURNAL, nrow=6)+
  ylab("Geographic Richness") +
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 30, 5))+
  scale_x_continuous(breaks=seq(1985, 2014, 5))
JrnlRichnessFig<-JrnlRichnessFig+theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        strip.text = element_text(face = "italic", size = 10))
JrnlRichnessFig

#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left



######################################################
# Appendin Fig 2: Geo Diversity of EDITORS EACH YEAR split by JOURNAL
######################################################
EdsPerCountryPerJrnlPerYr.LONG<-AnalysisData %>% group_by(JOURNAL, YEAR, geo.code) %>% summarize(Total = n_distinct(editor_id))
EdsPerCountryPerJrnlPerYr.LONG[is.na(EdsPerCountryPerJrnlPerYr.LONG)] <- 0
DivDataJrnl<-as_tibble(EdsPerCountryPerJrnlPerYr.LONG)
DivDataJrnl<-spread(DivDataJrnl, geo.code, Total) 
DivDataJrnl[is.na(DivDataJrnl)] <- 0
DivDataJrnl<-ungroup(DivDataJrnl)
#Using simposns inverse
IsimpsonJRNL <- diversity((DivDataJrnl %>% select(-YEAR, - JOURNAL)), index="invsimpson") #Need to strip away the journal and year columns for vegan to do the analysis
# Table DIVERSITY with Results and Journals
JrnlIsimpDivTable <- data.frame(IsimpsonJRNL)
JrnlIsimpDivTable$YEAR <-DivDataJrnl$YEAR #Add year as a column
JrnlIsimpDivTable$JOURNAL <-DivDataJrnl$JOURNAL #Add year as a column
JrnlIsimpDivTable<-rename(JrnlIsimpDivTable, InvSimpson=IsimpsonJRNL) #rename the columns
JrnlIsimpDivTable <- JrnlIsimpDivTable[c("JOURNAL","YEAR","InvSimpson")] #reorder the columns
# ShannonDivTable<-arrange(ShannonDivTable, YEAR, desc(ShannonDiv)) # sort in descending order
JrnlIsimpDivTable

#computing evenness
GEOperYRJRNL<-EdsPerCountryPerJrnlPerYr.LONG %>% summarize(Countries = sum(n_distinct(geo.code)))
JrnlIsimpDivTable<-full_join(GEOperYRJRNL,JrnlIsimpDivTable, by=c("JOURNAL","YEAR"))
JrnlIsimpDivTable<-mutate(JrnlIsimpDivTable, Geo.Evenness = InvSimpson/Countries)

rm(GEOperYRJRNL, IsimpsonJRNL,DivDataJrnl,EdsPerCountryPerJrnlPerYr.LONG)


JrnlDiversityFig<-ggplot(data=JrnlIsimpDivTable, aes(x=YEAR, y=InvSimpson)) +
  geom_line(size=1, color="blue")+
  facet_wrap(~JOURNAL, nrow=6)+
  ylab("Geographic Diversity") +
  xlab("Year")+
  scale_y_continuous(breaks=seq(0, 18, 5))+   #####NEED TO SET MARGIN BY HIGHEST POSSIBLE VALUE FOR THAT JOURNAL???
  scale_x_continuous(breaks=seq(1985, 2014, 5))
JrnlDiversityFig<-JrnlDiversityFig+theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        strip.text = element_text(face = "italic", size = 10))
JrnlDiversityFig



######################################################
# Appendinx Fig 3: Geo Eveness of EDITORS EACH YEAR split by JOURNAL
######################################################

JrnlEvennessFig<-ggplot(data=JrnlIsimpDivTable, aes(x=YEAR, y=Geo.Evenness)) +
  geom_line(size=1, color="blue")+
  facet_wrap(~JOURNAL, nrow=6)+
  ylab("Geographic Evenness") +
  xlab("Year")+
  #scale_y_continuous(breaks=seq(0, 18, 5))+
  scale_x_continuous(breaks=seq(1985, 2014, 5))
JrnlEvennessFig<-JrnlEvennessFig+theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        strip.text = element_text(face = "italic", size = 10))
JrnlEvennessFig




######################################################
# Binding these up to make Fig. 1
######################################################
# source("multiplot.R")
# # if you jiust want to take a quick look at the results...
# AppA<-multiplot(JrnlRichnessFig, JrnlDiversityFig, JrnlEvennessFig, cols=1)
# # to save the figure in format for submission
# # for an explanation of why you need to do multiplot INSIDe of ggsave see: http://stackoverflow.com/questions/11721401/r-save-multiplot-to-file
# ggsave("Fig1.eps", plot = multiplot(JrnlRichnessFig, JrnlDiversityFig, JrnlEvennessFig, cols=1), device = "eps", scale = 1, width = 9, height = 21, units = c("in"), dpi = 300, limitsize = TRUE)










######################################################
# Appendinx Fig 4: Prop of EDITORS EACH YEAR FROM EACH REGION  plit by JOURNAL
######################################################

RegionyrJRNL<-AnalysisData %>% group_by(JOURNAL, YEAR, REGION) %>% summarize(Total = n_distinct(editor_id))
RegionyrJRNL[is.na(RegionyrJRNL)] <- 0
RegionyrJRNL2<-RegionyrJRNL %>% group_by(JOURNAL, YEAR)  %>%  summarise(Editors=sum(Total))
RegionyrJRNL<-full_join(RegionyrJRNL,RegionyrJRNL2, by=c("JOURNAL","YEAR")) %>% mutate(pcnt=Total/Editors*100)
rm(RegionyrJRNL2)
RegionyrJRNLFig<-ggplot(data=RegionyrJRNL, aes(x=YEAR, y=pcnt, color=REGION)) +
  geom_line(size=1)+
  scale_x_continuous(breaks=seq(1985, 2014, 5))+
  facet_wrap(~JOURNAL, nrow=6, scales="free")+
  # facet_wrap(~JOURNAL, nrow=6, scales="free")+ #USE THIS ONE IF YOU USE NUMBERS INSTEAD OF % DUE TO UNEQUAL ED BOARD SIZES
  ylab("Editors from different global regions (%)") +
  xlab("Year")
RegionyrJRNLFig<-RegionyrJRNLFig+theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        strip.text = element_text(face = "italic", size = 10))
RegionyrJRNLFig

#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left




######################################################
# Appendinx Fig 5: Geo Prop of EDITORS EACH YEAR FROM EACH INCOME CATEGORY  plit by JOURNAL
######################################################

INCOMEyrJRNL<-AnalysisData %>% group_by(JOURNAL, YEAR, INCOME_LEVEL) %>% summarize(Total = n_distinct(editor_id))
INCOMEyrJRNL[is.na(INCOMEyrJRNL)] <- 0
INCOMEyrJRNL2<-INCOMEyrJRNL %>% group_by(JOURNAL, YEAR)  %>%  summarise(Editors=sum(Total))
INCOMEyrJRNL<-full_join(INCOMEyrJRNL,INCOMEyrJRNL2, by=c("JOURNAL","YEAR")) %>% mutate(pcnt=Total/Editors*100)
rm(INCOMEyrJRNL2)
INCOMErJRNLFig<-ggplot(data=INCOMEyrJRNL, aes(x=YEAR, y=pcnt, color=INCOME_LEVEL)) +
  geom_line(size=1)+
  scale_x_continuous(breaks=seq(1985, 2014, 5))+
  facet_wrap(~JOURNAL, nrow=6, scales="free")+
  # facet_wrap(~JOURNAL, nrow=6, scales="free")+ #USE THIS ONE IF YOU USE NUMBERS INSTEAD OF % DUE TO UNEQUAL ED BOARD SIZES
  ylab("Editors from countries in different income categories (%)") +
  xlab("Year")
INCOMErJRNLFig<-INCOMErJRNLFig+theme_bw()+
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        # strip.background = element_rect(colour="black", fill="white"),
        panel.border = element_rect(colour = "black"),
        strip.text = element_text(face = "italic", size = 10))
INCOMErJRNLFig

#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
