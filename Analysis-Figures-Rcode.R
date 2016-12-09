
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN ANALYSIS OF THE GEOGRAPHY OF EDITORIAL BOARDS
#This is a clone of the code in the Github Repo for analaysis of Gender and Editorial Boards (https://github.com/embruna/Editorial-Board-Gender).

#CLear out everything from the environment 
rm(list=ls())


#Set WD and load packages you need. Not all of which you need after all.
#setwd("-------")
library(countrycode)
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
library(WDI)

source("helpers.R")    #Code to plot all journals in one figure



######################################################
# DATA UPLOAD 
######################################################
# Step 1: load the individual CSV files and save them as dataframes

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

#Import data from Cho et al 2014 PeerJ
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

#Import Data collected by 2015 UF Scientific Publishing Seminar 
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
LECO<-read.csv("./Data2015/LECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
MARECOL<-read.csv("./Data2015/MARECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
NAJFM2<-read.csv("./Data2015/NAJFM2.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
NEWPHYT<-read.csv("./Data2015/NEWPHYT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) 
OECOL<-read.csv("./Data2015/OECOL.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
OIKOS<-read.csv("./Data2015/OIKOS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE ) #5 are missing country
PLANTECO<-read.csv("./Data2015/PLANTECO.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

#
# STILL MISSING SOME DATA # WILL nEED TO 2x NAMES ON THESE AND ADD TO LIST BELOW
#

# GCB<-read.csv("./Data2015/GCB.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
# #Still missing years and putting eds into cats
# #LE is missing 2004, 2011-2014
# #Need to define as EIC, SE, AE, Other
# MEPS<-read.csv("./Data2015/MEPS.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
# #Need to define as EIC, SE, AE, Other
# #INCOMPLETE


######################################################
# DATA CLEANUP AND ORGANIZATION: CHODATA
######################################################

ChoData<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE) #Bind the data from Cho

# Names are all in one cell. Split them so first, middle, and last name are in seperate columns. 
# This requires some clean-up of the csv files archived at dryad first: 
  # 1. remove some extra spaces from some of the names
  # 2. Correct some spelling
  # 3. Put multiple middle names together in one column  

# separate(data, col, into, sep = " ", remove = TRUE, convert = FALSE) # http://garrettgman.github.io/tidying/
# which(ChoData == "Rene van Der Wal")

# remove any double spaces
ChoData$NAME<-gsub("  ", " ", ChoData$NAME, fixed=TRUE)

# Fix individual names
ChoData$NAME <- as.character(ChoData$NAME) #Must first convert them from factor to string  
ChoData$NAME[ChoData$NAME == "J.C. DE M. CARVALHO"] <- "JC DeM-Carvalho"
ChoData$NAME[ChoData$NAME == "William H Karasov "] <- "William H Karasov"
ChoData$NAME[ChoData$NAME == "Frank J Messina "] <- "Frank J Messina"
ChoData$NAME[ChoData$NAME == "Vojtech Novotny "] <- "Vojtech Novotny"
ChoData$NAME[ChoData$NAME == "Jason Warren "] <- "Jason Warren"
ChoData$NAME[ChoData$NAME == "_a_an H. _ekercio_lu"] <-"Cagan Sekercioglu"
ChoData$NAME[ChoData$NAME == "A. DE VOS"] <- "A DeVos"
ChoData$NAME[ChoData$NAME == "Andre M de Roos"] <- "Andre M DeRoos"
ChoData$NAME[ChoData$NAME == "Anthony Di Fiore"] <- "Anthony DiFiore"
ChoData$NAME[ChoData$NAME == "Arnold G van der Valk"] <- "Arnold G VanDerValk"
ChoData$NAME[ChoData$NAME == "Arturo Gomez Pompa"] <- "Arturo Gomez-Pompa"
ChoData$NAME[ChoData$NAME == "Ary T de Oliveira-Filho"] <- "Ary T DeOliveira-Filho"
ChoData$NAME[ChoData$NAME == "B.N.K.DAVIS"] <- "B. N. K. DAVIS"
ChoData$NAME[ChoData$NAME == "Michael J. Van Den Avyle"] <- "Michael J. VanDenAvyle"
ChoData$NAME[ChoData$NAME == "Eddy Van der Meijden"] <- "Eddy VanDerMeijden"
ChoData$NAME[ChoData$NAME == "Jorge Meave del Castillo"] <- "Jorge Meave DelCastillo"
ChoData$NAME[ChoData$NAME == "William Seaman, Jr."] <- "William Seaman"
ChoData$NAME[ChoData$NAME == "Nicole M Van Dam"] <- "Nicole M VanDam"
ChoData$NAME[ChoData$NAME == "Carlos Martinez del Rio"] <- "Carlos Martinez DelRio"
ChoData$NAME[ChoData$NAME == "Marcel van der Heijden"] <- "Marcel VanDerHeijden"
ChoData$NAME[ChoData$NAME == "Gerline Barbra de Deyn"] <- "Gerline Barbra DeDeyn"
ChoData$NAME[ChoData$NAME == "Marcel van de Heijden"] <- "Marcel VanDeHeijden"
ChoData$NAME[ChoData$NAME == "MANUEL G. DE VIEDMA"] <- "Manuel G DeViedma"
ChoData$NAME[ChoData$NAME == "Eddy van der Meijden"] <- "Eddy VanDerMeijden"
ChoData$NAME[ChoData$NAME == "Rene van Der Wal"] <- "Rene VanDerWal"
ChoData$NAME[ChoData$NAME == "Philip M. Dixon "] <- "Philip M Dixon"
ChoData$NAME[ChoData$NAME == "Philip M Dixon "] <- "Philip M Dixon"
ChoData$NAME[ChoData$NAME == "Richard  D. Bardgett"] <- "Richard D Bardgett"
ChoData$NAME[ChoData$NAME == "Hans de Kroon"] <- "Hans DeKroon"
ChoData$NAME[ChoData$NAME == "Gerlinde de Deyn"] <- "Gerlinde DeDeyn"
ChoData$NAME[ChoData$NAME == "Franciska De Vires"] <- "Franciska DeVires"
ChoData$NAME[ChoData$NAME == "Diane DeSteven"] <- "Diane DeSteven"
ChoData$NAME[ChoData$NAME == "Lindsey du Toit"] <- "Lindsey DuToit"
ChoData$NAME[ChoData$NAME == "Marinus J A Werger"] <- "Marinus JA Werger"
ChoData$NAME[ChoData$NAME == "Nathan Jared Boardman Kraft"] <- "Nathan JB Kraft"
ChoData$NAME[ChoData$NAME == "Ralph Mac Nally"] <- "Ralph MacNally"
ChoData$NAME[ChoData$NAME == "Enrique Mart_nez Meyer"] <- "Enrique Martinez-Meyer"
ChoData$NAME[ChoData$NAME == "Chris van Kessel"] <- "Chris VanKessel"
ChoData$NAME[ChoData$NAME == "Jan van Groenendael"] <- "Jan VanGroenendael"
ChoData$NAME[ChoData$NAME == "Ellen van Donk"] <- "Ellen VanDonk"
ChoData$NAME[ChoData$NAME == "Tara Van Toai"] <- "Tara VanToai"
ChoData$NAME[ChoData$NAME == "Frits Van Evert"] <- "Frits VanEvert"
ChoData$NAME[ChoData$NAME == "Geraldine Vander Haegen"] <- "Geraldine VanderHaegen"
ChoData$NAME[ChoData$NAME == "Jay Ver Hoef"] <- "Jay VerHoef"
ChoData$NAME[ChoData$NAME == "Li (Aster) Zhang"] <- "Li Aster Zhang"

# Remove the suffixes
ChoData$NAME<-gsub(", Jr", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" Jr", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" JR", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" III", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" II", "", ChoData$NAME, fixed=TRUE)

# Remove the periods from peoples names to make consistent accross all files
ChoData$NAME<-gsub(".", "", ChoData$NAME, fixed=TRUE) #Fixed makes it replace the ".", which is otherwise a wildcard

# Split the names into first, middle, last
ChoData$NAME <- as.factor(ChoData$NAME) # CHnage back to factor. Can also do with strings, but I learned this way first

ChoData<-separate(ChoData, NAME, c("FIRST_NAME", "LAST_NAME"), sep = " ", remove = TRUE, convert = FALSE, extra = "merge", fill = "right")
ChoData<-separate(ChoData, LAST_NAME, c("MIDDLE_NAME_1", "LAST_NAME"), sep = " ", remove = TRUE, extra = "merge", fill = "left")
ChoData<-separate(ChoData, LAST_NAME, c("MIDDLE_NAME_2", "LAST_NAME"), sep = " ", remove = TRUE, extra = "merge", fill = "left")
ChoData$MIDDLE_NAME_TEMP<- with(ChoData, (paste(MIDDLE_NAME_1, MIDDLE_NAME_2))) #Paste the two middle names together
ChoData$MIDDLE_NAME_TEMP<-gsub("NA", "", ChoData$MIDDLE_NAME_TEMP, fixed=TRUE) #delete all the NA
ChoData$MIDDLE_NAME_TEMP<-gsub(" ", "", ChoData$MIDDLE_NAME_TEMP, fixed=TRUE)  #remove any excess spaces
ChoData$MIDDLE_NAME_1 <- ChoData$MIDDLE_NAME_TEMP
ChoData$MIDDLE_NAME_2 <- NULL
ChoData$MIDDLE_NAME_TEMP <- NULL
ChoData<-rename(ChoData, MIDDLE_NAME=MIDDLE_NAME_1)

ChoData$FIRST_NAME <- as.factor(ChoData$FIRST_NAME) #They were converted to chr above, so convert back to factor
ChoData$MIDDLE_NAME <- as.factor(ChoData$MIDDLE_NAME)
ChoData$LAST_NAME <- as.factor(ChoData$LAST_NAME)

# WHY ISN"T THIS PIPING WORKING???
# ChoData %>% 
#   select(FIRST_NAME, MIDDLE_NAME, LAST_NAME)  %>% 
#   mutate_each(funs(as.factor))
 


############################################################
# Organiation & Cleaning: CLASSDATA  
############################################################

ClassData<-rbind(AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, EVOL, FEM, FUNECOL, 
                 JANE, JAPE, JTE2, JZOOL, LECO, MARECOL, NAJFM2, NEWPHYT, OECOL, OIKOS,PLANTECO) #Bind the data from 2015 workshop

# Make the data types consistent with ChoData 

ClassData$VOLUME<-as.integer(ClassData$VOLUME)
ClassData$ISSUE<-as.integer(ClassData$ISSUE)




CHECKFILE<-filter(CHECKFILE,CHECKFILE$FIRST_NAME!="Mar\x90a")
CHECKFILE<-filter(CHECKFILE,CHECKFILE$FIRST_NAME!="J\xd3rg")

######################################

# BIND THEM UP

# str(ChoData)
# str(ClassData)

ChoData<-ChoData %>% 
  select(-NOTES, -GENDER)

ClassData<-ClassData %>% 
  select(-INSTITUTION,-NOTES,-GENDER,-SUFFIX)

AllJournals<-rbind(ChoData,ClassData)
str(AllJournals)
summary(AllJournals)



# NOW NEED TO MAKE SURE ALL NAMES ARE CONSISTENT, CASES, CATEGORRIES, ETC, Make Cap 1st letter, rest lowercase
# THIS SHOULD BE CONVERETED TO A FUNCTION

JrnlToClean<-ChoData

JrnlToClean$CATEGORY<-gsub(" ", "", JrnlToClean$CATEGORY, fixed=TRUE) #remove extra spaces, converts to chr


##DOUBLE CHECK WHICH THESE ARE IN. IF THEY ARE IN NEW DATA CAN CORRECT!!!!!
##SYSTEMATIZE OTHER, SPECIAL, PRODUCTION in CATEGORY COLUMN
# 3512   Briones   Mar\x90a          JI 10
# 4099     Kudla    J\xd3rg        <NA> 1

# Several in NAMES1 are apparently wrong
#Alan G
# E VanDer
#Evan S
# #R NA
# George H
#Allan G
#Last name A, First KIMBERLY


JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "Ae"] <- "AE"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "OTHER"] <- "Other"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "other"] <- "Other"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "SPECIAL"] <- "Special"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "Production editor"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "Production Staff"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "Journal Supervisor"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "JPS"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "JS"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "PE"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "Productioneditor"] <- "PS"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == "EDITOR-IN-CHIEF"] <- "EIC"
JrnlToClean$CATEGORY[JrnlToClean$CATEGORY == ""] <- ""

JrnlToClean$CATEGORY <- as.factor(JrnlToClean$CATEGORY) #Convert back to factor
JrnlToClean$CATEGORY<-droplevels(JrnlToClean$CATEGORY)
# 
# Trying to find names that are mispelled or close to correct close
#   http://stackoverflow.com/questions/6683380/techniques-for-finding-near-duplicate-records
# # https://cran.r-project.org/web/packages/RecordLinkage/index.html AND
# # https://cran.r-project.org/web/packages/stringdist/stringdist.pdf
# # https://cran.r-project.org/web/packages/RecordLinkage/RecordLinkage.pdf
# https://journal.r-project.org/archive/2010-2/RJournal_2010-2_Sariyar+Borg.pdf
# http://stackoverflow.com/questions/11535625/similarity-scores-based-on-string-comparison-in-r-edit-distance
# http://stackoverflow.com/questions/28952034/finding-partial-matches-on-strings-in-r

str(JrnlToClean)
levels(JrnlToClean$CATEGORY)
which(JrnlToClean$CATEGORY=="Other") 
summary(JrnlToClean$CATEGORY)

# 
# JrnlToClean %>% group_by("LAST_NAME","FIRST_NAME","MIDDLE_NAME") %>% 
#    summarise("count"=cumsum("LAST_NAME"))
# JrnlToClean %>% tally(group_by("LAST_NAME","FIRST_NAME","MIDDLE_NAME")) %>% summarise(count=tally("LAST_NAME"))
# str(JrnlToClean$LAST_NAME)
# str(CHECKFILE)

CHECKFILE<-JrnlToClean %>%
  group_by(LAST_NAME,FIRST_NAME,MIDDLE_NAME) %>% 
  tally(sort=FALSE)
str(CHECKFILE)
CHECKFILE<-as.data.frame(CHECKFILE)
which(CHECKFILE == "")
CHECKFILE[CHECKFILE == ""] <- NA
CHECKFILE<-droplevels(CHECKFILE)
CHECKFILE$COMPLETE_NAME<-paste(CHECKFILE$FIRST_NAME,CHECKFILE$LAST_NAME, sep=" ")
str(CHECKFILE)
summary(CHECKFILE)
CHECKFILE$FIRST_NAME<-as.character(CHECKFILE$FIRST_NAME)
CHECKFILE$LAST_NAME<-as.character(CHECKFILE$LAST_NAME)
CHECKFILE$MIDDLE_NAME<-as.character(CHECKFILE$MIDDLE_NAME)
CHECKFILE$COMPLETE_NAME<-as.character(CHECKFILE$COMPLETE_NAME)

str(CHECKFILE)

# This will look over the names and check for mistakes, spelling errors, etc.
# LAST NAMES: this should help pick up things like Abrams vs Abrasm


CheckNames<-CHECKFILE$COMPLETE_NAME
CheckNames<-tolower(CheckNames) #drop all to lower case - makes it easier to error check and analyze
CheckNames<-unique(CheckNames)

# This uses agrep to check similarity, then outputs a list of all names in your file compared to 
# all other names. This is what will help find spelling mistakes, eg. "abrams" and "abrasm"  will be counted as unique, as will 
# "e bruna" and "emilio bruna". You can use this info to error correct or make changes to correctly pool the people with multiple names
NamesList<-sapply(CheckNames,agrep,CheckNames, value=TRUE) 

# Convert this list to a dataframe (with help from this post:   
# https://aurelienmadouasse.wordpress.com/2012/05/22/r-code-how-to-convert-a-list-to-a-data-frame/)

NamesDF<-data.frame(
  Name1 = rep(names(NamesList), lapply(NamesList, length)),
  Name2 = unlist(NamesList))

summary(NamesDF)
str(NamesDF)

# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match<-NA
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
# match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
# NamesDF<-cbind(NamesDF,match2) 
# head(NamesDF,40)
# str(NamesDF)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH 

# Convert to chr
NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
# str(NamesDF)

# Calclulate the proportional similarity and # changes required to go from one name to another. Package RecordLinkage
NamesDF$Name_sim<-levenshteinSim(NamesDF$Name1, NamesDF$Name2)
NamesDF$Name_dist<-levenshteinDist(NamesDF$Name1, NamesDF$Name2)

# Because this does all pairwise comparisons, it results in redundancy: "e bruna vs emilio bruna" and "emilio bruna vs e bruna"
# are in different rows, even though they are the same "comparison". This deletes one of the two 
NamesDF<-NamesDF[!duplicated(t(apply(NamesDF, 1, sort))),]
# this arranges them in order from most similar (1 change required) to least similar.
# look carefully at those with a few changes, as they are likely to be a tiny spelling mistake or difference in intials

NamesDF<-arrange(NamesDF,Name_dist,Name1)
write.csv(NamesDF, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/NameCheck.csv", row.names = F) #export it as a csv file










# NamesDF %>% distinct
# 
# 
# a <- c("pear","pear","apple","kiwi")
# b <- c("apple","apple","pear","watermelon")
# df <-data.frame(a,b)
# df[!duplicated(t(apply(df, 1, sort))),]


# a <- c("pear","pear","apple","kiwi")
# b <- c("apple","apple","pear","watermelon")
# df <-data.frame(a,b)
# distinct(NamesDF, Name1, Name2)
# NamesDF[!duplicated(NamesDF[,c('Name1', 'Name2')]),]
# NamesDF<-unique(NamesDF[,c('Name1','Name2')])


NamesDF<-unique(NamesDF["Name1","Name2"],)
NamesDF<-unique(t(apply(NamesDF, 1, sort)))
NamesDF<-arrange(NamesDF,desc(Name_sim))
head(NamesDF,40)
# trying to run throws errors showing these have mistakes
# Briones   Mar\x90a          
# Kudla    J\xd3rg     

CHECKFILE$last_sim<-levenshteinSim(CHECKFILE$LAST_NAME, CHECKFILE$LAST_NAME)
CHECKFILE$both_sim<-levenshteinSim(CHECKFILE$COMPLETE_NAME, CHECKFILE$COMPLETE_NAME)
# CHECKFILE<-arrange(CHECKFILE, desc(first_sim))
NamesDF<-arrange(NamesDF, fullName_sim)
head(NamesDF,50)



sapply(CHECKFILE$LAST_NAME,agrep,CHECKFILE$LAST_NAME) #Trying to speedup the check for misspelled or duplicate names
  

sapply(CHECKFILE$LAST_NAME,agrep,CHECKFILE$LAST_NAME, value=TRUE)
str(CHECKFILE$COMPLETE_NAME)
foo<-sapply(CHECKFILE$COMPLETE_NAME,agrep,CHECKFILE$COMPLETE_NAME, value=TRUE) #MAKES A LIST

# then convert to a dataframe  
# https://aurelienmadouasse.wordpress.com/2012/05/22/r-code-how-to-convert-a-list-to-a-data-frame/
NamesDF<-data.frame(
  Name1 = rep(names(foo), lapply(foo, length)),
  Name2 = unlist(foo))
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
match2<-ifelse(NamesDF$match=="TRUE",1,0)
NamesDF<-cbind(NamesDF,match2) #convert TRUE/FALSEto 0/1
head(NamesDF,20)
str(NamesDF)
NamesDF<-arrange(NamesDF,desc(Name1,Name2))
NamesDF<-filter(NamesDF, match2=="0")

NamesDF$Name1<-as.character(NamesDF$Name1)
NamesDF$Name2<-as.character(NamesDF$Name2)
str(NamesDF)
###HERE IT IS!!! HOW TO COMPARE NAMES

# library(RecordLinkage)
# NamesDF$similarity<-levenshteinSim(A, B)
# NamesDF<-arrange(NamesDF, desc(similarity))
# NamesDF<-arrange(NamesDF, similarity)
# NamesDF[3,1]
# NamesDF[3,2]
# 
# # DO THE FOLLOWING
# 1. compare similarity of last name
# 2. compare similarity of first Name
# 3 compare similarity of 2 names together in one name

# use high similarity of last name but low of first name to find misspelled or shortened first name
# use high similarity of first name but low of last name to find misspelled last names
# confirm with complete string similarity




C<-compare.linkage (A,B, blockfld = FALSE,
                 phonetic = FALSE, strcmp = FALSE,
                 strcmpfun = jarowinkler, exclude = FALSE, identity1 = NA, identity2 = NA,
                 n_match = NA, n_non_match = NA)





#step3: change all the country names to the codes used in mapping
#Add a column with the 3 letter country codes to be consistent with the other datasets
#Maptools uses the ISO 3166 three letter codes: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#The packahge countrycode will take your column of country names and convert them to ISO3166-3 Codes
#I began by checking the values of COUNTRY to see if there are any mistakes. To do so I just created a vector 
#called CODECHECK
JrnlToClean$CODECHECK<-countrycode(JrnlToClean$COUNTRY, "country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
#You can correct these as follows in the dataframe with all the data, then add a new column to the dataframe with the country codes

JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "USA "]  <- "USA" #One of the datasets in Cho et al had a space after USA so needs to be corrected
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "lndonesia"]  <- "Indonesia" #One of the datasets in Cho et al had Indonesia mispelled somewhere
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "Scotland"]  <- "UK" #With apologies to Scots everywhere
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "Wales"]  <- "UK"
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "England"]  <- "UK"
JrnlToClean$COUNTRY[JrnlToClean$COUNTRY == "German Democratic Republic"]  <- "Germany" #removing old names

#we need to change yugoslavia to what?
#we need to add french guiana wold bank classficiation







#This line adds a column of country codes based on the country name
#some countries may not be correctly coded
JrnlToClean$COUNTRY.CODE<-countrycode(JrnlToClean$COUNTRY, "country.name", "iso3c", warn = TRUE)   #create new column with country ISO code


#These lines add the income level and region level based on the editor country
JrnlToClean$INCOME_LEVEL <- WDI_data[JrnlToClean$COUNTRY.CODE, 'income']  #Making a new column of income level by country
JrnlToClean$REGION <- WDI_data[JrnlToClean$COUNTRY.CODE, 'region']  #Making a new column of income level by country

#subsetting data to only EIC, AE and SE classifications
JrnlToClean <- JrnlToClean[JrnlToClean$CATEGORY %in% c('EIC', 'AE', 'SE'),]

#step 4: choose the temporal coverage
#use only 1985 to 2013 
JrnlToClean<-JrnlToClean[JrnlToClean$YEAR>=1985 & JrnlToClean$YEAR<=2013,]

#step 5: 2x that it all looks ok
summary(JrnlToClean)

#2x check - are there any with country missing?
MISSING=subset(JrnlToClean, subset=(COUNTRY=="?"))
MISSING

#Deleting rows without country
JrnlToClean <- JrnlToClean[!is.na(JrnlToClean$COUNTRY.CODE),] 


##############################################
# BAR PLOT TOTAL EDITORIAL MEMBERS BY COUNTRY (ALL JOURNALS, ALL YEARS)
# GROUPED COUNTRIES WITH SMALL SIZES
##############################################
#Group dataframe by COUNTRY.CODE
byCOUNTRY <- dplyr::group_by(AllJournals, COUNTRY.CODE)

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each EIC only once
byCOUNTRY <- unique( byCOUNTRY[ , c('NAME', 'COUNTRY.CODE', 'JOURNAL') ] )

#Count the number of unique editors by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(NAME)))

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
highest_n$COUNTRY.CODE <- factor(x = highest_n$COUNTRY.CODE,
                                levels = highest_n$COUNTRY.CODE)

highest_n$total=sum(highest_n$number) #this will allow you to calclulate % and plot that way
highest_n$percent=highest_n$number/highest_n$total*100

tiff(file = "Plots/COUNTRY_Editors.tiff",
     width = 500,
     height = 500)
#Plot of EIC numbers by country in decreasing number

country_plot<-ggplot(data=highest_n, aes(x=COUNTRY.CODE, y=percent)) +   #changed this to % instead of absolute #
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
#Group dataframe by COUNTRY.CODE
byCOUNTRY <- dplyr::group_by(AllJournals, COUNTRY.CODE, CATEGORY)

#Editors can perform duties for >1 year, so we remove the duplicate names to make sure we count each editor only once
byCOUNTRY <- unique( byCOUNTRY[ , c('NAME', 'COUNTRY.CODE', 'JOURNAL', 'CATEGORY') ] )

#Count the number of unique editors by country
byCOUNTRY = summarize (byCOUNTRY,
                       number = length(unique(NAME)))

#Reshape the table from long to wide
byCOUNTRY <- dcast(data = byCOUNTRY,
                   formula = COUNTRY.CODE ~ CATEGORY, 
                   value.var = 'number')

#changing NA to 0
byCOUNTRY[is.na(byCOUNTRY)] <- 0

#Converting to proportion
byCOUNTRY['EIC_perc'] <-  byCOUNTRY$EIC / sum(byCOUNTRY$EIC)
byCOUNTRY['AE_perc'] <-  byCOUNTRY$AE / sum(byCOUNTRY$AE)
byCOUNTRY['SE_perc'] <-  byCOUNTRY$SE / sum(byCOUNTRY$SE)

#Assign each country a WDI income and region
byCOUNTRY$INCOME_LEVEL <- WDI_data[byCOUNTRY$COUNTRY.CODE, 'income']  #Making a new column of income level by country
byCOUNTRY$REGION <- WDI_data[byCOUNTRY$COUNTRY.CODE, 'region']  #Making a new column of income level by country

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
round(100*byCOUNTRY[byCOUNTRY$COUNTRY.CODE == 'USA',5:7], 1)
round(100*byCOUNTRY[byCOUNTRY$COUNTRY.CODE == 'GBR',5:7], 1)


##############################################
# PLOT NUMBER OF COUNTRIES REPRESENTED BY YEAR BY JOURNAL ALL CATEGORIES
# WITH LINE ADDING HIGH INCOME COUNTRIES (OECD AND NON-OECD)
##############################################
#Group dataframe by CATEGORY, JOURNAL AND YEAR
COUNTRYYEAR <- dplyr::group_by(AllJournals, JOURNAL, YEAR)

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
                                  COUNTRIES = length(unique(COUNTRY.CODE)),
                                  HIGHINCOME = sum(unique(COUNTRY.CODE) %in% 
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
INCOME_byYEAR <- dplyr::group_by(AllJournals, INCOME_LEVEL, YEAR)
#Group dataframe by REGION, CATEGORY, YEAR 
REGION_byYEAR <- dplyr::group_by(AllJournals, REGION, YEAR)

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
tiff(file = "Plots/REGION_AllJournals.tiff",
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
tiff(file = "Plots/INCOME_AllJournals.tiff",
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
INCOME_byYEARJOURNAL <- dplyr::group_by(AllJournals, 
                                        JOURNAL, YEAR, INCOME_LEVEL)
REGION_byYEARJOURNAL <- dplyr::group_by(AllJournals, 
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

