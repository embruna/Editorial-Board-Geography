
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


#Data cleanup of Dryad Files proceeds as follows:

# 1. Bind the csv files you want together. 
# 2. Remove any spaces at the start or end of names, which will mess up the splitting of the names column into first, middle, last
# 3. remove some double spaces between or after names (i.e., "  ") 
# 4. Remove all the periods (some journals use them, others don't)
# 5. Correct a few errors in the names. These are mostly due to accent marks that 
#       were converted incorrectly or spelling mistakes in a subset of records for the same name (e.g., Cagan Sekercioglu was saved as "_a_an H _ekercio_lu")
# 6. For editors with multiple last names or that use multiple first or middle initials put names in a consistent "first middle last format 
#       (e.g., "Manuel G de Viedma"->"Manuel G DeViedma", "B N K Davis"->"B NK Davis"
# 7. Make the author names consistent accross journals (eg "E M Bruna", "Emilio Bruna", and "Emilio M. Bruna" all become "Emilio M Bruna"). This section
#       also conveerts names in ALL CAPS to Proper Case (EMILIO BRUNA -> Emilio Bruna). 
#       Best thing to o is actually to convert all to lower case prior to analysis to make sure none were missed
# 8. Correct a few locations and added a few notes to some records
# 9. Remove the suffixes (Jr. II, III)
# 10. Can now split names into 3 seperate columns: first name, middle name, last name

#Bind the data from Cho
ChoData<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE) 

#Remove (trim) the leading and trailing white spaces (not can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
trim.trailing <- function (x) sub("\\s+$", "", x)
ChoData$NAME<-trim.trailing(ChoData$NAME)
trim.leading <- function (x)  sub("^\\s+", "", x)
ChoData$NAME<-trim.leading(ChoData$NAME)

# remove any double spaces
ChoData$NAME<-gsub("  ", " ", ChoData$NAME, fixed=TRUE)

# Remove the periods from peoples names to make consistent accross all files
ChoData$NAME<-gsub(".", "", ChoData$NAME, fixed=TRUE) #Fixed makes it replace the ".", which is otherwise a wildcard

# Corrections to the database
ChoData$NAME <- as.character(ChoData$NAME) #Must first convert them from factor to string  

# are these necessary? DOESNT LOOK LIKE IT: DELETE
# ChoData$NAME[ChoData$NAME == "William Seaman, Jr"] <- "William Seaman"
# ChoData$NAME[ChoData$NAME == "Richard  D. Bardgett"] <- "Richard D Bardgett"
# ChoData$NAME[ChoData$NAME == "MW SCHUARTZ"] <- "Mark W Schwartz"
# ChoData$NAME[ChoData$NAME == "RL JEFFERIES"] <- "Robert L Jefferies"
# #Remove extra space at end of name
# ChoData$NAME[ChoData$NAME == "William H Karasov "] <- "William H Karasov"
# ChoData$NAME[ChoData$NAME == "Frank J Messina "] <- "Frank J Messina"
# ChoData$NAME[ChoData$NAME == "Vojtech Novotny "] <- "Vojtech Novotny"
# ChoData$NAME[ChoData$NAME == "Jason Warren "] <- "Jason Warren"
# ChoData$NAME[ChoData$NAME == "Philip M Dixon "] <- "Philip M Dixon"
# ChoData$NAME[ChoData$NAME == "Philip M Dixon "] <- "Philip M Dixon"


# Error Correction
ChoData$NAME[ChoData$NAME == "_a_an H _ekercio_lu"] <-"Cagan Sekercioglu"
ChoData$NAME[ChoData$NAME == "Ralph Mac Nally"] <- "Ralph MacNally"
ChoData$NAME[ChoData$NAME == "Enrique Mart_nez Meyer"] <- "Enrique Martinez-Meyer"
ChoData$NAME[ChoData$NAME == "Li (Aster) Zhang"] <- "Li Aster Zhang"
ChoData$NAME[ChoData$NAME == "Allan Blaylock"] <- "Alan Blaylock"
ChoData$NAME[ChoData$NAME == "Biswapti Mandal"] <- "Biswapati Mandal"
ChoData$NAME[ChoData$NAME == "Bret Dickson"] <- "Brett Dickson"
ChoData$NAME[ChoData$NAME == "Chister Nilsson"] <- "Christer Nilsson"
ChoData$NAME[ChoData$NAME == "Chirster Nilsson"] <- "Christer Nilsson"
ChoData$NAME[ChoData$NAME == "Cristopher Tingle"] <- "Christopher Tingle"
ChoData$NAME[ChoData$NAME == "DavidR Foster"] <- "David R Foster"
ChoData$NAME[ChoData$NAME == "Gerline Barbra DeDeyn"] <- "Gerlinde Barbra DeDeyn"
ChoData$NAME[ChoData$NAME == "Gerlinde DeDeyn"] <- "Gerlinde Barbra DeDeyn"
ChoData$NAME[ChoData$NAME == "Herman Urcola"] <- "Hernan Urcola"
ChoData$NAME[ChoData$NAME == "Johnathan A Newman"] <- "Jonathan A Newman"
ChoData$NAME[ChoData$NAME == "Josua Drew"] <- "Joshua Drew"
ChoData$NAME[ChoData$NAME == "Kiaas Metselaar"] <- "Klaas Metselaar"
ChoData$NAME[ChoData$NAME == "Loyd Stone"] <- "Lloyd Stone"
ChoData$NAME[ChoData$NAME == "Nthya Rajan"] <- "Nithya Rajan"
ChoData$NAME[ChoData$NAME == "Scoot D Wilson"] <- "Scott D Wilson"
ChoData$NAME[ChoData$NAME == "Tomas"  & ChoData$JOURNAL == "BITR"] <- "Tomas Carlo"
ChoData$NAME[ChoData$NAME == "Phill Watt"] <- "Phill Watts"
ChoData$NAME[ChoData$NAME == "Daniel Simberhoff"] <- "Daniel Simberloff"
ChoData$NAME[ChoData$NAME == "JM IRLONDO"] <- "Jose M Iriondo"
ChoData$NAME[ChoData$NAME == "MW SCHUARTZ"] <- "Mark W Schwartz"
ChoData$NAME[ChoData$NAME == "Marcel VanDeHeijden"] <- "Marcel VanDerHeijden"
ChoData$NAME[ChoData$NAME == "Prasanna Gowda"] <- "Prasanna Gowde"
ChoData$NAME[ChoData$NAME == "RL JEFFERLES"] <- "Robert L Jefferies"
ChoData$NAME[ChoData$NAME == "Roberto Salquero-Gomez"] <- "Roberto Salguero-Gomez"
ChoData$NAME[ChoData$NAME == "STEVE J HAWKIN"] <- "Steve J Hawkings"
ChoData$NAME[ChoData$NAME == "Anthony J Felder"] <- "Anthony J Fedler"
ChoData$NAME[ChoData$NAME == "Micahel Proctor"] <- "Michael CF Proctor"

# Changes made to account for multiple middle or last names
ChoData$NAME[ChoData$NAME == "JC DE M CARVALHO"] <- "JC DeM-Carvalho"
ChoData$NAME[ChoData$NAME == "A DE VOS"] <- "A DeVos"
ChoData$NAME[ChoData$NAME == "Andre M de Roos"] <- "Andre M DeRoos"
ChoData$NAME[ChoData$NAME == "Anthony Di Fiore"] <- "Anthony DiFiore"
ChoData$NAME[ChoData$NAME == "Arnold G van der Valk"] <- "Arnold G VanDerValk"
ChoData$NAME[ChoData$NAME == "Arturo Gomez Pompa"] <- "Arturo Gomez-Pompa"
ChoData$NAME[ChoData$NAME == "Ary T de Oliveira-Filho"] <- "Ary T DeOliveira-Filho"
ChoData$NAME[ChoData$NAME == "B N K Davis"] <- "B NK Davis"
ChoData$NAME[ChoData$NAME == "BNKDAVIS"] <- "B NK Davis"
ChoData$NAME[ChoData$NAME == "Michael J Van Den Avyle"] <- "Michael J VanDenAvyle"
ChoData$NAME[ChoData$NAME == "Eddy Van der Meijden"] <- "Eddy VanDerMeijden"
ChoData$NAME[ChoData$NAME == "Jorge Meave del Castillo"] <- "Jorge Meave DelCastillo"
ChoData$NAME[ChoData$NAME == "Nicole M Van Dam"] <- "Nicole M VanDam"
ChoData$NAME[ChoData$NAME == "Carlos Martinez del Rio"] <- "Carlos Martinez DelRio"
ChoData$NAME[ChoData$NAME == "Marcel van der Heijden"] <- "Marcel VanDerHeijden"
ChoData$NAME[ChoData$NAME == "Gerline Barbra de Deyn"] <- "Gerline Barbra DeDeyn"
ChoData$NAME[ChoData$NAME == "Marcel van de Heijden"] <- "Marcel VanDerHeijden"
ChoData$NAME[ChoData$NAME == "MANUEL G DE VIEDMA"] <- "Manuel G DeViedma"
ChoData$NAME[ChoData$NAME == "Eddy van der Meijden"] <- "Eddy VanDerMeijden"
ChoData$NAME[ChoData$NAME == "Rene van Der Wal"] <- "Rene VanDerWal"
ChoData$NAME[ChoData$NAME == "Hans de Kroon"] <- "Hans DeKroon"
ChoData$NAME[ChoData$NAME == "Gerlinde de Deyn"] <- "Gerlinde DeDeyn"
ChoData$NAME[ChoData$NAME == "Franciska De Vires"] <- "Franciska DeVires"
ChoData$NAME[ChoData$NAME == "Lindsey du Toit"] <- "Lindsey DuToit"
ChoData$NAME[ChoData$NAME == "Marinus J A Werger"] <- "Marinus JA Werger"
ChoData$NAME[ChoData$NAME == "Nathan Jared Boardman Kraft"] <- "Nathan JB Kraft"
ChoData$NAME[ChoData$NAME == "Chris van Kessel"] <- "Chris VanKessel"
ChoData$NAME[ChoData$NAME == "Jan van Groenendael"] <- "Jan VanGroenendael"
ChoData$NAME[ChoData$NAME == "Ellen van Donk"] <- "Ellen VanDonk"
ChoData$NAME[ChoData$NAME == "Tara Van Toai"] <- "Tara VanToai"
ChoData$NAME[ChoData$NAME == "Frits Van Evert"] <- "Frits VanEvert"
ChoData$NAME[ChoData$NAME == "Geraldine Vander Haegen"] <- "Geraldine VanderHaegen"
ChoData$NAME[ChoData$NAME == "Jay Ver Hoef"] <- "Jay VerHoef"
ChoData$NAME[ChoData$NAME == "James E M Watson"] <- "James EM Watson"
ChoData$NAME[ChoData$NAME == "EJ Milner-Gullard"] <- "E J Milner-Gulland"
ChoData$NAME[ChoData$NAME == "Diane De Steven"] <- "Diane DeSteven"
ChoData$NAME[ChoData$NAME == "MJ SCOTT"] <- "M J Scott"
ChoData$NAME[ChoData$NAME == "SIR PETER SCOTT"] <- "Peter Scott"



#Changes made to make names consistent accross journals 
ChoData$NAME[ChoData$NAME == "Arnold Bruns"] <- "H Arnold Bruns"
ChoData$NAME[ChoData$NAME == "Arthur Willis"] <- "Arthur J Willis"
ChoData$NAME[ChoData$NAME == "Carlos Herrera"] <- "Carlos M Herrera"
ChoData$NAME[ChoData$NAME == "David Wardle"] <- "David A Wardle"
ChoData$NAME[ChoData$NAME == "David Gibson"] <- "David J Gibson"
ChoData$NAME[ChoData$NAME == "David Pearson"] <- "David L Pearson"
ChoData$NAME[ChoData$NAME == "Donald Feener"] <- "Donald H Feener"
ChoData$NAME[ChoData$NAME == "Douglas Schemske"] <- "Douglas W Schemske"
ChoData$NAME[ChoData$NAME == "Emilio Bruna"] <- "Emilio M Bruna"
ChoData$NAME[ChoData$NAME == "Gary Meffe"] <- "Gary K Meffe"
ChoData$NAME[ChoData$NAME == "Gene Huntsman"] <- "Gene R Huntsman"
ChoData$NAME[ChoData$NAME == "Glenn Matlack"] <- "Glenn R Matlack"
ChoData$NAME[ChoData$NAME == "Herman Shugart"] <- "Herman H Shugart"
ChoData$NAME[ChoData$NAME == "James Hamrick"] <- "James L Hamrick"
ChoData$NAME[ChoData$NAME == "Jim Hamrick"] <- "James L Hamrick"
ChoData$NAME[ChoData$NAME == "John Etherington"] <- "John R Etherington"
ChoData$NAME[ChoData$NAME == "John Robinson"] <- "John G Robinson"
ChoData$NAME[ChoData$NAME == "John Ewel"] <- "John J Ewel"
ChoData$NAME[ChoData$NAME == "Ken Paige"] <- "Ken N Paige"
ChoData$NAME[ChoData$NAME == "Kurt Pregitzer"] <- "Kurt S Pregitzer"
ChoData$NAME[ChoData$NAME == "Lian P Koh"] <- "Lian Pin Koh"
ChoData$NAME[ChoData$NAME == "Malcolm Press"] <- "Malcolm C Press"
ChoData$NAME[ChoData$NAME == "Michael Usher"] <- "Michael B Usher"
ChoData$NAME[ChoData$NAME == "Navjot Sodhi"] <- "Navjot S Sodhi"
ChoData$NAME[ChoData$NAME == "Paul Angermeier"] <- "Paul L Angermeier"
ChoData$NAME[ChoData$NAME == "Peter Bellingham"] <- "Peter J Bellingham"
ChoData$NAME[ChoData$NAME == "Peter Moore"] <- "Peter D Moore"
ChoData$NAME[ChoData$NAME == "Philip Hedrick"] <- "Philip W Hedrick"
ChoData$NAME[ChoData$NAME == "Pieter Zuidema"] <- "Pieter A Zuidema"
ChoData$NAME[ChoData$NAME == "Richard Bardgett"] <- "Richard D Bardgett"
ChoData$NAME[ChoData$NAME == "Richard Mack"] <- "Richard N Mack"
ChoData$NAME[ChoData$NAME == "Richard Shefferson"] <- "Richard P Shefferson"
ChoData$NAME[ChoData$NAME == "Robert Jackson"] <- "Robert B Jackson"
ChoData$NAME[ChoData$NAME == "Robert Jones"] <- "Robert J Jones"
ChoData$NAME[ChoData$NAME == "Robert Marquis"] <- "Robert J Marquis"
ChoData$NAME[ChoData$NAME == "Scott Robinson"] <- "Scott K Robinson"
ChoData$NAME[ChoData$NAME == "Steven Gaines"] <- "Steven D Gaines"
ChoData$NAME[ChoData$NAME == "Will Cornwell"] <- "Will K Cornwell"
ChoData$NAME[ChoData$NAME == "Walter Carson"] <- "Walter P Carson"
ChoData$NAME[ChoData$NAME == "Willaim Raun"] <- "William Raun"
ChoData$NAME[ChoData$NAME == "William Lauenroth"] <- "William K Lauenroth"
ChoData$NAME[ChoData$NAME == "AW FABIO CASSOLA"] <- "AW Fabio Cassola"
ChoData$NAME[ChoData$NAME == "Fabio Cassola"] <- "AW Fabio Cassola"
ChoData$NAME[ChoData$NAME == "JL CRAIG"] <- "John L Craig"
ChoData$NAME[ChoData$NAME == "TR NEW"] <- "Tim R New"
ChoData$NAME[ChoData$NAME == "Matt mcGlone"] <- "Matthew McGlone"
ChoData$NAME[ChoData$NAME == "KR LIPS"] <- "Karen R Lips"
ChoData$NAME[ChoData$NAME == "MR WILLIG"] <- "Michael R Willig"
ChoData$NAME[ChoData$NAME == "Jonathan Newman"] <- "Jonathan A Newman"
ChoData$NAME[ChoData$NAME == "FG BERNALDEZ"] <- "F G Bernaldez"
ChoData$NAME[ChoData$NAME == "R GERALD WRIGHT"] <- "R Gerald Wright"
ChoData$NAME[ChoData$NAME == "FABIO CASSOLA"] <- "AW Fabio Cassola"
ChoData$NAME[ChoData$NAME == "F CASSOLA"] <- "AW Fabio Cassola"
ChoData$NAME[ChoData$NAME == "AP DOBSON"] <- "Andrew P Dobson"
ChoData$NAME[ChoData$NAME == "R H MARRS"] <- "R H Marrs"
ChoData$NAME[ChoData$NAME == "T CARO"] <- "Tim Caro"
ChoData$NAME[ChoData$NAME == "T SPARKS"] <- "Tim Sparks"
ChoData$NAME[ChoData$NAME == "E DUFFEY"] <- "Eric Duffey"
ChoData$NAME[ChoData$NAME == "F RUMSEY"] <- "Fred Rumsey"
ChoData$NAME[ChoData$NAME == "RG WRIGHT"] <- "R Gerald Wright"
ChoData$NAME[ChoData$NAME == "Scott Wilson"] <- "Scott D Wilson"
ChoData$NAME[ChoData$NAME == "Steve Gutreuter"] <- "Steven J Gutreuter"
ChoData$NAME[ChoData$NAME == "Steveb Evett "] <- "Steven Evett "
ChoData$NAME[ChoData$NAME == "B PRESSEY"] <- "Bob Pressey"
ChoData$NAME[ChoData$NAME == "Charles Canham"] <- "Charles D Canham"
ChoData$NAME[ChoData$NAME == "Charlie D Canham"] <- "Charles D Canham"
ChoData$NAME[ChoData$NAME == "Andrew Dobson"] <- "Andrew P Dobson"
ChoData$NAME[ChoData$NAME == "Phil Rundel"] <- "Philip Rundel"
ChoData$NAME[ChoData$NAME == "Rob Marrs"] <- "R H Marrs"
ChoData$NAME[ChoData$NAME == "J P METZGER"] <- "Jean Paul Metzger"
ChoData$NAME[ChoData$NAME == "L GUSTAFSSON"] <- "Lena Gustafsson"
ChoData$NAME[ChoData$NAME == "Mark Burgman"] <- "Mark A Burgman"
ChoData$NAME[ChoData$NAME == "M BURGMAN"] <- "Mark A Burgman"
ChoData$NAME[ChoData$NAME == "MW SCHWARTZ"] <- "Mark W Schwartz"
ChoData$NAME[ChoData$NAME == "Ray Callaway"] <- "Ragan M Callaway"
ChoData$NAME[ChoData$NAME == "Ray M Callaway"] <- "Ragan M Callaway"
ChoData$NAME[ChoData$NAME == "Tony Davy"] <- "Anthony J Davy"
ChoData$NAME[ChoData$NAME == "Anthony Davy"] <- "Anthony J Davy"
ChoData$NAME[ChoData$NAME == "B MOSS"] <- "Brian Moss"
ChoData$NAME[ChoData$NAME == "C GUYER"] <- "Craig Guyer"
ChoData$NAME[ChoData$NAME == "D A SAUNDERS"] <- "Denis A Saunders"
ChoData$NAME[ChoData$NAME == "DB LINDENMAYER"] <- "David B Lindenmayer"
ChoData$NAME[ChoData$NAME == "James Clark"] <- "James S Clark"
ChoData$NAME[ChoData$NAME == "K KIRBY"] <- "Keith Kirby"
ChoData$NAME[ChoData$NAME == "P POSCHLOD"] <- "Peter Poschlod"
ChoData$NAME[ChoData$NAME == "P WATTS"] <- "Phill Watts"
ChoData$NAME[ChoData$NAME == "A B GILL"] <- "Andrew B Gill"
ChoData$NAME[ChoData$NAME == "A S PULLIN"] <- "Andrew S Pullin"
ChoData$NAME[ChoData$NAME == "A TAYLOR"] <- "Andrea Taylor"
ChoData$NAME[ChoData$NAME == "A YOUNG"] <- "Andrew Young"
ChoData$NAME[ChoData$NAME == "GF PETERKEN"] <- "George F Peterken"
ChoData$NAME[ChoData$NAME == "R CORLETT"] <- "Richard T Corlett"
ChoData$NAME[ChoData$NAME == "R DIRZO"] <- "Rodolfo Dirzo"
ChoData$NAME[ChoData$NAME == "R FRANKHAM"] <- "Richard Frankham"
ChoData$NAME[ChoData$NAME == "R B PRIMACK"] <- "Richard B Primack"
ChoData$NAME[ChoData$NAME == "S WALDREN"] <- "Stephen Waldren"
ChoData$NAME[ChoData$NAME == "WF LAURANCE"] <- "William F Laurance"
ChoData$NAME[ChoData$NAME == "K A WITH"] <- "Kimberly A With"
ChoData$NAME[ChoData$NAME == "AJT JOHNSINGH"] <- "A JT Johnsingh"
ChoData$NAME[ChoData$NAME == "James Cahil"] <- "James Cahill"
ChoData$NAME[ChoData$NAME == "JM IRIONDO"] <- "Jose M Iriondo"
ChoData$NAME[ChoData$NAME == "MW SCHWARTZ"] <- "Mark W Schwartz"
ChoData$NAME[ChoData$NAME == "RL JEFFERIES"] <- "Robert L Jefferies"
ChoData$NAME[ChoData$NAME == "Robert Jefferies"] <- "Robert L Jefferies"
ChoData$NAME[ChoData$NAME == "RobertL Jefferies"] <- "Robert L Jefferies"
ChoData$NAME[ChoData$NAME == "Robert J Jefferies"] <- "Robert L Jefferies"
ChoData$NAME[ChoData$NAME == "Eelke Jonegejanns"] <- "Eelke Jongejans"
ChoData$NAME[ChoData$NAME == "Michelle Lesihman"] <- "Michelle Leishman"
ChoData$NAME[ChoData$NAME == "Roberth A Raguso"] <- "Robert A Raguso"
ChoData$NAME[ChoData$NAME == "Robert Raguso"] <- "Robert A Raguso"
ChoData$NAME[ChoData$NAME == "Steveb Evett"] <- "Steven Evett"
ChoData$NAME[ChoData$NAME == "John Young"] <- "John R Young"
ChoData$NAME[ChoData$NAME == "Steve Hawkings"] <- "Steve J Hawkings"
ChoData$NAME[ChoData$NAME == "FB GOLDSMITH"] <- "F B Goldsmith"
ChoData$NAME[ChoData$NAME == "Amy Austin"] <- "Amy T Austin"
ChoData$NAME[ChoData$NAME == "Robert Salguero-Gomez"] <- "Roberto Salguero-Gomez"
ChoData$NAME[ChoData$NAME == "E Duffey"] <- "Eric Duffey"
ChoData$NAME[ChoData$NAME == "S J ANDELMAN"] <- "Sandy J Andelman"
ChoData$NAME[ChoData$NAME == "Sandy Andelman"] <- "Sandy J Andelman"
ChoData$NAME[ChoData$NAME == "Michael C F Proctor"] <- "Michael CF Proctor"  
ChoData$NAME[ChoData$NAME == "Michael Proctor"] <- "Michael CF Proctor"

#Found a few with incorrect country where based and added a few notes 
ChoData$COUNTRY[ChoData$NAME == "J Grace"] <- "UK"
ChoData$COUNTRY[ChoData$NAME == "David J Gibson"] <- "USA"
ChoData$COUNTRY[ChoData$NAME == "Richard D Bardgett"] <- "UK"
#for notes must first convert them from factor to string  
ChoData$NOTES <- as.character(ChoData$NOTES) 
ChoData$NOTES[ChoData$NAME == "J Grace"] <- "probJohnGraceUofEdinborough"
ChoData$NOTES[ChoData$NAME == "Robert Jenkins"] <- "RobertEJenkins-TNC"

# Remove the suffixes
ChoData$NAME<-gsub(", Jr", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" Jr", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" JR", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" III", "", ChoData$NAME, fixed=TRUE)
ChoData$NAME<-gsub(" II", "", ChoData$NAME, fixed=TRUE)


# Split the names into first, middle, last
ChoData$NAME <- as.factor(ChoData$NAME) # CHnage back to factor. Can also do with strings, but I learned this way first

ChoData<-separate(ChoData, NAME, c("FIRST_NAME", "LAST_NAME"), sep = " ", remove = FALSE, convert = FALSE, extra = "merge", fill = "right")
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



# Now make sure all names, cases, categories, etc. are consistent
# THIS SHOULD BE CONVERETED TO A FUNCTION!!!!!
#

# JrnlToClean<-ChoData
JrnlToClean<-ClassData 
head(JrnlToClean)
head(ChoData)
#remove extra spaces, converts to chr
JrnlToClean$CATEGORY<-gsub(" ", "", JrnlToClean$CATEGORY, fixed=TRUE) 

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

CHECKFILE<-JrnlToClean %>%
  group_by(FULL_NAME,LAST_NAME,FIRST_NAME,MIDDLE_NAME) %>% 
  tally(sort=FALSE)
str(CHECKFILE)
CHECKFILE<-as.data.frame(CHECKFILE)
which(CHECKFILE == "")
CHECKFILE[CHECKFILE == ""] <- NA
CHECKFILE<-droplevels(CHECKFILE)
CHECKFILE$FIRSTLAST_NAME<-paste(CHECKFILE$FIRST_NAME,CHECKFILE$LAST_NAME, sep=" ")
str(CHECKFILE)
summary(CHECKFILE)

CHECKFILE$NAME<-as.character(CHECKFILE$FULL_NAME)
CHECKFILE$FIRST_NAME<-as.character(CHECKFILE$FIRST_NAME)
CHECKFILE$LAST_NAME<-as.character(CHECKFILE$LAST_NAME)
CHECKFILE$MIDDLE_NAME<-as.character(CHECKFILE$MIDDLE_NAME)
CHECKFILE$FIRSTLAST_NAME<-as.character(CHECKFILE$FIRSTLAST_NAME)

str(CHECKFILE)

# This will look over the names and check for mistakes, spelling errors, etc.
# LAST NAMES: this should help pick up things like Abrams vs Abrasm


CheckNames<-CHECKFILE$NAME  #ClassData$FULL_NAME
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

# summary(NamesDF)
# str(NamesDF)

# Create a column to which you will add a logical condition telling you if the names are an EXACT match
NamesDF$match<-NA
NamesDF$match<-NamesDF$Name1==NamesDF$Name2
# match2<-ifelse(NamesDF$match=="TRUE",1,0) #convert TRUE/FALSEto 0/1
# NamesDF<-cbind(NamesDF,match2) 
# head(NamesDF,40)
# str(NamesDF)
NamesDF<-arrange(NamesDF,Name1,Name2) #organize in alphabetica order
NamesDF<-filter(NamesDF, match==FALSE)  # THIS DELETES ALL NAMES THAT ARE 100% MATCH 
head(NamesDF)
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
NamesDF$index<-seq.int(nrow(NamesDF)) #adds a column with an index to make it easier to id which row you need'
NamesDF
write.csv(NamesDF, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/NameCheckCLASS-FIRST-LAST.csv", row.names = T) #export it as a csv file

head(NamesDF)


summary(ChoData)









# 
# 
# #### END OF FUNCTION 1
# 
# #look over the file, identify the names (rows) that eed to be fixed. Create a vector of those row numbers. That will be used in function 2 to pull them out of the file
# # CHO NAMES TO FIX
# # NamesDFfix<-c(1,3:38,46:49,52,56:58,60,61,62,64,67:69,71,81,84,86,88,97,98,102:104,107,
# #               111,122:123,129,142:144,146,147,153:155,158,159,161,168,170,171,185:189,
# #               195,200,207,212,213,214,222,226,227,263,269,270,273:276,278,279,282,284,307)
# 
# NamesDFfix<-NamesDF$index
# 
# #Now select them out
# NamesDFfix<-slice(NamesDF,NamesDFfix)
# 
# # split the Name1 and Name2 into seperate columns for 1st and last name
# NamesDFfix<-separate(NamesDFfix, Name1, c("Name1first", "Name1last"), sep = " ", remove = TRUE, convert = FALSE)
# NamesDFfix<-separate(NamesDFfix, Name2, c("Name2first", "Name2last"), sep = " ", remove = TRUE, convert = FALSE)
# # Do the 1st names match each other
# NamesDFfix$FirstNamesMatch<-NA
# NamesDFfix$FirstNamesMatch<-NamesDFfix$Name1first==NamesDFfix$Name2first
# # do the last names match each other
# NamesDFfix$LastNamesMatch<-NA
# NamesDFfix$LastNamesMatch<-NamesDFfix$Name1last==NamesDFfix$Name2last
# 
# 
# # THESE ARE THE ONES THAT NEED TO BE 2x in ChoData
# LastToFix<-filter(NamesDFfix,FirstNamesMatch==TRUE & LastNamesMatch==FALSE) #These suggest the last name is misspelled
# FirstToFix<-filter(NamesDFfix,FirstNamesMatch==FALSE & LastNamesMatch==TRUE) #These suggest the first name is misspelled
# AllToFix<-filter(NamesDFfix,FirstNamesMatch==FALSE & LastNamesMatch==FALSE) #either 1) first ÅND last name is mispelled OR  Something needs to be 2x
# 
# # additional checks
# index == 146 | index == 147) #Huntley
# filter(NamesDFfix, index == 22 | index == 23) #all combos of spellings r wright rg wright
# 
# 
# 
# filter(ChoData,tolower(LAST_NAME)==NamesDFfix$Name1last[1])
# filter(ChoData,LAST_NAME=="Simberhoff")
# filter(ChoData,tolower(LAST_NAME)=="simberhoff")
# filter(ChoData,tolower(LAST_NAME)==NamesDFfix[7,2])
# filter(ChoData,tolower(LAST_NAME)==LastToFix$Name1last)
# # tolower("Simberhoff")
# # ChoData %>% filter(tolower(LAST_NAME==Name1last))
# ## ChoData %>% filter(tolower(ChoData$LAST_NAME)==(LastToFix[1,2]) | LastToFix[1,4]))
# # filter(ChoData,tolower(LAST_NAME)=="simberhoff")
# # filter(ChoData,(tolower(LAST_NAME)=="iriondo" | tolower(LAST_NAME)=="irlondo"))
# # filter(ChoData,(tolower(LAST_NAME)=="cahil" | tolower(LAST_NAME)=="cahill"))
# 
# 
# ChoData$LAST_NAME <- as.character(ChoData$LAST_NAME) #Must first convert them from factor to string  

 





















# use high similarity of last name but low of first name to find misspelled or shortened first name
# use high similarity of first name but low of last name to find misspelled last names
# confirm with complete string similarity



############################################################
# Organiation & Cleaning: CLASSDATA  
############################################################

#Bind the data from 2015 workshop

ClassData<-rbind(AGRON2, AMNAT, ARES2, BIOCON2, BIOG, BITR2, ECOG, EVOL, FEM, FUNECOL, 
                 JANE, JAPE, JTE2, JZOOL, LECO, MARECOL, NAJFM2, NEWPHYT, OECOL, OIKOS, PLANTECOL) 

str(ClassData)
summary(ClassData)
ClassData$JOURNAL<-as.factor(ClassData$JOURNAL)

####FIX THIS
ClassData[which(ClassData$JOURNAL==""),] #are there any with no journal?
ClassData[which(ClassData$FIRST_NAME==""),] #are there any with no 1st name?
ClassData[which(ClassData$LAST_NAME==""),] #are there any with no 1st name?

ClassData$FIRST_NAME<-as.character(ClassData$FIRST_NAME)
ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Mar\x90a"] <- "Mar-x90a"
ClassData$FIRST_NAME[ClassData$FIRST_NAME == "J\xd3rg"] <- "J-xd3rg"
#####


foo<-ClassData %>%
  select(JOURNAL, YEAR)  %>% 
  filter(YEAR>1984 & YEAR<2015)
str(foo)
summary(foo)
summary_table<-as.data.frame(table(foo$YEAR, foo$JOURNAL))
missing_yrs<-filter(summary_table,Freq<1) %>% #Filters the summary table to include years for whihc zero records
  filter(Var2!="AGRONOMY") %>%  # Eliminates the ones that are extnsions of Cho et al data
  filter(Var2!="AREES") %>% 
filter(Var2!="BIOCON")%>% 
filter(Var2!="BITR")%>% 
filter(Var2!="JTE")%>% 
  filter(Var2!="NAJFM")%>% 
filter(Var2!="") # Eliminates any missing journal

write.csv(missing_yrs, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData_missingYrs.csv", row.names = T) #export it as a csv file



str(ClassData)
# Make the data types consistent with ChoData 

ClassData$VOLUME<-as.integer(ClassData$VOLUME)
ClassData$ISSUE<-as.integer(ClassData$ISSUE)


#Remove (trim) the leading and trailing white spaces (not can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
trim.trailing <- function (x) sub("\\s+$", "", x)
ClassData$FIRST_NAME<-trim.trailing(ClassData$FIRST_NAME)
ClassData$MIDDLE_NAME<-trim.trailing(ClassData$MIDDLE_NAME)
ClassData$LAST_NAME<-trim.trailing(ClassData$LAST_NAME)
trim.leading <- function (x)  sub("^\\s+", "", x)
ClassData$FIRST_NAME<-trim.leading(ClassData$FIRST_NAME)
ClassData$MIDDLE_NAME<-trim.leading(ClassData$MIDDLE_NAME)
ClassData$LAST_NAME<-trim.leading(ClassData$LAST_NAME)
# remove any double spaces

ClassData$FIRST_NAME<-gsub("  ", " ", ClassData$FIRST_NAME, fixed=TRUE)
ClassData$LAST_NAME<-gsub("  ", " ", ClassData$LAST_NAME, fixed=TRUE)
ClassData$MIDDLE_NAME<-gsub("  ", " ", ClassData$MIDDLE_NAME, fixed=TRUE)

# Remove the periods from peoples names to make consistent accross all files
ClassData$FIRST_NAME<-gsub(".", "", ClassData$FIRST_NAME, fixed=TRUE) #Fixed makes it replace the ".", which is otherwise a wildcard
ClassData$MIDDLE_NAME<-gsub(".", "", ClassData$MIDDLE_NAME, fixed=TRUE)
ClassData$LAST_NAME<-gsub(".", "", ClassData$LAST_NAME, fixed=TRUE)

# Corrections to the database
str(ClassData)
ClassData$FULL_NAME<-paste(ClassData$FIRST_NAME,ClassData$MIDDLE_NAME,ClassData$LAST_NAME, sep=" ")
# Remove the periods from peoples names to make consistent accross all files
ClassData$FULL_NAME<-gsub("  ", " ", ClassData$FULL_NAME, fixed=TRUE)

##DOUBLE CHECK WHICH THESE ARE IN. IF THEY ARE IN NEW DATA CAN CORRECT!!!!!
# 1) SYSTEMATIZE OTHER, SPECIAL, PRODUCTION in CATEGORY COLUMN
# 2) EVOL: several titles missing 
# 3) AMNAT: 1985-1992 has two volumes for each year. use oone? both? 
# 4) AMNAT: some missing volume and issue data
# 5) AMNAT: Need to correct AE for Editor
# 6) Oecologia has several EIC's (plants, animals, etc)
# 7 One name missing in Oecologia due to blurry pic
#8) Removed MEPS, GCB because so many years missing.






















######################################################
#
# STANDARDINZING THE COUNTRY CODES ON CLEAN DATASETS
# Make this a function
#
######################################################

DATASET<-ChoData OR DATASET<-ClassData

#step3: change all the country names to the codes used in mapping
#Add a column with the 3 letter country codes to be consistent with the other datasets
#Maptools uses the ISO 3166 three letter codes: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#The packahge countrycode will take your column of country names and convert them to ISO3166-3 Codes
#I began by checking the values of COUNTRY to see if there are any mistakes. To do so I just created a vector 
#called CODECHECK
DATASET$CODECHECK<-countrycode(DATASET$COUNTRY, "country.name", "iso3c", warn = TRUE)
#By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
#You can correct these as follows in the dataframe with all the data, then add a new column to the dataframe with the country codes

DATASET$COUNTRY[DATASET$COUNTRY == "USA "]  <- "USA" #One of the datasets in Cho et al had a space after USA so needs to be corrected
DATASET$COUNTRY[DATASET$COUNTRY == "lndonesia"]  <- "Indonesia" #One of the datasets in Cho et al had Indonesia mispelled somewhere
DATASET$COUNTRY[DATASET$COUNTRY == "Scotland"]  <- "UK" #With apologies to Scots everywhere
DATASET$COUNTRY[DATASET$COUNTRY == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
DATASET$COUNTRY[DATASET$COUNTRY == "Wales"]  <- "UK"
DATASET$COUNTRY[DATASET$COUNTRY == "England"]  <- "UK"
DATASET$COUNTRY[DATASET$COUNTRY == "German Democratic Republic"]  <- "Germany" #removing old names

#we need to change yugoslavia to what?
#we need to add french guiana wold bank classficiation







#This line adds a column of country codes based on the country name
#some countries may not be correctly coded
DATASET$COUNTRY.CODE<-countrycode(DATASET$COUNTRY, "country.name", "iso3c", warn = TRUE)   #create new column with country ISO code


#These lines add the income level and region level based on the editor country
DATASET$INCOME_LEVEL <- WDI_data[DATASET$COUNTRY.CODE, 'income']  #Making a new column of income level by country
DATASET$REGION <- WDI_data[DATASET$COUNTRY.CODE, 'region']  #Making a new column of income level by country

#subsetting data to only EIC, AE and SE classifications
DATASET <- DATASET[DATASET$CATEGORY %in% c('EIC', 'AE', 'SE'),]

#step 4: choose the temporal coverage
#use only 1985 to 2013 
DATASET<-DATASET[DATASET$YEAR>=1985 & DATASET$YEAR<=2013,]

#step 5: 2x that it all looks ok
summary(DATASET)

#2x check - are there any with country missing?
MISSING=subset(DATASET, subset=(COUNTRY=="?"))
MISSING

#Deleting rows without country
DATASET <- DATASET[!is.na(DATASET$COUNTRY.CODE),] 


############################################################################
#
# BIND THEM UP AND ANALYZE!
#
############################################################################

# str(ChoData)
# str(ClassData)

ChoData<-ChoData %>% 
  select(-NOTES, -GENDER)

ClassData<-ClassData %>% 
  select(-INSTITUTION,-NOTES,-GENDER,-SUFFIX)

AllJournals<-rbind(ChoData,ClassData)
str(AllJournals)
summary(AllJournals)


############################################################################################
# BAR PLOT TOTAL EDITORIAL MEMBERS BY COUNTRY (ALL JOURNALS, ALL YEARS)
# GROUPED COUNTRIES WITH SMALL SIZES
############################################################################################
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

