Cho.Fix <- function(A) {
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
  
  
  # Error Correction
  
  ChoData$NAME[ChoData$NAME == "_a_an H _ekercio_lu"] <-"Cagan Sekercioglu"
  ChoData$NAME[ChoData$NAME == "STEVE J HAWKINGS"] <-"Stephen J Hawkins"
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
  ChoData$NAME[ChoData$NAME == "STEVE J HAWKIN"] <- "Stephen J Hawkins"
  ChoData$NAME[ChoData$NAME == "Anthony J Felder"] <- "Anthony J Fedler"
  ChoData$NAME[ChoData$NAME == "Micahel Proctor"] <- "Michael CF Proctor"
  ChoData$NAME[ChoData$NAME == "Saeid Soufizadeh_"] <- "Saeid Soufizadeh"
  ChoData$NAME[ChoData$NAME == "Spencer H C Barrett"] <- "Spencer CH Barrett"
  ChoData$NAME[ChoData$NAME == "Soo-hyung Kim"] <- "Soo-Hyung Kim"  
  
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
  ChoData$NAME[ChoData$NAME == "Marcel van der Heijden"] <- "Marcel VanDerHeijden"
  ChoData$NAME[ChoData$NAME == "Gerline Barbra de Deyn"] <- "Gerlinde Barbra DeDeyn"
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
  ChoData$NAME[ChoData$NAME == "Kim Wllliams-Guillen"] <- "Kim Williams-Guillen"
  ChoData$NAME[ChoData$NAME == "Daniel Issermann"] <- "Daniel Isermann"
  ChoData$NAME[ChoData$NAME == "STEVE J HAWKINGS"] <- "Steve J Hawkins"
  ChoData$NAME[ChoData$NAME == "Johnathan Holman"] <- "Johnathon Holman"
  ChoData$NAME[ChoData$NAME == "J. P. METZGER"] <- "Jean-Paul Metzger"
  ChoData$NAME[ChoData$NAME == "Carlos Martinez del Rio"] <- "Carlos MartinezDelRio"
  ChoData$NAME[ChoData$NAME == "Prasanna Gowde"] <- "Prasanna Gowda"
  ChoData$NAME[ChoData$NAME == "Victoria Stork"] <- "Victoria Sork"
  ChoData$NAME[ChoData$NAME == "Kasutuv Roy"] <- "Kaustuv Roy"
  ChoData$NAME[ChoData$NAME == "Ferry Silk"] <- "Ferry Slik"
  ChoData$NAME[ChoData$NAME == "P.A.R. HOCKEY"] <- "Philip AR Hockey"
  ChoData$NAME[ChoData$NAME == "Phil A R Hockey"] <- "Philip AR Hockey"
  ChoData$NAME[ChoData$NAME == "Gerline Barbra de Deyn"] <- "Gerlinde Barbra de Deyn"
  
  
  #Changes made to make names consistent accross journals 
  
  ChoData$NAME[ChoData$NAME == "MJ COE"] <- "M J COE"
  ChoData$NAME[ChoData$NAME == "Steve Bonser"] <- "Stephen P Bonser" # NEED TO CONFIRM
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
  ChoData$NAME[ChoData$NAME == "Steve Hawkings"] <- "Steve J Hawkins"
  ChoData$NAME[ChoData$NAME == "FB GOLDSMITH"] <- "F B Goldsmith"
  ChoData$NAME[ChoData$NAME == "Amy Austin"] <- "Amy T Austin"
  ChoData$NAME[ChoData$NAME == "Robert Salguero-Gomez"] <- "Roberto Salguero-Gomez"
  ChoData$NAME[ChoData$NAME == "E Duffey"] <- "Eric Duffey"
  ChoData$NAME[ChoData$NAME == "S J ANDELMAN"] <- "Sandy J Andelman"
  ChoData$NAME[ChoData$NAME == "Sandy Andelman"] <- "Sandy J Andelman"
  ChoData$NAME[ChoData$NAME == "Michael C F Proctor"] <- "Michael CF Proctor"  
  ChoData$NAME[ChoData$NAME == "Michael Proctor"] <- "Michael CF Proctor"
  ChoData$NAME[ChoData$NAME == "Judie Bronstein"] <- "Judith L Bronstein"
  ChoData$NAME[ChoData$NAME == "Jean Paul Metzger"] <- "Jean-Paul Metzger"
  ChoData$NAME[ChoData$NAME == "LENNART HANSSON"] <- "Lennart Hansson"
  
  
  #Found a few with incorrect country where based and added a few notes 
  ChoData$COUNTRY[ChoData$NAME == "J Grace"] <- "UK"
  ChoData$COUNTRY[ChoData$NAME == "David J Gibson"] <- "USA"
  ChoData$COUNTRY[ChoData$NAME == "Richard D Bardgett"] <- "UK"
  ChoData$COUNTRY[ChoData$NAME == "Steven P Bonser"] <- "Australia"
  
  #for notes must first convert them from factor to string  
  ChoData$NOTES <- as.character(ChoData$NOTES) 
  ChoData$NOTES[ChoData$NAME == "J Grace"] <- "probJohnGraceUofEdinborough"
  ChoData$NOTES[ChoData$NAME == "Robert Jenkins"] <- "RobertEJenkins-TNC"
  ChoData$COUNTRY[ChoData$NAME == "Steven P Bonser"] <- "Australia"
  
  
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
  
  
  
  # Adding combinations of names to the database
  # First Name, Last Name
  ChoData$FirstLast<-paste(ChoData$FIRST_NAME,ChoData$LAST_NAME, sep=" ") 
  # First Name, Middle Name, Last Name
  ChoData$FirstMiddleLast<-paste(ChoData$FIRST_NAME,ChoData$MIDDLE_NAME,ChoData$LAST_NAME, sep=" ")
  # First initial 1st name + last name": 
  ChoData$FIRST_INIT<-as.character(ChoData$FIRST_NAME)
  ChoData$FIRST_INIT<-substring(ChoData$FIRST_INIT,1,1)
  ChoData$FirstInitialLast<-paste(ChoData$FIRST_INIT,ChoData$LAST_NAME, sep=" ")
  ChoData$FIRST_INIT<-NULL #delete it out now that we don't need it
  #Delete column with suffix
  ChoData$SUFFIX<-NULL #delete it out now that we don't need it
  #Delete column with original Name
  ChoData$NAME<-NULL #delete it out now that we don't need it
  
  
  # Remove the periods from peoples names to make consistent accross all files
  ChoData$FirstLast<-gsub("  ", " ", ChoData$FirstLast)
  ChoData$FirstMiddleLast<-gsub("  ", " ", ChoData$FirstMiddleLast)
  ChoData$FirstInitialLast<-gsub("  ", " ", ChoData$FirstInitialLast)
  
  ChoData$FIRST_NAME<-as.character(ChoData$FIRST_NAME)
  ChoData$MIDDLE_NAME<-as.character(ChoData$MIDDLE_NAME)
  ChoData$LAST_NAME<-as.character(ChoData$LAST_NAME)
  ChoData$NOTES<-as.factor(ChoData$NOTES)
  
  #Add a column for Institution in case you need it later
  ChoData$INSTITUTION<-NA
  
  # Correct some of the countries
  ChoData$COUNTRY[ChoData$COUNTRY == "Austrailia"]  <- "Australia" #removing old names
  ChoData$COUNTRY[ChoData$COUNTRY == "USA "]  <- "USA" #One of the datasets in Cho et al had a space after USA so needs to be corrected
  ChoData$COUNTRY[ChoData$COUNTRY == "lndonesia"]  <- "Indonesia" #One of the datasets in Cho et al had Indonesia mispelled somewhere
  ChoData$COUNTRY[ChoData$COUNTRY == "?"]  <- NA
  ChoData$COUNTRY[ChoData$COUNTRY == "Unknown"]  <- NA


  # Make Gender Consistent
  ChoData$GENDER <- as.character(ChoData$GENDER) 
  ChoData$GENDER[ChoData$GENDER == "F"] <- "female"
  ChoData$GENDER[ChoData$GENDER == "M"] <- "male"
  ChoData$GENDER[ChoData$GENDER == "U"] <- NA
  ChoData$GENDER[ChoData$GENDER == "Unkown"] <- NA
  ChoData$GENDER[ChoData$GENDER == "Unknown"] <- NA
  ChoData$GENDER[ChoData$GENDER == ""] <- NA
  ChoData$GENDER <- as.factor(ChoData$GENDER)
  ChoData$GENDER<-droplevels(ChoData$GENDER)
  
  ChoData$COUNTRY[ChoData$COUNTRY == "USa"]  <- "USA" # correcting names


  ChoData_clean<-ChoData

  return(ChoData_clean)
  
}
