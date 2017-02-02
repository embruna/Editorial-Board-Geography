Class.Fix <- function(A) {
  
  str(ClassData)
  summary(ClassData)
  ClassData$JOURNAL<-as.factor(ClassData$JOURNAL)
  #write.csv(ClassData, file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/ClassData.csv", row.names = T) #export it as a csv file
  
  # THIS REMOVEAS A FEW WITH BLANKS IN THE NAMES
  ClassData <-filter(ClassData, ClassData$FIRST_NAME!="" & ClassData$LAST_NAME!="")
  # Error Correction
  
  ####FIX THIS
  # ClassData[which(ClassData$JOURNAL==""),] #are there any with no journal?
  # ClassData[which(ClassData$FIRST_NAME==""),] #are there any with no 1st name?
  # ClassData[which(ClassData$LAST_NAME==""),] #are there any with no 1st name?
  
  ClassData$FIRST_NAME<-as.character(ClassData$FIRST_NAME)
  #####
  
 
  str(ClassData)
  # Make the data types consistent with ChoData 
  
  ClassData$VOLUME<-as.integer(ClassData$VOLUME)
  ClassData$ISSUE<-as.integer(ClassData$ISSUE)
  
  
  #Remove (trim) the leading and trailing white spaces (note can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  ClassData$FIRST_NAME<-trim.trailing(ClassData$FIRST_NAME)
  ClassData$MIDDLE_NAME<-trim.trailing(ClassData$MIDDLE_NAME)
  ClassData$LAST_NAME<-trim.trailing(ClassData$LAST_NAME)
  ClassData$TITLE<-trim.trailing(ClassData$TITLE)
  
  trim.leading <- function (x)  sub("^\\s+", "", x)
  ClassData$FIRST_NAME<-trim.leading(ClassData$FIRST_NAME)
  ClassData$MIDDLE_NAME<-trim.leading(ClassData$MIDDLE_NAME)
  ClassData$LAST_NAME<-trim.leading(ClassData$LAST_NAME)
  ClassData$TITLE<-trim.leading(ClassData$TITLE)
  
  # remove any double spaces
  ClassData$FIRST_NAME<-gsub("  ", " ", ClassData$FIRST_NAME)
  ClassData$LAST_NAME<-gsub("  ", " ", ClassData$LAST_NAME, fixed=TRUE)
  ClassData$MIDDLE_NAME<-gsub("  ", " ", ClassData$MIDDLE_NAME, fixed=TRUE)
  
  # Remove the periods from peoples names to make consistent accross all files
  ClassData$FIRST_NAME<-gsub("[.]","", ClassData$FIRST_NAME) # . is a wildcard, so [.] removes only the period.
  ClassData$MIDDLE_NAME<-gsub(".", "", ClassData$MIDDLE_NAME, fixed=TRUE)
  ClassData$LAST_NAME<-gsub(".", "", ClassData$LAST_NAME, fixed=TRUE)
  
  
  # which(ClassData$FIRST_NAME=="Marc-Andr_")
  # which(ClassData$MIDDLE_NAME=="JT")
  # which(ClassData$LAST_NAME=="Selosse")
  
  
  # FIRST NAMES TO BE CORRECTED
  ClassData$FIRST_NAME[ClassData$LAST_NAME == "Briones"] <- "Maria"
  ClassData$FIRST_NAME[ClassData$LAST_NAME == "Kudla"] <- "Jorg" 
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Aliastair"] <- "Alastair"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Marc-Andr_"] <- "Marc-Andre"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "J_rgen"] <- "Jurgen"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Pihilip"] <- "Philip"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Ricardo"] <- "Riccardo"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Daphnew"] <- "Daphne"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Emflia"] <- "Emilia"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Clharles"] <- "Charles"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Daniei"] <- "Daniel"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Dianne"] <- "Diane"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Harol"] <- "Harold"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Jefery"] <- "Jeffry"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Natalie"] <- "Nathalie"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Candance"] <- "Candace"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Pfter"] <- "Peter"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Rolbert"] <- "Robert"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Andr_"] <- "Andre"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Vejo"] <- "Veijo"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Ikka"] <- "Ilkka"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Drie"] <- "Dries"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Jean-Michelle"] <- "Jean-Michel"
  ClassData$FIRST_NAME[ClassData$FIRST_NAME == "Neal"] <- "Neil" #JBIOG has his name wrong in the journal - corrct is Neil j enright, not neal l enright
  ClassData$MIDDLE_NAME[ClassData$FIRST_NAME == "Neil"] <- "J" #JBIOG has his name wrong in the journal - corrct is Neil j enright, not neal l enright
  ClassData$FIRST_NAME[ClassData$FIRST == "Charles" & ClassData$LAST_NAME == "Godfray"] <- "H" #WORKING?
  
  
  # MIDDLE NAMES TO BE CORRECTED
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "J A"] <- "JA"
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "Richiard"] <- "Richard"
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "AlbertC"] <- "Albert C"
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "GA"] <- "G A"
  ClassData$MIDDLE_NAME[ClassData$FIRST_NAME == "H" & ClassData$LAST_NAME == "Godfray"] <- "CharlesJ" #WORKING?
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "Paolo"] <- ""
  ClassData$MIDDLE_NAME[ClassData$MIDDLE_NAME == "G"] <- "Green"
  
  # LAST NAMES TO BE CORRECTED
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Saltzburger"] <- "Salzburger"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Ballar_"] <- "Ballare"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "JD"] <- "JT"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Mueller" & ClassData$FIRST_NAME == "Caroline"] <- "Muller" #WORKING?
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Fairburn"] <- "Fairbairn"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Buerger"] <- "Burger"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Colwel"] <- "Colwell"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Abrecht"] <- "Albrecht"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Meaghe"] <- "Meagher"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "McCailum"] <- "McCallum"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Mit[On"] <- "Mitton"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Gerber"] <- "Geber"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Harevey"] <- "Harvey"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "O'Donnell"] <- "ODonnell"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Odonnell"] <- "ODonnell"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Beashop"] <- "Bearhop"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Plnero"] <- "Pinero"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Coyn"] <- "Coyne"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Fryxwell"] <- "Fryxell"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Lillim"] <- "Lill"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Lillm"] <- "Lill"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Ericksson"] <- "Eriksson"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Bennet"] <- "Bennett"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Pim"] <- "Pimm"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Rot"] <- "Roth"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Sieman"] <- "Siemann"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Yl_nen"] <- "Ylonen"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Lillm"] <- "Lill"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Niemala"] <- "Niemela"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Schmid"] <- "Schmitz"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Fielder"] <- "Fiedler"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Karieva"] <- "Kareiva"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Diaz-Filho"] <- "Diniz-Filho"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Diniz-Filho"] <- "Diniz-Filho"
  ClassData$FIRST_NAME[ClassData$LAST_NAME == "Diniz-Filho"] <- "Jose" #WORKING?
  ClassData$MIDDLE_NAME[ClassData$LAST_NAME == "Diniz-Filho"] <- "Alexandre" #WORKING?
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Rea"] <- "Real"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Paolo"] <- "Paolo-Patti"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "Patti"] <- "Paolo-Patti"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "H" & ClassData$FIRST_NAME=="George"] <- "Heimpel"
  ClassData$LAST_NAME[ClassData$LAST_NAME == "vanderhaijden"] <- "vanderheijden"
  
  # Cleaning up the titles
  # #remove extra spaces, converts to chr
  # ClassData$TITLE<-gsub(" ", "", ClassData$TITLE, fixed=TRUE) 
  
  ClassData$TITLE<-gsub("\\ ", ".", ClassData$TITLE) #Replace spaces with period
  ClassData$TITLE<-gsub(":", "", ClassData$TITLE) #Replace : with period
  ClassData$TITLE[ClassData$TITLE == "Editor-in-Chief"] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "Editor-In-Chief"] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "Natural.HistoryEditor"] <- "Natural.History.Editor"
  ClassData$TITLE[ClassData$TITLE == "Deputy.Editor-In-Chief"] <- "Deputy.EIC"
  ClassData$TITLE[ClassData$TITLE == "Deputy.Editor.in.Chief"] <- "Deputy.EIC"
  ClassData$TITLE[ClassData$TITLE == "Editor-in-Chief"] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "SPECIAL.EDITORS"] <- "Special.Editors"
  ClassData$TITLE[ClassData$TITLE == "editor"] <- "Editor"
  ClassData$TITLE[ClassData$TITLE == "guest.editor"] <- "Guest.Editor"
  ClassData$TITLE[ClassData$TITLE == "Associate.Editors"] <- "Associate.Editor"
  ClassData$TITLE[ClassData$TITLE == "EIC "] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "Acting.director.in.chief"] <- "Acting.Director.In.Chief"
  ClassData$TITLE[ClassData$TITLE == "Co-Editor"] <- "CoEditor"
  ClassData$TITLE[ClassData$TITLE == "Deputy.director.in.chief"] <- "Deputy.Director.In.Chief"
  ClassData$TITLE[ClassData$TITLE == "editor"] <- "Editor"
  ClassData$TITLE[ClassData$TITLE == "Editor.-.Executive.Editor"] <- "Editor-Executive.Editor"
  ClassData$TITLE[ClassData$TITLE == "Editorial.board"] <- "Editorial.Board"
  ClassData$TITLE[ClassData$TITLE == "Editors"] <- "Editor"
  ClassData$TITLE[ClassData$TITLE == "Executive.Sevretary"] <- "Executive.Secretary"
  ClassData$TITLE[ClassData$TITLE == "Production.editor"] <- "Production.Editor"
  ClassData$TITLE[ClassData$TITLE == "ReviewsEditor"] <- "Reviews.Editor"
  ClassData$TITLE[ClassData$TITLE == "editorial.review.board"] <- "Editorial.Review.Board"
  ClassData$TITLE[ClassData$TITLE == "EditorialBoard"] <- "Editorial.Board"
  ClassData$TITLE[ClassData$TITLE == "HandlingEditor"] <- "Handling.Editor"
  ClassData$TITLE[ClassData$TITLE == "Section.Editors..Environment"] <- "Section.Editors.Environment"
  ClassData$TITLE[ClassData$TITLE == "Section.Editors..Physiology.&.Development"] <- "Section.Editors.Physiology.&.Development"
  ClassData$TITLE[ClassData$TITLE == "Tansley.review.Editor"] <- "Tansley.Review.Editor"
  ClassData$TITLE[ClassData$TITLE == "associate.editor"] <- "Associate.Editor"
  ClassData$TITLE[ClassData$TITLE == "Deputy.editor.in.chief"] <- "Deputy.EIC"
  ClassData$TITLE[ClassData$TITLE == "Editor.In.Chief"] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "Editorial.board"] <- "Editorial.Board"
  ClassData$TITLE[ClassData$TITLE == "Production.editor"] <- "Production.Editor"
  ClassData$TITLE[ClassData$TITLE == "ReviewsEditor"] <- "Reviews.Editor"
  ClassData$TITLE[ClassData$TITLE == "Review.Editor"] <- "Reviews.Editor"
  ClassData$TITLE[ClassData$TITLE == "Section.Editor.Physiology.&.Development"] <- "Section.Editor.Physiology.Development"
  ClassData$TITLE[ClassData$TITLE == "Section.Editors..Function"] <- "Section.Editors.Function"
  ClassData$TITLE[ClassData$TITLE == "Sections.Editor"] <- "Section.Editor"
  ClassData$TITLE[ClassData$TITLE == "Subject.Editors"] <- "Subject.Editor"
  ClassData$TITLE[ClassData$TITLE == "Editor.in.chief"] <- "EIC"
  ClassData$TITLE[ClassData$TITLE == "TE"] <- "Technical.Editor"
  ClassData$TITLE[ClassData$TITLE == "WA"] <- "Website.Administrator"
  ClassData$TITLE[ClassData$TITLE == "JPS"] <- "Journal.Production.Supervisor"
  ClassData$TITLE[ClassData$TITLE == "JES"] <- "Journal.Editorial.Supervisor"
  ClassData$TITLE[ClassData$TITLE == "JS"] <- "Journal.Supervisor"
  ClassData$TITLE[ClassData$TITLE == "PE"] <- "Production.Editor"
  ClassData$TITLE[ClassData$TITLE == "ME"] <- "Managing.Editor"
  ClassData$TITLE[ClassData$TITLE == "PD"] <- "Publications.Director"
  ClassData$TITLE[ClassData$TITLE == "BRE"] <- "Book.Review.Editor"


  

  
  ClassData$CATEGORY<-as.character(ClassData$CATEGORY)
  
  ClassData$CATEGORY[ClassData$TITLE == "Editor" & ClassData$JOURNAL=="JBIOG"] <- "SE"   # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Senior.Editor" & ClassData$JOURNAL=="JBIOG"] <- "AE" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Associate.Editor" & ClassData$JOURNAL=="JBIOG"] <- "SE" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Journal.Editorial.Supervisor"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Production.Editor"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Publications.Director"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Book.Review.Editor"] <- "special"
  ClassData$CATEGORY[ClassData$TITLE == "Reviews.Editor"] <- "SE"
  ClassData$CATEGORY[ClassData$TITLE == "Production.Staff"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Editorial.Assistant"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Editorial.Office.Manager"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Production.Editor"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Technical.Editor"] <- "production"
  ClassData$CATEGORY[ClassData$TITLE == "Editorial.Board" & ClassData$JOURNAL == "AMNAT" ] <- "SE"
  ClassData$CATEGORY[ClassData$TITLE == "Special.Editor" & ClassData$JOURNAL=="EVOL"] <- "special" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Academic.Associate" & ClassData$JOURNAL=="EVOL"] <- "2xCheck" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Advisory.Panel" & ClassData$JOURNAL=="OIKOS"] <- "2xCheck" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Publication.Board" & ClassData$JOURNAL=="OIKOS"] <- "2xCheck" # NEED to 2x
  ClassData$CATEGORY[ClassData$CATEGORY == "" & ClassData$JOURNAL=="EVOL"] <- "2xCheck" # NEED to 2x
  ClassData$CATEGORY[ClassData$TITLE == "Guest.Editor"] <- "special"
  
  
  #Clean Up Categories
  ClassData$CATEGORY<-as.character(ClassData$CATEGORY)
  ClassData$CATEGORY[ClassData$CATEGORY == "Ae"] <- "AE"
  ClassData$CATEGORY[ClassData$CATEGORY == "OTHER"] <- "other"
  ClassData$CATEGORY[ClassData$CATEGORY == "Other"] <- "other"
  ClassData$CATEGORY[ClassData$CATEGORY == "Other "] <- "other"
  ClassData$CATEGORY[ClassData$CATEGORY == "EDITOR-IN-CHIEF"] <- "EIC"
  ClassData$CATEGORY[ClassData$CATEGORY == "SE "] <- "SE"
  ClassData$CATEGORY[ClassData$CATEGORY == "EIC "] <- "EIC"
  ClassData$CATEGORY[ClassData$CATEGORY == "None"] <- "none"
  ClassData$CATEGORY[ClassData$CATEGORY == "SPECIAL"] <- "special"
  ClassData$CATEGORY[ClassData$CATEGORY == "Special"] <- "special"
  ClassData$CATEGORY[ClassData$CATEGORY == "Production editor"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "Production Staff"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "Journal Supervisor"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "JPS"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "JS"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "PE"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "Productioneditor"] <- "production"
  ClassData$CATEGORY[ClassData$CATEGORY == "other"] <- "production"
  
  #make  special->SPECIAL to match with CHO
  ClassData$CATEGORY[ClassData$CATEGORY == "special"] <- "SPECIAL"
  
  # Convert to factor and drop levels
  ClassData$TITLE<-as.factor(ClassData$TITLE)
  ClassData$TITLE<-droplevels(ClassData$TITLE)
  ClassData$CATEGORY<-as.factor(ClassData$CATEGORY)
  ClassData$CATEGORY<-droplevels(ClassData$CATEGORY)
  # levels(ClassData$CATEGORY)
  # levels(ClassData$TITLE) 

  
  
  
  # Make Gender Consistent
  ClassData$GENDER[ClassData$GENDER == "F"] <- "female"
  ClassData$GENDER[ClassData$GENDER == "M"] <- "male"
  ClassData$GENDER[ClassData$GENDER == "U"] <- NA
  ClassData$GENDER[ClassData$GENDER == "Unkown"] <- NA
  ClassData$GENDER[ClassData$GENDER == "Unknown"] <- NA
  ClassData$GENDER[ClassData$GENDER == ""] <- NA
  ClassData$GENDER<-droplevels(ClassData$GENDER)
  

  # Correct some of the countries
  ClassData$COUNTRY[ClassData$COUNTRY == "Austrailia"]  <- "Australia" #removing old names
  
  
  
  # Adding combinations of names to the database
  # First Name, Last Name
  ClassData$FirstLast<-paste(ClassData$FIRST_NAME,ClassData$LAST_NAME, sep=" ") 
  # First Name, Middle Name, Last Name
  ClassData$FirstMiddleLast<-paste(ClassData$FIRST_NAME,ClassData$MIDDLE_NAME,ClassData$LAST_NAME, sep=" ")
  # First initial 1st name + last name": 
  ClassData$FIRST_INIT<-as.character(ClassData$FIRST_NAME)
  ClassData$FIRST_INIT<-substring(ClassData$FIRST_INIT,1,1)
  ClassData$FirstInitialLast<-paste(ClassData$FIRST_INIT,ClassData$LAST_NAME, sep=" ")
  ClassData$FIRST_INIT<-NULL #delete it out now that we don't need it
  #Delete column with suffix
  ClassData$SUFFIX<-NULL #delete it out now that we don't need it
  
  
  # Remove the periods from peoples names to make consistent accross all files
  ClassData$FirstLast<-gsub("  ", " ", ClassData$FirstLast)
  ClassData$FirstMiddleLast<-gsub("  ", " ", ClassData$FirstMiddleLast)
  ClassData$FirstInitialLast<-gsub("  ", " ", ClassData$FirstInitialLast)
  
  # 
  ClassData_clean<-ClassData
  return(ClassData_clean)
  
}
