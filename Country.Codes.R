Country.Codes <- function(DATASET) {
  
  # DATA CLEANUP
  
  #Remove (trim) the leading and trailing white spaces (not can do with one command as per: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  DATASET$COUNTRY<-trim.trailing(DATASET$COUNTRY)
  trim.leading <- function (x)  sub("^\\s+", "", x)
  DATASET$COUNTRY<-trim.leading(DATASET$COUNTRY)
  
  # remove any double spaces
  DATASET$COUNTRY<-gsub("  ", " ", DATASET$COUNTRY, fixed=TRUE)
  
  # USING COUNTRY NAMES AND Package "countrycode" TO ADD A COLUMN WITH THE 3 DIGIT CODE 
  
  library(countrycode)
  
  # ELIMINATE ANY ROWS WITH BLANKS IN THE 
  
  DATASET<-subset(DATASET, COUNTRY!="Unknown")
  
  # Chnage countries as needed, either because country names have changed or to reflect political organization
  
  DATASET$COUNTRY[DATASET$COUNTRY == "Scotland"]  <- "UK" #With apologies to Scots everywhere
  DATASET$COUNTRY[DATASET$COUNTRY == "SCOTLAND"]  <- "UK" #With apologies to Scots everywhere
  DATASET$COUNTRY[DATASET$COUNTRY == "Wales"]  <- "UK"
  DATASET$COUNTRY[DATASET$COUNTRY == "England"]  <- "UK"
  DATASET$COUNTRY[DATASET$COUNTRY == "German Democratic Republic"]  <- "Germany" #removing old names
  DATASET$COUNTRY[DATASET$COUNTRY == "US"]  <- "USA" #in case any snuck in
  
  #step3: change all the country names to the codes used in mapping
  #Add a column with the 3 letter country codes to be consistent with the other datasets
  #Maptools uses the ISO 3166 three letter codes: https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
  #The packahge countrycode will take your column of country names and convert them to ISO3166-3 Codes
  #I began by checking the values of COUNTRY to see if there are any mistakes. To do so I just created a vector 
  
  #called CODECHECK
  
  DATASET$geo.code<-countrycode(DATASET$COUNTRY, "country.name", "iso3c", warn = TRUE)
  #By setting "warn=TRUE" it will tell you which ones it couldn't convert. Because of spelling mistakes, etc.
  DATASET$geo.code<-as.factor(DATASET$geo.code)
  
  
  #Deleting rows without country
  # DATASET <- DATASET[!is.na(DATASET$geo.code),] 
  
  #You can correct these as follows in the dataframe with all the data, then add a new column to the dataframe with the country codes
  
  
  return(DATASET)
  
}
  