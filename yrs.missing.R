yrs.missing <- function(A,B,C) {
  
  filteryrs<-c(seq(B,C, by=1))
  A<-A %>%
    select(JOURNAL, YEAR)  %>% 
    filter(YEAR%in% filteryrs)
  A$YEAR<-as.factor(A$YEAR)
  A$YEAR<-droplevels(A$YEAR)
  # str(A)
  # summary(A)
  summary_table<-as.data.frame(table(A$YEAR, A$JOURNAL))
  missing_yrs<-filter(summary_table,Freq<1) %>% #Filters the summary table to include years for whihc zero records
    filter(Var2!="AGRONOMY") %>%  # Eliminates the ones that are extnsions of Cho et al data
    filter(Var2!="AREES") %>% 
    filter(Var2!="BIOCON")%>% 
    filter(Var2!="BITR")%>% 
    filter(Var2!="JTE")%>% 
    filter(Var2!="NAJFM")%>% 
    filter(Var2!="") # Eliminates any missing journal

  #STILL TRYING TO FIGURE OUT HOW TO SAVE THE CSV WITH THE CORRECT FILE PATH & NAME
#   final <- list(missing_yrs,write.csv(missing_yrs, 
#                                       (file="/Users/emiliobruna/Dropbox/EMB - ACTIVE/MANUSCRIPTS/Editorial Board Geography/",paste0("missing_yrs", A,"_",B,"_",C,".csv"), row.names=T)))
# return(final)
  return(missing_yrs)
  
  }
