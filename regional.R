
AnalysisDivwide<-AnalysisDiv %>% count(JOURNAL, YEAR, divmetric = REGION) %>% spread(divmetric, n)
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

##############################################################
# COUNT OF NO. OF EDITORS AND COUNTRIES EACH BOARD IN EACH YEAR
##############################################################

# Count by how many from each region on each journal board each year 
N_Regions<-AnalysisDivwide %>% gather("REGION", "N_editors", 3:ncol(AnalysisDivwide)) %>% 
  group_by(JOURNAL, YEAR)  
N_Regions<-as.data.frame(N_Regions)



#######################
AnalysisDivwide<-AnalysisDiv %>% count(JOURNAL, YEAR, divmetric = INCOME_LEVEL) %>% spread(divmetric, n)
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
N_Income<-AnalysisDivwide %>% gather("INCOME_LEVEL", "N_editors", 3:ncol(AnalysisDivwide)) %>% 
  group_by(JOURNAL, YEAR)  


TOTALS<-N_Income %>% group_by(JOURNAL, YEAR) %>% summarise(BoardSize=sum(N_editors))
N_Income<-full_join(N_Income, TOTALS, by = c("JOURNAL" = "JOURNAL", "YEAR" = "YEAR")) 
N_Income<-mutate(N_Income, percentage = N_editors/ BoardSize*100)


# PLOT


plotRegionPercent<-ggplot(N_Income, aes(x=YEAR, y=percentage, group=INCOME_LEVEL)) +
  geom_point(shape=1) + # Use hollow circles
  ylab("Percentage of boards From Each Region") +
  xlab("Year")+
  geom_smooth()#Add a loess smoothed fit curve with confidence region (by default includes 95% confidence region)
# plotEDvCountries<-plotEDvCountries + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30))
# plotEDvCountries<-plotEDvCountries + scale_x_continuous(breaks = seq(0, 240, 20), limits = c(0, 230))
plotRegionPercent<-plotRegionPercent+theme_classic()+
  theme(axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 10))                              #sets size and style of labels on axes
        # legend.title = element_blank(),   #Removes the Legend title
        # legend.text = element_text(color="black", size=10),  
        # legend.position = c(0.9,0.8),
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'))
#plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
plotRegionPercent

