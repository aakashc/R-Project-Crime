rm(list=ls())

# LIbrary to read Excel File
library('XLConnect')

# Read columns from the file
cities <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='E84:G136', header=F)
cities[is.na(cities)] <- ''
cities <- paste0(cities$Col1, cities$Col2, cities$Col3)
cities <- gsub('\\(C.*\\)','',cities)
cities <- gsub('-','',cities)
cities <- gsub('VIJAYAW','VIJAYAWADA',cities)

femalePop <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='M84:M136', header=F)

rape <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='N84:N136', header=F)
kidnapping <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='Q84:Q136', header=F)
insult <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='M226:M278', header=F)
immoral <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='I369:I421', header=F)
dowry <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='Q369:Q421', header=F)
dowry_deaths <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='V84:V136', header=F)
cruelty <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='Y84:Y136', header=F)
assult <- readWorksheetFromFile('data/city_crime.xls', sheet='Sheet1', region='J226:J278', header=F)

cdata <- data.frame(cities,femalePop,rape,kidnapping,insult,immoral,dowry,dowry_deaths,cruelty,assult)
names(cdata)[2:10] <- c('femalePop','rape','kidnapping','insult','immoral','dowry','dowry_deaths','cruelty','assult')

# Type Conversion of the columns
str(cdata)
cdata$femalePop <- as.numeric(cdata$femalePop)
cdata$rape <- as.numeric(cdata$rape)
cdata$kidnapping <- as.numeric(cdata$kidnapping)
cdata$insult <- as.numeric(cdata$insult)
cdata$immoral <- as.numeric(cdata$immoral)
cdata$dowry <- as.numeric(cdata$dowry)
cdata$dowry_deaths <- as.numeric(cdata$dowry_deaths)
cdata$cruelty <- as.numeric(cdata$cruelty)
cdata$assult <- as.numeric(cdata$assult)

# Save the file as rda - to be used for Shiny App
dfc <- cdata[,-2]
save(dfc, file='shiny/dfc.rda', compress=F, precheck=F)

# Reading Female population so as to calculate Crime Density
cdata$femalePop <- cdata$femalePop*100000
TotalCrimeCities <- rowSums((cdata[,3:10]))
cdata$TotalCrimeCities <- TotalCrimeCities
crimeDensity <- round((cdata$TotalCrimeCities/cdata$femalePop)*100,3)
cdata$crimeDensity <- crimeDensity

citiesByCrimeDensity <- cdata[order(-cdata$crimeDensity),c(1,11,12)]
row.names(citiesByCrimeDensity) <- NULL
citiesByTotalCrime <- cdata[order(-cdata$TotalCrimeCities),c(1,11,12)]
row.names(citiesByTotalCrime) <- NULL

library(reshape2)
# Melt the dataset
melting <- melt(cdata, id.vars='cities')

library(ggplot2)
# Plot - City Ranks By Total Number of Cases
x <- subset(cdata, select=c(1,11))
m <- melt(x, id.vars='cities')
cityPlot1 <- ggplot(m)+ geom_bar(aes(x=reorder(cities, value),y=value),stat='identity',fill=rownames(m)) +coord_flip() + theme_bw() + xlab('Cities') + ylab('Number of Cases') + ggtitle('City Ranks by Total Number of Cases')
cityPlot1 <- cityPlot1 + theme(plot.background=element_rect(fill="#FFF9AB"),panel.background=element_rect(fill='#4C2E11'),panel.grid.minor = element_blank(),panel.grid.major=element_blank()) + theme(axis.text=element_text(size=6,face="bold"),plot.title = element_text(lineheight=.8, face="bold"))

# Plot - City Ranks By Crime Density
y <- subset(cdata, select=c(1,12))
n <- melt(y, id.vars='cities')
cityPlot2 <- ggplot(n)+ geom_bar(aes(x=reorder(cities, value),y=value), stat='identity',fill=rownames(m)) +coord_flip()+ theme_bw() + xlab('Cities') + ylab('Crime Density') + ggtitle('City Ranks By Crime Density')
cityPlot2 <- cityPlot2 + theme(plot.background=element_rect(fill="#FFF9AB"),panel.background=element_rect(fill='#4C2E11'),panel.grid.minor = element_blank(),panel.grid.major=element_blank())+ theme(axis.text=element_text(size=6,face="bold"),plot.title = element_text(lineheight=.8, face="bold"))
##########
# Plot - Top Cities for each Crime Type
library(sqldf)
crimeTypeMelt <- subset(melting, melting$variable!='femalePop' & melting$variable!='crimeDensity' & melting$variable!='TotalCrimeCities')
query <- sqldf('select cities, variable, MAX(value) as Count from crimeTypeMelt group by variable')
cityPlot3 <- ggplot(query, aes(x=variable, y=Count, fill=cities)) + geom_bar(stat='identity',width=.3) + theme_bw() + xlab('Crime Types') + ylab('Number of Cases') + ggtitle('Top Cities for each Crime Type')
cityPlot3 <- cityPlot3 + theme(plot.background=element_rect(fill="#FFF9AB"),panel.background=element_rect(fill='#4C2E11'),panel.grid.minor = element_blank(),panel.grid.major=element_blank())+ theme(axis.text=element_text(size=8,face="bold"),plot.title = element_text(lineheight=.8, face="bold"))
cityPlot3