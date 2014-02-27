rm(list=ls()) # to remove all objects and clean the workspace
# Read Data
raw <- read.table('data/crimeAgainstWomen.csv', sep=',', check.names=F,header=T,fill=TRUE)
names(raw) <- c('state', 'crimeType', seq(2001,2012,1))
# Remove columns
clean <- raw[,c(1,2,8:14)]
# Remove Rows
clean <- subset(clean,!clean$state=='A & N ISLANDS')
clean <- subset(clean,!clean$state=='D & N HAVELI')
clean <- subset(clean,!clean$state=='PUDUCHERRY')
clean <- subset(clean,!clean$state=='LAKSHADWEEP')
clean <- subset(clean,!clean$state=='DAMAN & DIU')
clean <- subset(clean,!clean$state=='CHANDIGARH')
clean <- subset(clean,!clean$crimeType=='COMMISSION OF SATI PREVENTION ACT')
clean <- subset(clean,!clean$crimeType=='INDECENT REPRESENTATION OF WOMEN (P) ACT')
clean <- subset(clean,!clean$crimeType=='IMPORTATION OF GIRLS FROM FOREIGN COUNTRY')
row.names(clean) <- NULL
# convert to wide format
library(reshape2)
long <- melt(clean, id.vars = c("state", "crimeType"))
wide <- dcast(long, variable+state ~ crimeType)
wide <- wide[,c(2,10:3,1)]
names(wide)[2:10] <- c('rape','kidnapping','insult','immoral','dowry','dowry_deaths','cruelty','assult','year')
names(wide)
str(wide)
wide$rape <- as.numeric(wide$rape)
wide$kidnapping <- as.numeric(wide$kidnapping)
wide$insult <- as.numeric(wide$insult)
wide$immoral <- as.numeric(wide$immoral)
wide$dowry <- as.numeric(wide$dowry)
wide$dowry_deaths <- as.numeric(wide$dowry_deaths)
wide$cruelty <- as.numeric(wide$cruelty)
wide$assult <- as.numeric(wide$assult)
wide$year <- as.numeric(as.character(wide$year))

wide$state <- gsub('ODISHA', 'Orissa', wide$state)

#Function to convert Upper case to Lower
simpleCap <- function(x) { s <- strsplit(x, " ")[[1]] 
                           paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")}

library(plyr)
wide$state <- tolower(wide$state)
wide$state <- sapply(wide$state, simpleCap)

# Save RDA file for 2012 - to be used for Shiny App
dfs <- subset(wide, year==2012)
row.names(dfs) <- NULL
dfs <- dfs[,-10]
save(dfs, file='shiny/dfs.rda', compress=F, precheck=F)
# Calculate Total Crimes
TotalCrimes <- rowSums((wide[,2:9]))
wide$TotalCrimes <- TotalCrimes
wide <- wide[,c(1:9,11,10)]

# Calcluate Total Crimes for each year
TOTAL <- ddply(wide, .(year), function(x) colSums(x[c(2:10)]))
merge <- rbind.fill(wide,TOTAL)
merge$state <- replace(merge$state, is.na(merge$state), "TOTAL")

longNew <- melt(merge, id.vars = c("year", "state"))
names(longNew)[3:4] <- c('crimeType', 'cases') 
###########################33
# Calculate %change of crimes for each state and each year
good<-function(x){((x - x[1])/x[1])*100}
pc06_07 <- ddply(subset(merge, year==2006 | year==2007,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))
pc07_08 <- ddply(subset(merge, year==2007 | year==2008,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))
pc08_09 <- ddply(subset(merge, year==2008 | year==2009,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))
pc09_10 <- ddply(subset(merge, year==2009 | year==2010,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))
pc10_11 <- ddply(subset(merge, year==2010 | year==2011,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))
pc11_12 <- ddply(subset(merge, year==2011 | year==2012,c(1,10:11)), .(state), transform, perchange= round(good(TotalCrimes),2))

x <- cbind(pc06_07,pc07_08,pc08_09,pc09_10,pc10_11,pc11_12)
y <- x[,c(1,3,4,7,8,11,12,15,16,19,20,23,24)]
z <- subset(y, year.5=='2012')
row.names(z) <- NULL
zz <- z[,c(1,3,5,7,9,11,13)]
names(zz)[2:7] <- c(2007:2012)

# Chart for % change in crimes for each state
m <- melt(zz, id.vars='state')
m$state <- gsub('Pradesh', '', m$state)
m$state <- gsub('Jammu & Kashmir', 'J&K', m$state)
m$state <- gsub('West Bengal', 'WB', m$state)
m$state <- gsub('Tamil Nadu', 'TN', m$state)
m$state <- gsub('Tamil Nadu', 'TN', m$state)
m$state <- gsub('Chhattisgarh', 'Chgarh', m$state)

library(rCharts)
p <- nPlot(value ~ state, group = "variable", data =m, type = "multiBarChart", height=500,width=1200)
p$chart(stacked=TRUE)
p$chart(reduceXTicks = FALSE)
p$xAxis(staggerLabels = TRUE)
#p$xAxis(rotateLabels=-90)              
p$save('graph/crimePct.html', cdn = TRUE)

zzz <- melt(zz, id.vars='state')
zzzz<- ddply(melt(zz, id.vars='state'), .(variable), summarise, maxChange = max(value),state=state[which.max(value)])
zzzzz <- ddply(melt(zz, id.vars='state'), .(variable), summarise, minChange = min(value),state=state[which.min(value)])

# States with Maximum Crime Increase in percentage
library(rCharts)
sPlot1 <- nPlot(x = "state", y = "maxChange", group = "variable", data = zzzz, type = "multiBarChart")
sPlot1$save("graph/sPlot1.html",cdn=T)

# States with Maximum Crime Decrease in percentage
sPlot2 <- nPlot(x = "state", y = "minChange", group = "variable", data = zzzzz, type = "multiBarChart")
sPlot2$save("graph/sPlot2.html",cdn=T)
#########################
# Convert to Percentage for all the crimes
rapePct <- round((merge$rape/merge$TotalCrimes)*100,1)
kidnappingPct <- round((merge$kidnapping/merge$TotalCrimes)*100,1)
insultPct <- round((merge$insult/merge$TotalCrimes)*100,1)
immoralPct <- round((merge$immoral/merge$TotalCrimes)*100,1)
dowryPct <- round((merge$dowry/merge$TotalCrimes)*100,1)
dowry_deathsPct <- round((merge$dowry_deaths/merge$TotalCrimes)*100,1)
crueltyPct <- round((merge$cruelty/merge$TotalCrimes)*100,1)
assultPct <- round((merge$assult/merge$TotalCrimes)*100,1)
TotalCrimesPct <- round((merge$TotalCrimes/merge$TotalCrimes)*100,1)

dfPct <- data.frame(rapePct,kidnappingPct,insultPct,immoralPct,dowryPct,dowry_deathsPct,crueltyPct,assultPct,TotalCrimesPct)

# Merging Datasets:
mergedData <- cbind(merge, dfPct)
dfOnlyPct <- mergedData[,c(1,11,12:20)]
dfOnlyCnt <- mergedData[,c(1:11)]
meltPct <- melt(dfOnlyPct, id.vars = c("state", "year"))
meltCnt <- melt(dfOnlyCnt, id.vars = c("state", "year"))
meltCom <- cbind(meltCnt, meltPct$value)
names(meltCom) <- c('state', 'year', 'crimeType', 'count', 'percent')
meltCom$state <- as.factor(meltCom$state)
##############################################
# ArePlot for each crime from year 2006 to year 2012
AreaTotPlot <- nPlot(count~year, group='crimeType', data=subset(meltCom, meltCom$state=="TOTAL"), type="stackedAreaChart")
AreaTotPlot$yAxis(tickFormat = "#!  function(y) { return (y*100).toFixed(0)} !#")
AreaTotPlot$save("graph/AreaTotPlot.html",cdn=T)

# State Map - Crime count for each state
library(googleVis)
stateMap <- gvisGeoChart(data=subset(meltCom, meltCom$crimeType=="TotalCrimes" & meltCom$year==2012 & meltCom$state!='TOTAL'),locationvar="state", colorvar="count",options=list(title="XXX",region='IN',displayMode="region",resolution="provinces",colorAxis="{colors:['grey','yellow','green','blue','orange','red']}",width=600,height=600))
google <- print(stateMap, file="graph/google.html")
##################################################################################
# Find the Top Crime per Year
library(ggplot2)
topCrimeByYr <- ddply(subset(meltCom, meltCom$state!='TOTAL' & meltCom$crimeType!='TotalCrimes'), .(crimeType, year), summarise, maxCrime = max(count),state=state[which.max(count)])
sPlot3 <- ggplot(topCrimeByYr, aes(x = year, y = maxCrime, fill = state)) + geom_bar(stat='identity', position="dodge",width=0.4) + theme_bw() + facet_grid(.~crimeType) + theme(axis.text=element_text(size=7,face="bold"),plot.title = element_text(lineheight=.8, face="bold")) + ggtitle('Top Crime by State and Year')
sPlot3 <- sPlot3 + theme(plot.background=element_rect(fill="#FFF9AB"),panel.background=element_rect(fill='#4C2E11'),panel.grid.minor = element_blank(),panel.grid.major=element_blank())
####################################################################################
# Data Frame Containing data for the year 2012
df2012 <- subset(meltCom, year==2012 & crimeType!='TotalCrimes' & state!='TOTAL')
row.names(df2012) <- NULL
df2012 <- df2012[,c(-2,-4)]
wide2012 <- dcast(df2012, state ~ crimeType )
######################
df2012CrimeP <- subset(meltCom, year==2012 & crimeType=='TotalCrimes')
row.names(df2012CrimeP) <- NULL
df2012CrimeP <- df2012CrimeP[,c(1,4)]
p <- sapply(df2012CrimeP[,2],function(x) ((x/243690)*100))
p <- round(p,1)
df2012CrimeP$perCrime <- p
df2012CrimeP <- df2012CrimeP[-30,c(1,3)] 
save(df2012CrimeP, file='data/df2012CrimeP.rda')
#################################################################
# Correlation Plot
library(scales)
cormat <- cor(dfs[c(-1)])
cm <- melt(cormat)
names(cm)=c("Var1","Var2","CorrelationCoefficient")
cplot = rPlot(Var2 ~ Var1, color = 'CorrelationCoefficient', data = cm, type = 'tile', height = 600)
cplot$addParams(height = 450, width=1000)
cplot$guides("{color: {scale: {type: gradient2, lower: 'red',  middle: 'white', upper: 'blue',midpoint: 0}}}")
cplot$guides(y = list(numticks = length(unique(cm$Var1))))
cplot$guides(x = list(numticks = 8))
cplot$addParams(staggerLabels=TRUE)
cplot$save("graph/corrmatplot.html",cdn=T)
# heatmap of variables and State UTs
hmMelt <- ddply(melt(dfs),.(variable),transform,rescale=rescale(value))
names(hmMelt)=c("state","type","count","rescale")
hmap <- rPlot(state ~ type, color = 'rescale', data = hmMelt, type = 'tile')
hmap$addParams(height = 600, width=800)
hmap$guides(reduceXTicks = FALSE)
hmap$guides("{color: {scale: {type: gradient, lower: 'white', upper: 'red'}}}")
hmap$guides(y = list(numticks = length(unique(hmMelt$state))))
hmap$guides(x = list(numticks = 10))
hmap$save("graph/hmap.html",cdn=T)
# Clustering:
set.seed(200)
kmeansdata <- kmeans(dfs[c(-1)],4)
meanvars <- aggregate(dfs[c(-1)],by=list(kmeansdata$cluster),FUN=mean)
clust <- data.frame(dfs, kmeansdata$cluster)
dPlots <- dPlot(x="state", y="kmeansdata.cluster",groups="kmeansdata.cluster",data=clust,type="bar",width=500,height=800,bounds = list(x=50, y=10, width=400, height=400))
dPlots$yAxis(type="addCategoryAxis")
dPlots$xAxis(type="addCategoryAxis",orderRule="kmeansdata.cluster")
dPlots$save("graph/dPlots.html",cdn=T)
##########################################