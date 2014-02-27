rm(list=ls()) # to remove all objects and clean the workspace
library(XML)
url <- 'http://en.wikipedia.org/wiki/States_and_union_territories_of_India'
tab <- readHTMLTable(url)
states<-tab[["States of India"]]
subS <- states[,c(2,4,5,10)]
ut<-tab[["Union Territories"]]
subU <- ut[6,c(2,4,8)]
row.names(subU) <- NULL

subU$Name <- gsub('National Capital Territory of Delhi', 'Delhi', subU$Name)
colnames(subU) <- c('state', 'population','literacyRate')

levels(subS$Name)[20]<-"Orissa"
colnames(subS) <- c('state', 'population','area', 'literacyRate')
library(plyr)
merge <- rbind.fill(subS,subU)
merge$area <- gsub(',','',merge$area)
merge$population <- gsub(',','',merge$population)
str(merge)
merge$state <- as.factor(merge$state)
merge$population <- as.numeric(as.character(merge$population))
merge$area <- as.numeric(as.character(merge$area))
merge$literacyRate <- as.numeric(as.character(merge$literacyRate))
str(merge)
merge[29,3] <- 1043
####################################
library(scales)
popPerc <- percent(merge$population/1270272105)
popPerc <- gsub('%', '', popPerc)
areaPerc <- percent(merge$area/2973190)
areaPerc <- gsub('%', '', areaPerc)
dfPerc <- data.frame(merge[,c(1,4)], popPerc, areaPerc)
gsub('%', '', dfPerc)
dfPerc$literacyRate <- round(dfPerc$literacyRate,1)
str(dfPerc)
dfPerc$popPerc <- as.numeric(as.character(dfPerc$popPerc))
dfPerc$areaPerc <- as.numeric(as.character(dfPerc$areaPerc))
library(plyr)
dfPerc <- arrange(dfPerc, state)
save(dfPerc, file='data/dfPerc.rda')

# Load Crime% data
load('data/df2012CrimeP.rda')

data <- cbind(df2012CrimeP, dfPerc)
data <- data[,-3]

f_IlltRate <- function(x) 100-x
data <- ddply(data, .(state), transform, illteracyRate=f_IlltRate(literacyRate))
data <- data[,-3]
library(ggplot2)
m <- melt(data, id.vars='state')
# Plot - various stats for each state - Crime%, Population%, Area%, Illetracy%
plot5 <- ggplot(m, aes(x=reorder(state,value), y=value, fill=variable)) + geom_bar(stat='identity',width=.6) + coord_flip() + theme_bw() + xlab('States') + ylab('%')
plot5 <- plot5 + theme(axis.text=element_text(size=12,face="bold"),plot.title = element_text(lineheight=.8, face="bold")) + ggtitle('Crime Vs Population/Area/IlletracyRate') +theme(plot.background=element_rect(fill="#FFF9AB"),panel.background=element_rect(fill='#4C2E11'),panel.grid.minor = element_blank(),panel.grid.major=element_blank())+ theme(axis.text=element_text(size=8,face="bold"),plot.title = element_text(lineheight=.8, face="bold"))
plot5
#######################
library(corrplot)
# Corerelation plot for Crime%, Population%, Area%, Illetracy%
corrmatrix<-cor(data[,-1])
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA")) #Color scheme, verbatim from the R Graphics cookbook - Chang
corrplot(corrmatrix, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", order="AOE")
