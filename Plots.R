# MPP-E1180: Introduction to Collaborative Social Science Data Analysis
### Fall 2014
### Instructor: Christopher Gandrud


################################
########  MA  Proposal  ########
################################
######### URBAN TERROR #########
################################
###Lukas.B Cameron.R Sascha.S###
################################
# Getting a sense for the data #
################################

#Load basic R packages
library(foreign)
library(car)
library(ggplot2)
library(RCurl)

#download a limited GTD we created for this assignment (5MB instead of 100MB) file 
GTD_in_Memory <- getURL("https://rawgit.com/SaschaSchuster/CSSDA_Assignment2_UrbanTerror/master/GTD.csv", ssl.verifypeer=0L, followlocation=1L)
writeLines(GTD_in_Memory,'GTD.csv')

#Load the Global Terrorism Database
rawGTD <- read.csv("GTD.csv", header=TRUE)

#Load the Global Terrorism Database
rawGTD <- read.csv("GTD.csv", header=TRUE)

#The Global Terror Database (GTD) we are using for our analysis contains over a 120k observations on more than 120 variables.
#The complete database contains redundant or dispensable data for our analysis.
#We therefore filter the database to make it fit our needs. We erase over a 100 variables.
#We only want to look at successfull terror attacks and include basic data on time, location and target.

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, attacktype1, targtype1, targsubtype1,
                                  propextent, nkill, nwound),iyear >= 1970, na.strings = c("", " "))

### We introduce our first scale: "Targets Urbanity Potential Scale (TUPscale)"
GTD["TUPscale"] <- GTD$targsubtype1
GTD$TUPscale <- recode(GTD$TUPscale, "40:42 = 3; 9 = 1; 27:35 = 1; 37:39 = 1; 65 = 1; 72 = 1; 1 = 2; 4:5 = 2; 10 = 2;
                       12 = 2; 53:56 = 2; 58:59 = 2; 61:62 = 2; 82 = 2; 95:96 = 2;6 = 3; 13 = 3;
                       104:108 = 3; 51:52 = 3; 57 = 3; 60 = 3; 63:64 = 3; 73 = 3; 80:81 = 3; 88:92 = 3;
                       98 = 3; 2 = 4; 3 = 4; 7:8 = 4; 44 = 4; 48:50 = 4; 67:71 = 4; 74:79 = 4; 83:87 = 4;
                       97 = 4; 99 = 4; 14:26 = 5; 100:103 = 5; 111 = 5; 109 = 5; 110 = 5; 36 = 5; 43 = 5;
                       45:47 = 5; 66 = 5; 93:94 = 5; 11 = 5", as.numeric.result=TRUE)


### We introduce our second scale: "Extent of Property Damage (PROPscale)"
GTD["PROPscale"] <- GTD$propextent
GTD$PROPscale <- as.numeric(GTD$PROPscale)

### We introduce our second scale: "Extent of Human Damage (HUMscale)" which adds wounded and killed
GTD["HUMscale"] <- GTD$nkill+GTD$nwound
GTD$HUMscale <- as.numeric(GTD$HUMscale)

#Bring down to self explanatory values for the Plot
PROPforPlot <- recode(GTD$PROPscale, "1= '> Billion $'; 2= '> Million $'; 3='< Million $'; 4='no economic damage'; NA=NA; 0=NA")

# In oder to use it in the later charts we recode from the arbitrary 1:5 to a new arbitrary scale that indicates
# a level of urbanity alreadey. The new element is introduced to become part of a dataframe for plotting.

TUPforPlot <- recode(GTD$TUPscale, "1 = '0 - Military and Farmland'; 2 = '2 - Local Governanceand Police';
                     3 = '5 - Potentially Urban Workplace'; 4 = '7 - Potentially Urban Infrastructure'; 5
                     = '9 - Potentially Expressions of Urban Life'", as.numeric.result=FALSE)


#prepare a data frame for plotting
Plotframe <- data.frame(year=GTD$iyear, TUP=TUPforPlot, PROP=PROPforPlot, HUM=GTD$HUMscale)

#as we want to get a sense if different collection issues merged in the GTD, we will split plots with
# horizontal lines that indicate an transition on collecting entity (PIGS, CETIS, ISVG, START)
intersepz=data.frame(date=as.numeric(c("27", "38", "41")), date2=as.numeric(c("1997", "2009", "2011")), event=c("PGIS Data Collection End", "CETIS Data Collection End", "ISVG Data Collection End"))

# the first plot shows the count of attack on our categoriesed targets
PlotTUP <- qplot(factor(year), data=Plotframe, geom = "freqpoly", color = TUP, group = TUP, ylab= "sum of attacks", xlab= "year",
main="Sum of attacks per year
with indication for the transition of collecting entities:
PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz, 
                                                                                       mapping=aes(xintercept=date))
PlotTUP + theme(axis.text.x = element_text(angle = 90), complete = TRUE)


# the second plot shows the count of attack in different categories of economic damage
PlotPROP <- qplot(factor(year), data=Plotframe, geom = "freqpoly", color = PROP, group = PROP, ylab= "sum of attacks", xlab= "year",
main="Sum of attacks per year
with indication for the transition of collecting entities:
PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz,
                                                                                       mapping=aes(xintercept=date))
PlotPROP + theme(axis.text.x = element_text(angle = 90), complete = TRUE)


# the third plot shows the sum of killed and injured of attacks per year
qplot(year, HUM, data=Plotframe, stat="summary", fun.y="sum", geom ="line", 
ylab= "sum of deaths and injured", main="Sum of casualties per year 
with indication for the transition of collecting entities:
PIGS until 1997, CETIS until 2008, ISVG until 2011 and START since 2011") + geom_vline(data=intersepz, 
                                                                                       mapping=aes(xintercept=date2))
