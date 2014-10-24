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

#set Working Depositoty
setwd("C:/Users/Lokus/Dropbox/Master Thesis/GitHub-Repo/UrbanTerror")

#Load the Global Terrorism Database
rawGTD <- read.csv("globalterrorismdb_0814dist.csv", header=TRUE)

#The Global Terror Database (GTD) we are using for our analysis contains over a 120k observations on more than 120 variables. 
#The complete database contains redundant or dispensable data for our analysis.
#We therefore filter the database to make it fit our needs. We erase over a 100 variables. 
#We only want to look at successfull terror attacks and include basic data on time, location and target.  

GTD <- subset(rawGTD, select = c(eventid, iyear, imonth, iday, country, region, attacktype1, targtype1, targsubtype1,
                                  propextent, nkill, nwound),iyear >= 1970 & success == 1, na.strings = c("", " "))

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

TUPforPlot <- recode(TUPforPlot, "1 = '0 - Military and Farmland'; 2 = '2 - Local Governanceand Police'; 
                     3 = '5 - Potentially Urban Workplace'; 4 = '7 - Potentially Urban Infrastructure'; 5
                     = '9 - Potentially Expressions of Urban Life'", as.numeric.result=FALSE)


#prepare a data frame for plotting
Plotframe <- data.frame(year=GTD$iyear, TUP=TUPforPlot, PROP=PROPforPlot, HUM=GTD$HUMscale)
  
#as we want to get a sense if different collection issues merged in the GTD, we will splitt plots with 
# horizontal lines that indicate an transition on collecting entity (PIGS, CETIS, ISVG, START)
TUP_intersepz=data.frame(date=as.numeric(c("27", "36", "39")), event=c("PGIS Data Collection End", "CETIS Data Collection End", "ISVG Data Collection End"))

# the first plot hows the count of attack on our categoriesed targets
qplot(factor(year), data=Plotframe, geom = "freqpoly", color = TUP, group = TUP) + geom_vline(data=TUP_intersepz, mapping=aes(xintercept=date))

# the first plot hows the count of attack on our categoriesed targets
qplot(factor(year), data=Plotframe, geom = "freqpoly", color = PROP, group = PROP) + geom_vline(data=TUP_intersepz, mapping=aes(xintercept=date))







