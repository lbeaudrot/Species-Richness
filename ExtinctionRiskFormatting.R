#ctdata <- f.teamdb.query("camera trap")
#load(ctdata)
#alldata <- ctdata

#alldata<-f.fix.data2(alldata)
#Site.Code <- substr(alldata$Sampling.Unit.Name,4,6)
#alldata <- cbind(alldata, Site.Code)

# Set threshold to one day (24 hours * 60 minutes = 1440 minutes) and the number of replicates for each camera trap as 30 days
# Note that f.separate.events takes a good bit of time to run on complete CT dataset. Avoid replicating where possible.
#alldata <- f.order.data(alldata)
#eventsdata <- f.separate.events(alldata, thresh=(1440))
#allevents <- f.separate.events(alldata, thresh=(1))


Pecari <- subset(eventsdata, Site.Code=="VB-" & bin=="Pecari tajacu")
Dasyprocta <- subset(eventsdata, Site.Code=="VB-" & bin=="Dasyprocta punctata")

species <- Pecari

X <- table(species$Sampling.Unit.Name, species$Photo.Date)
X <- as.matrix(ifelse(X>0,1,0))


ELEV <- read.csv("CT_edgedist_elevation_final.csv")
ELEVsub <- ELEV[match(rownames(X), ELEV$Sampling.Unit.Name),]

X <- cbind(Elevation=ELEVsub$Elevation, EdgeDist=ELEVsub$EdgeDist, X)

#write.csv(X, file="VolcanBarva_Pecari.tajacu.csv")
#write.csv(X, file="VolcanBarva_Dasyprocta.punctata.csv")

#Next step is to include the camera traps and dates with no observations (above table only includes obs)
#Also, decide whether to collapse the number of columns down into Day1...Day30 rather than keeping dates
#for now, use same approach to control for the variation in # of days as used in carbon policy paper

test <- f.matrix.creator2(alldata,"2008.01")




################## Camera Trap Temperature Data Formatting ####################
#Examine distribution of NA temperature values across camera traps
table(eventsdata$Sampling.Unit.Name, is.na(eventsdata$Temperature)==TRUE)


library(stringr)
eventsdata$Temperature <- str_trim(eventsdata$Temperature, side="both")
temp.degrees <- sub(" .*","", eventsdata$Temperature)
temp.unit <- str_sub(eventsdata$Temperature, nchar(eventsdata$Temperature), nchar(eventsdata$Temperature))
temp.unit <- str_trim(temp.unit, side="both")
#Convert F temperature values to Celcius
#Change C Temperatures to numeric format from character
temp.degreesC <- as.numeric(ifelse(temp.unit=="F", f.FtoC(as.numeric(temp.degrees)), temp.degrees))
eventsdata <- cbind(eventsdata, temp.degreesC)

#eventsdata$Temperature <- factor(eventsdata$Temperature)
#strsplit(eventsdata$Temperature, "[ ]")
#unlist(strsplit(eventsdata$Temperature, " ", fixed = TRUE))
#temp.degrees <- substr(eventsdata$Temperature,1,3)
#temp.unit <- substr(eventsdata$Temperature,4,5)





#Then, determine the annual min, max and variance of the non-calibrated temperature data for each CT

Temp.Min <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=min)
names(Temp.Min) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Min")
Temp.Max <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=max)
names(Temp.Max) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Max")
Temp.Var <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=var)
names(Temp.Var) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Var")

CT.Temp <- cbind(Temp.Min, Temp.Max$Temp.Max, Temp.Var$Temp.Var)
names(CT.Temp) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Min", "Temp.Max", "Temp.Var")
