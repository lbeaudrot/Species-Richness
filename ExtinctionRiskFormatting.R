library(lubridate)
library(reshape)

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


CT.dates <- unique(eventsdata$Photo.Date[eventsdata$Site.Code=="VB-"])
CT.names <- unique(eventsdata$Sampling.Unit.Name[eventsdata$Site.Code=="VB-"])

empty <- matrix(999, length(CT.names), length(CT.dates))
rownames(empty) <- CT.names
colnames(empty) <- Dates(CT.dates)

# Code from EstimateSpeciesRichness.R to determine # sampling days per CT
# Control for the number of sampling day per camera trap
#effortdays <- as.matrix(cbind(data.use$Sampling.Unit.Name, f.start.minus.end(data.use)))
#effortdays <- as.matrix(unique(effortdays))
#effortdays <- effortdays[match(colnames(X), effortdays[,1]),]
#edays <- as.numeric(effortdays[,2])
#X <- round(t(t(X)*(edays/30)))




f.matrix.creatorLB<-function(data,year){
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  cams<-unique(data$Sampling.Unit.Name)
  cams<-sort(cams)
  rows<-length(cams)
  species<-unique(data$bin)
  #start and end dates of sampling periods
  data<-data[data$Sampling.Period==year,]
  min<-min(data$Start.Date)
  max<-max(data$End.Date)
  cols<-max-min+1
  
  #sampling period
  date.header<-seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$Start.Date),data$Sampling.Unit.Name,unique)
  nms<-names(start.dates)
  start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  end.dates<-tapply(as.character(data$End.Date),data$Sampling.Unit.Name,unique)
  end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==start.dates[j])
    hi<-which(date.header==end.dates[j])
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[names(start.dates)[j],indx]<-0
    } else next
  }
  mat.template<-mat
  #get the species
  #species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$Photo.Date[indx]
    cameras<-data$Sampling.Unit.Name[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}


Matrix2008 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2007.01")
Matrix2009 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2008.01")
Matrix2010 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2009.01")
Matrix2011 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2010.01")
Matrix2012 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2011.01")
Matrix2013 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2012.01")
Matrix2014 <- f.matrix.creatorLB(eventsdata[eventsdata$Site.Code=="VB-",], "2013.01")

Pecari.2008 <- Matrix2008[[1]]
Pecari.2009 <- Matrix2009[[1]]
Pecari.2010 <- Matrix2010[[1]]
Pecari.2011 <- Matrix2011[[1]]
Pecari.2012 <- Matrix2012[[1]]
Pecari.2013 <- Matrix2013[[1]]
Pecari.2014 <- Matrix2014[[1]]


Pecari.2008c <- rbind(Pecari.2008[1:20,2:31], Pecari.2008[21:40,37:66], Pecari.2008[41:60,80:109])
colnames(Pecari.2008c) <- paste0(rep("2008.Day",30),1:30)
Pecari.2009c <- rbind(Pecari.2009[1:20,2:31], Pecari.2009[21:40,37:66], Pecari.2009[41:60,80:109])
colnames(Pecari.2009c) <- paste0(rep("2009.Day",30),1:30)
Pecari.2010c <- rbind(Pecari.2010[1:20,2:31], Pecari.2010[21:40,37:66], Pecari.2010[41:60,72:101])
colnames(Pecari.2010c) <- paste0(rep("2010.Day",30),1:30)
Pecari.2011c <- rbind(Pecari.2011[1:20,2:31], Pecari.2011[21:40,37:66], Pecari.2011[41:60,72:101])
colnames(Pecari.2011c) <- paste0(rep("2011.Day",30),1:30)
Pecari.2012c <- rbind(Pecari.2012[1:20,2:31], Pecari.2012[21:40,34:63], Pecari.2012[41:60,72:101])
colnames(Pecari.2012c) <- paste0(rep("2012.Day",30),1:30)
Pecari.2013c <- rbind(Pecari.2013[1:20,2:31], Pecari.2013[21:40,33:62], Pecari.2013[41:60,71:100])
colnames(Pecari.2013c) <- paste0(rep("2013.Day",30),1:30)
Pecari.2014c <- rbind(Pecari.2014[1:20,2:31], Pecari.2014[21:40,40:69], Pecari.2014[41:60,71:100])
colnames(Pecari.2014c) <- paste0(rep("2014.Day",30),1:30)

Pecari_allyears <- cbind(Pecari.2008c, Pecari.2009c, Pecari.2010c, Pecari.2011c, Pecari.2012c, Pecari.2013c, Pecari.2014c)










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

# Clean temperature data and convert F measurements to C
library(stringr)
eventsdata$Temperature <- str_trim(eventsdata$Temperature, side="both")
temp.degrees <- sub(" .*","", eventsdata$Temperature)
temp.unit <- str_sub(eventsdata$Temperature, nchar(eventsdata$Temperature), nchar(eventsdata$Temperature))
temp.unit <- str_trim(temp.unit, side="both")
#Convert F temperature values to Celcius
#Change C Temperatures to numeric format from character
temp.degreesC <- as.numeric(ifelse(temp.unit=="F", f.FtoC(as.numeric(temp.degrees)), temp.degrees))
eventsdata <- cbind(eventsdata, temp.degreesC)


# Determine the annual min, max and variance of the non-calibrated temperature data for each CT

Temp.Min <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=min)
names(Temp.Min) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Min")
Temp.Max <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=max)
names(Temp.Max) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Max")
Temp.Var <- aggregate(eventsdata$temp.degreesC ~ eventsdata$Site.Code + eventsdata$Sampling.Unit.Name + eventsdata$Sampling.Period, FUN=var)
names(Temp.Var) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Var")

CT.Temp <- cbind(Temp.Min, Temp.Max$Temp.Max, Temp.Var$Temp.Var)
names(CT.Temp) <- c("Site.Code", "Sampling.Unit.Name", "Sampling.Period", "Temp.Min", "Temp.Max", "Temp.Var")

CT.Temp.VB <- melt(CT.Temp[CT.Temp$Site.Code=="VB-",])
Tmin <- as.data.frame(cast(CT.Temp.VB, Sampling.Unit.Name ~ Sampling.Period ~ variable)[,,1])
names(Tmin) <- paste0("Tmin.",2008:2014)
Tmax <- as.data.frame(cast(CT.Temp.VB, Sampling.Unit.Name ~ Sampling.Period ~ variable)[,,2])
names(Tmax) <- paste0("Tmax.",2008:2014)
Tvar <- round(as.data.frame(cast(CT.Temp.VB, Sampling.Unit.Name ~ Sampling.Period ~ variable)[,,3]),2)
names(Tvar) <- paste0("Tvar.",2008:2014)

ELEV <- read.csv("CT_edgedist_elevation_final.csv")
ELEVsub <- ELEV[match(rownames(Tmin), ELEV$Sampling.Unit.Name),]

Pecari.Data <- cbind(Elevation=ELEVsub$Elevation, Tmin, Tmax, Tvar, Pecari_allyears)
#write.csv(Pecari.Data, file="VB_allyears.temp_Pecari.tajacu.csv")


