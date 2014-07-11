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

species <- Dasyprocta

X <- table(species$Sampling.Unit.Name, species$Photo.Date)
X <- as.matrix(ifelse(X>0,1,0))


ELEV <- read.csv("CT_edgedist_elevation_final.csv")
ELEVsub <- ELEV[match(rownames(X), ELEV$Sampling.Unit.Name),]

X <- cbind(Elevation=ELEVsub$Elevation, EdgeDist=ELEVsub$EdgeDist, X)


write.csv(X, file="VolcanBarva_Pecari.tajacu.csv")
write.csv(X, file="VolcanBarva_Dasyprocta.punctata.csv")

