# Process Trait Data and Calculate Functional Diversity Metrics
library(FD)

# Connect TEAM master species list to the PanTHERIA mammalian trait data

masterlist<-read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
pantheria <- read.csv("Pantheria_Data_WR05_Aug2008.csv") # PanTHERIA data

#subset TEAM species list from overall PanTHERIA database
matchedlist <- pantheria[match(masterlist$Unique_Name, pantheria$Binomial),]
splist <- cbind(masterlist, matchedlist)

mammals <- splist[splist$Class=="MAMMALIA",]
birds <- splist[splist$Class=="AVES",]
birds <- birds[,1:6]

# Determine the number of species for which data are available for each trait
Nspecies <- vector(mode = "numeric", length=dim(mammals)[2])
for(i in 1:length(Nspecies)){
  Nspecies[i] <- sum(table(mammals[,i]))
}

Ntraits <- cbind(colnames(mammals), Nspecies)

# Subset trait data for which >150 mammal species on TEAM list have data

mammalianTraits <- cbind(mammals[1:4], mammals$AdultHeadBodyLen_mm, mammals$LitterSize, mammals$GR_Area_km2, as.factor(mammals$ActivityCycle), as.factor(mammals$HabitatBreadth), as.factor(mammals$DietBreadth), mammals$Guild)
names(mammalianTraits) <- c("Bin", "Mass", "Class", "Family", "BodyLength", "LitterSize", "GR_Area", "ActivityCycle",  "HabitatBreadth", "DietBreadth", "Guild")

# Use "mammalianTraits" as input data for the f.family.avg function, which requires loading the helper functions from FunctionalTrait_HelperFunctions.R

fam_avg <- f.family.avg(mammalianTraits)
fam_avg <- as.data.frame(fam_avg)

# Apply to bird data
fam_avg_bird <- f.family.avg.bird(birds)
fam_avg_bird <- as.data.frame(fam_avg_bird)

# Fill in missing values using family level averages (mode for factors; median for continuous variables)
# Replace missing values in mammalianTraits with Family-level averages (object "fam_avg") so that they are "corrected" (objects end in "_c")


Mass_c <- ifelse(is.na(mammalianTraits$Mass)==TRUE, fam_avg$Mass_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$Mass)
#BodyLength_c <- ifelse(is.na(mammalianTraits$BodyLength)==TRUE, fam_avg$BodyLength_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$BodyLength)
LitterSize_c <- ifelse(is.na(mammalianTraits$LitterSize)==TRUE, fam_avg$LitterSize_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$LitterSize)
GR_Area_c <- ifelse(is.na(mammalianTraits$GR_Area)==TRUE, fam_avg$GR_Area_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$GR_Area)
ActivityCycle_c <- ifelse(is.na(mammalianTraits$ActivityCycle)==TRUE, fam_avg$ActivityCycle_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$ActivityCycle)
#HabitatBreadth_c <- ifelse(is.na(mammalianTraits$HabitatBreadth)==TRUE, fam_avg$HabitatBreadth_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$HabitatBreadth)
#DietBreadth_c <- ifelse(is.na(mammalianTraits$DietBreadth)==TRUE, fam_avg$DietBreadth_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$DietBreadth)
Guild_c <- ifelse(is.na(mammalianTraits$Guild)==TRUE, fam_avg$Guild_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$Guild)

# Apply to bird data
Mass_c_bird <- ifelse(is.na(birds$Mass)==TRUE, fam_avg_bird$Mass_m[match(birds$Family, rownames(fam_avg_bird))],birds$Mass)
Guild_c_bird <- ifelse(is.na(birds$Guild)==TRUE, fam_avg_bird$Guild_m[match(birds$Family, rownames(fam_avg_bird))],birds$Guild)


# Create data frame out of corrected Trait values to use in functional diversity metrics; coerce factors into factors
#Mtraits <- cbind(Mass_c, BodyLength_c, LitterSize_c, GR_Area_c, HabitatBreadth_c, DietBreadth_c, ActivityCycle_c, Guild_c)

Mtraits <- cbind(Mass_c, LitterSize_c, GR_Area_c, ActivityCycle_c, Guild_c)
rownames(Mtraits) <- mammalianTraits$Bin
Mtraits <- as.data.frame(Mtraits)
#Mtraits$HabitatBreadth_c <- as.factor(Mtraits$HabitatBreadth_c)
#Mtraits$DietBreadth_c <- as.factor(Mtraits$DietBreadth_c)
Mtraits$ActivityCycle_c <- as.factor(Mtraits$ActivityCycle_c)
Mtraits$Guild_c <- as.factor(Mtraits$Guild_c)
str(Mtraits)

# Apply to bird data
Btraits <- cbind(Mass_c_bird, Guild_c_bird, birds$Include)
Btraits <- as.data.frame(Btraits)
Btraits$Guild_c_bird <- as.factor(Btraits$Guild_c_bird)
rownames(Btraits) <- birds$Unique_Name
str(Btraits)



####### PROCESS DATA to subset for various models - time consuming so don't run unless necessary ###########
## NB processing code copied from EstimateSpeciesRichness.R

#ctdata <- f.teamdb.query(dataset=="camera trap")
#load(ctdata)
#alldata <- ctdata

#alldata<-f.fix.data2(alldata)
#Site.Code <- substr(alldata$Sampling.Unit.Name,4,6)
#alldata <- cbind(alldata, Site.Code)

# Set threshold to one minute
# Note that f.separate.events takes a good bit of time to run on complete CT dataset. Avoid replicating where possible.
#alldata <- f.order.data(alldata)
#eventsdata <- f.separate.events(alldata, thresh=(1440))
#allevents <- f.separate.events(alldata, thresh=(1))

table(alldata$Site.Code, alldata$Sampling.Period) #Examine which sites have data for which years


######## Start here to CHANGE INPUT DATA FOR Functional Diversity metrics ###############
# Use  "Site.Code" to designate which site to include
data.use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="YAN",]

# Subset the species list for each site to calculate the functional diversity metrics
splist<-read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist<-unique(data.use$bin) #site list
Msplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
Msplist <- Msplist[Msplist$Class=="MAMMALIA",]
#newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
#subdata<-subset(data.use, data.use$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
#subdata<-f.correct.DF(subdata)

# Apply to bird data 
Bsplist<-subset(Btraits,rownames(Btraits) %in% sitelist & Btraits$V3==1)
BirdTraits <- cbind(Bsplist[,1], Bsplist[,2])
rownames(BirdTraits) <- rownames(Bsplist)
colnames(BirdTraits) <- c("Mass", "Guild")
BirdTraits <- as.data.frame(BirdTraits)
BirdTraits$Guild <- droplevels(as.factor(BirdTraits$Guild))

# Use the subsetted species list for the site to extract the corresponding functional trait data
SiteTraits <- Mtraits[match(Msplist$Unique_Name, rownames(Mtraits)),]
#rownames(SiteTraits) <-paste("sp",1:dim(SiteTraits)[1], sep=".") #Alternate (shorter) species labels
## NB For traits that are factors, all levels must be represented in data to obtain results using dbFD()
#Reclassify SiteTraits list so that only new levels are used
SiteTraits <- cbind(SiteTraits[,1:3], droplevels(SiteTraits[,4:5]))
#SiteTraits

####### Calculate Functional Diversity using the FD package
#Calculate unweighted mammal functional diversity
#traitFD <- dbFD(SiteTraits, corr="cailliez")
#Use object wpi_weights to weight functional trait calculations by occupancy estimates (psi.mean or psi.median)
wpi_use <- wpi_weights[wpi_weights$Site.Code==data.use$Site.Code[1], ]
psi_weights <- wpi_use$psi.median[match(rownames(SiteTraits), wpi_use$bin)]
SiteTraits <- cbind(SiteTraits, psi_weights)
SiteTraits <- na.omit(SiteTraits)
psi_use <- SiteTraits$psi_weights
names(psi_use) <- rownames(SiteTraits)
traitFD_w <- dbFD(SiteTraits, a=psi_use, corr="cailliez", calc.FRic=FALSE)


#BBStraitFD_w <- traitFD_w
#BCItraitFD_w <- traitFD_w
#BIFtraitFD_w <- traitFD_w
#CAXtraitFD_w <- traitFD_w
#COUtraitFD_w <- traitFD_w
#KRPtraitFD_w <- traitFD_w
#MAStraitFD_w <- traitFD_w
#NNNtraitFD_w <- traitFD_w
#PSHtraitFD_w <- traitFD_w
#RNFtraitFD_w <- traitFD_w
#UDZtraitFD_w <- traitFD_w
#VBtraitFD_w <- traitFD_w
#YANtraitFD_w <- traitFD_w
#YAStraitFD_w <- traitFD_w

CTSite_FD <- list(BBStraitFD_w, BCItraitFD_w, BIFtraitFD_w, CAXtraitFD_w, COUtraitFD_w, KRPtraitFD_w, MAStraitFD_w,
  NNNtraitFD_w, PSHtraitFD_w, RNFtraitFD_w, UDZtraitFD_w, VBtraitFD_w, YANtraitFD_w, YAStraitFD_w) 


# Create output table from FD calculations
CT.nbsp <- vector()
CT.sing.sp <- vector()
CT.FEve <- vector()
CT.FDiv <- vector()
CT.FDis <- vector()
CT.RaoQ <- vector()
CWM.Mass <- vector()
CWM.Litter <- vector()
CWM.GR <- vector()
CWM.Activity <- vector()
CWM.Guild <- vector()

for(i in 1:length(CTSite_FD)){
  CT.nbsp[i] <- CTSite_FD[[i]]$nbsp
  CT.sing.sp[i] <- CTSite_FD[[i]]$sing.sp
  CT.FEve[i] <- CTSite_FD[[i]]$FEve
  CT.FDiv[i] <- CTSite_FD[[i]]$FDiv
  CT.FDis[i] <- CTSite_FD[[i]]$FDis
  CT.RaoQ[i] <- CTSite_FD[[i]]$RaoQ
  CWM.Mass[i] <- CTSite_FD[[i]]$CWM$Mass_c
  CWM.Litter[i] <- CTSite_FD[[i]]$CWM$LitterSize_c
  CWM.GR[i] <- CTSite_FD[[i]]$CWM$GR_Area_c
  CWM.Activity[i] <- CTSite_FD[[i]]$CWM$ActivityCycle_c
  CWM.Guild[i] <- CTSite_FD[[i]]$CWM$Guild_c
}

CTweighted <- cbind(CT.nbsp, CT.sing.sp, CT.FEve, CT.FDiv, CT.FDis, CT.RaoQ, CWM.Mass, CWM.Litter, CWM.GR)
CTweighted <- as.data.frame(CTweighted)






birdFD <- dbFD(BirdTraits, corr="cailliez")
birdFD
BirdTraits

# Create name for output for each site for loop
# paste(events.use$Site.Code[1], events.use$Sampling.Period[1], "FD", sep=".")