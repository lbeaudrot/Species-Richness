# Process Trait Data and Calculate Functional Diversity Metrics
library(FD)

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

table(alldata$Site.Code, alldata$Sampling.Period) #Examine which sites have data for which years

############################## CALCULATE FD FOR INDIVIDUAL SITE ######################
######## Start here to CHANGE INPUT DATA FOR Functional Diversity metrics ###############
# Use  "Site.Code" to designate which site to include
#Data.use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="CAX",]

# Subset the species list for each site to calculate the functional diversity metrics
splist<-read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist<-unique(Data.Use$bin) #site list
Msplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
Msplist <- Msplist[Msplist$Class=="MAMMALIA",]
#newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
#subdata<-subset(Data.Use, Data.Use$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
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
#Alternatively, read in wpi weights from csv file
wpi_weights <- read.csv(file="wpi_weights.csv")
wpi_use <- wpi_weights[wpi_weights$Site.Code==data.use$Site.Code[1], ]
psi_weights <- wpi_use$psi.median[match(rownames(SiteTraits), wpi_use$bin)]
names(psi_weights) <- rownames(SiteTraits)
psi_use <- na.omit(psi_weights)
SiteTraits <- SiteTraits[match(names(psi_use), rownames(SiteTraits)),]
SiteTraits <- cbind(SiteTraits[,1:3], droplevels(SiteTraits[,4:5]), psi_use)
traitFD_w <- dbFD(SiteTraits[,1:dim(SiteTraits)[2]-1], a=psi_use[1:dim(SiteTraits)[1]], corr="cailliez", calc.FRic=FALSE)

# Calculate FD for birds
# Unweighted FD
birdFD <- dbFD(BirdTraits, corr="cailliez")
BirdTraits

# Weighted FD by psi (occupancy) values from WPI
B.psi_weights <- wpi_use$psi.median[match(rownames(BirdTraits), wpi_use$bin)]
names(B.psi_weights) <- rownames(BirdTraits)
Bpsi_use <- na.omit(B.psi_weights)
BirdTraits <- BirdTraits[match(names(Bpsi_use), rownames(BirdTraits)),]
BirdTraits <- cbind(BirdTraits, B.psi_weights)
BtraitFD_w <- dbFD(BirdTraits[,1:dim(BirdTraits)[2]-1], a=Bpsi_use[1:dim(BirdTraits)[1]], corr="cailliez", calc.FRic=FALSE)


# MANUAL STORAGE
# Store site/year specific values for extraction
# MAMMALS and BIRDS for each site

# use 2012 data
#CAXtraitFD_w <- traitFD_w
#B.CAXtraitFD_w <- BtraitFD_w

#PSHtraitFD_w <- traitFD_w
#B.PSHtraitFD_w <- BtraitFD_w

#YAStraitFD_w <- traitFD_w
#B.YAStraitFD_w <- BtraitFD_w

# use 2011 data
#BBStraitFD_w <- traitFD_w
#B.BBStraitFD_w <- BtraitFD_w

#BCItraitFD_w <- traitFD_w
#B.BCItraitFD_w <- BtraitFD_w

#BIFtraitFD_w <- traitFD_w
#B.BIFtraitFD_w <- BtraitFD_w

#COUtraitFD_w <- traitFD_w
#B.COUtraitFD_w <- BtraitFD_w

#KRPtraitFD_w <- traitFD_w
#B.KRPtraitFD_w <- BtraitFD_w

#MAStraitFD_w <- traitFD_w
#B.MAStraitFD_w <- BtraitFD_w

#NNNtraitFD_w <- traitFD_w
#B.NNNtraitFD_w <- BtraitFD_w

#RNFtraitFD_w <- traitFD_w
#B.RNFtraitFD_w <- BtraitFD_w

#UDZtraitFD_w <- traitFD_w
#B.UDZtraitFD_w <- BtraitFD_w

#VBtraitFD_w <- traitFD_w
#B.VBtraitFD_w <- BtraitFD_w

#YANtraitFD_w <- traitFD_w
#B.YANtraitFD_w <- BtraitFD_w

# Store all data for extraction (or calculate using the loop below)
#CTSite_FD <- list(BBStraitFD_w, BCItraitFD_w, BIFtraitFD_w, CAXtraitFD_w, COUtraitFD_w, KRPtraitFD_w, MAStraitFD_w,
#                  NNNtraitFD_w, PSHtraitFD_w, RNFtraitFD_w, UDZtraitFD_w, VBtraitFD_w, YANtraitFD_w, YAStraitFD_w) 

#B.CTSite_FD <- list(B.BBStraitFD_w, B.BCItraitFD_w, B.BIFtraitFD_w, B.CAXtraitFD_w, B.COUtraitFD_w, B.KRPtraitFD_w, B.MAStraitFD_w,
#                    B.NNNtraitFD_w, B.PSHtraitFD_w, B.RNFtraitFD_w, B.UDZtraitFD_w, B.VBtraitFD_w, B.YANtraitFD_w, B.YAStraitFD_w) 


########################## CALCULATE FD FOR ALL SITES #####################################
# Extract relevant data and create a list to loop over
# Ensure that all relevant data objects are included in the Data.Use object to loop over (and are in the desired output order - i.e. alphabetical)
CAX.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="CAX",]
PSH.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="PSH",]
YAS.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="YAS",]
BBS.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="BBS",]
BCI.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="BCI",]
#BIF.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="BIF",]
COU.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="COU",]
KRP.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="KRP",]
MAS.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="MAS",]
NNN.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="NNN",]
RNF.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="RNF",]
UDZ.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="UDZ",]
VB.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="VB-",]
YAN.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="YAN",]

Data.Use <- list(BBS.Use, BCI.Use, CAX.Use, COU.Use, KRP.Use, MAS.Use, NNN.Use, PSH.Use, RNF.Use, UDZ.Use, VB.Use, YAN.Use, YAS.Use)
library(FD)
CTSite_FD <- list()
B.CTSite_FD <- list()
CT.Shannon <- vector()
B.CT.Shannon <- vector()
splist <- read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist <- list()
Msplist <- list()
Bsplist <- list()
BirdTraits <- list()
SiteTraits <- list()

wpi_weights <- read.csv(file="wpi_weights.csv")
wpi_use <- list()
psi_weights <- list()
psi_use <- list()
CTSite_FD <- list()

B.psi_weights <- list()
Bpsi_use <- list()
B.CTSite_FD <- list()

# Loop over sites
for(i in 1:length(Data.Use)){

# Extract trait data
  sitelist[[i]] <- unique(Data.Use[[i]]$bin)
  sitelist
  Msplist[[i]] <- subset(splist,splist$Unique_Name %in% sitelist[[i]] & splist$Include==1)
  Msplist[[i]] <- Msplist[[i]][Msplist[[i]]$Class=="MAMMALIA",]
  Bsplist[[i]] <- subset(Btraits,rownames(Btraits) %in% sitelist[[i]] & Btraits$V3==1)
  BirdTraits[[i]] <- cbind(Bsplist[[i]][,1], Bsplist[[i]][,2])
  rownames(BirdTraits[[i]]) <- rownames(Bsplist[[i]])
  colnames(BirdTraits[[i]]) <- c("Mass", "Guild")
  BirdTraits[[i]] <- as.data.frame(BirdTraits[[i]])
  BirdTraits[[i]]$Guild <- droplevels(as.factor(BirdTraits[[i]]$Guild))

# Extract weights and format mammal data
  SiteTraits[[i]] <- Mtraits[match(Msplist[[i]]$Unique_Name, rownames(Mtraits)),]
  SiteTraits[[i]] <- cbind(SiteTraits[[i]][,1:3], droplevels(SiteTraits[[i]][,4:5]))
  wpi_use[[i]] <- wpi_weights[wpi_weights$Site.Code==Data.Use[[i]]$Site.Code[1], ]
  psi_weights[[i]] <- wpi_use[[i]]$psi.median[match(rownames(SiteTraits[[i]]), wpi_use[[i]]$bin)]
  names(psi_weights[[i]]) <- rownames(SiteTraits[[i]])
  psi_use[[i]] <- na.omit(psi_weights[[i]])
  SiteTraits[[i]] <- SiteTraits[[i]][match(names(psi_use[[i]]), rownames(SiteTraits[[i]])),]
  SiteTraits[[i]] <- cbind(SiteTraits[[i]][,1:3], droplevels(SiteTraits[[i]][,4:5]), psi_use[[i]])

# Calculate mammal FD and Shannon diversity indices  
  CTSite_FD[[i]] <- dbFD(SiteTraits[[i]][,1:dim(SiteTraits[[i]])[2]-1], a=psi_use[[i]][1:dim(SiteTraits[[i]])[1]], corr="cailliez", calc.FRic=FALSE)
  CT.Shannon[i] <- diversity(psi_use[[i]], index="shannon")
  
# Extract weights and format bird data
  B.psi_weights[[i]] <- wpi_use[[i]]$psi.median[match(rownames(BirdTraits[[i]]), wpi_use[[i]]$bin)]
  names(B.psi_weights[[i]]) <- rownames(BirdTraits[[i]])
  Bpsi_use[[i]] <- na.omit(B.psi_weights[[i]])
  BirdTraits[[i]] <- BirdTraits[[i]][match(names(Bpsi_use[[i]]), rownames(BirdTraits[[i]])),]
  BirdTraits[[i]] <- cbind(BirdTraits[[i]], B.psi_weights[[i]])

# Calculate bird FD and Shannon diversity indices 
  B.CTSite_FD[[i]] <- dbFD(BirdTraits[[i]][,1:dim(BirdTraits[[i]])[2]-1], a=Bpsi_use[[i]][1:dim(BirdTraits[[i]])[1]], corr="cailliez", calc.FRic=FALSE)
  B.CT.Shannon[i] <- diversity(Bpsi_use[[i]], index="shannon")
} 


#Extract Mammal FD calculations and create output table 
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

CTweighted <- cbind(CT.nbsp, CT.sing.sp, CT.FEve, CT.FDiv, CT.FDis, CT.RaoQ, CWM.Mass, CWM.Litter, CWM.GR, CT.Shannon)
CTweighted <- as.data.frame(CTweighted)
rownames(CTweighted) <- c("BBS", "BCI", "CAX", "COU", "KRP", "MAS", "NNN", "PSH", "RNF", "UDZ", "VB", "YAN", "YAS") 


#Extract Bird FD calculations and create output table
B.CT.nbsp <- vector()
B.CT.sing.sp <- vector()
B.CT.FEve <- vector()
B.CT.FDiv <- vector()
B.CT.FDis <- vector()
B.CT.RaoQ <- vector()
B.CWM.Mass <- vector()
B.CWM.Guild <- vector()

for(i in 1:length(CTSite_FD)){
  B.CT.nbsp[i] <- B.CTSite_FD[[i]]$nbsp
  B.CT.sing.sp[i] <- B.CTSite_FD[[i]]$sing.sp
  B.CT.FEve[i] <- B.CTSite_FD[[i]]$FEve
  B.CT.FDiv[i] <- B.CTSite_FD[[i]]$FDiv
  B.CT.FDis[i] <- B.CTSite_FD[[i]]$FDis
  B.CT.RaoQ[i] <- B.CTSite_FD[[i]]$RaoQ
  B.CWM.Mass[i] <- B.CTSite_FD[[i]]$CWM$Mass
  B.CWM.Guild[i] <- B.CTSite_FD[[i]]$CWM$Guild
}

B.CTweighted <- cbind(B.CT.nbsp, B.CT.sing.sp, B.CT.FEve, B.CT.FDiv, B.CT.FDis, B.CT.RaoQ, B.CWM.Mass, B.CT.Shannon)
B.CTweighted <- as.data.frame(B.CTweighted)
rownames(B.CTweighted) <- c("BBS", "BCI", "CAX", "COU", "KRP", "MAS", "NNN", "PSH", "RNF", "UDZ", "VB", "YAN", "YAS") 

CTweighted
B.CTweighted
#write.csv(CTweighted, file="FunctionalDiversity_Mammals_29April2014.csv", row.names=TRUE)
#write.csv(B.CTweighted, file="FunctionalDiversity_Birds_29April2014.csv", row.names=TRUE)
# Manually add "Site.Code" as first column name for these output files
# Manuall add "-" to "VB-" Site.Code so that later merges will operate correctly