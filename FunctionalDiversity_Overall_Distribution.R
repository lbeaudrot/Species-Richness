# Process Trait Data and Calculate Functional Diversity Metrics
# Combine both mammals and birds into single site level calculation
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

# Manually update names in PanTheria data to match IUCN taxonomy used in TEAM master list
pantheria$Binomial <- as.character(pantheria$Binomial)
Binomial2 <- ifelse(pantheria$Binomial=="Profelis aurata", "Caracal aurata",
ifelse(pantheria$Binomial=="Cephalophus natalensis", "Cephalophus harveyi",
ifelse(pantheria$Binomial=="Eulemur rufus", "Eulemur rufifrons",
ifelse(pantheria$Binomial=="Mazama gouazoubira", "Mazama nemorivaga",
ifelse(pantheria$Binomial=="Catopuma temminckii", "Pardofelis temminckii",
ifelse(pantheria$Binomial=="Manis tricuspis", "Phataginus tricuspis",
ifelse(pantheria$Binomial=="Manis gigantea", "Smutsia gigantea",
ifelse(pantheria$Binomial=="Muntiacus muntjak", "Muntiacus montanus", pantheria$Binomial))))))))
pantheria$Binomial <- factor(Binomial2)

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
colnames(Btraits) <- c("Mass_c_bird", "Guild_c_bird", "Include")
Btraits <- Btraits[Btraits$Include==1,]
Btraits <- Btraits[,1:2]
BTraits <- cbind(Btraits[,1], rep(NA, dim(Btraits)[1]), rep(NA, dim(Btraits)[1]), rep(NA, dim(Btraits)[1]), Btraits[,2])
colnames(BTraits) <- colnames(Mtraits)
rownames(BTraits) <- rownames(Btraits)

# Combine mammals and birds into single object for functional diversity analyses
Mtraits <- rbind(Mtraits, BTraits)


# Limit list to species in the WPI include list
Includelist <- masterlist[masterlist$Include==1,]
IncludeTraits <- Mtraits[match(Includelist$Unique_Name, rownames(Mtraits)),]







############################## CALCULATE FD FOR INDIVIDUAL SITE ######################
######## Start here to CHANGE INPUT DATA FOR Functional Diversity metrics ###############
#Examine which sites have data for which years
#table(alldata$Site.Code, alldata$Sampling.Period) 
# Use  "Site.Code" to designate which site to include
#Data.use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="CAX",]

# Subset the species list for each site to calculate the functional diversity metrics
splist<-read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist<-unique(Data.use$bin) #site list
Splist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
Splist$Guild <- droplevels(as.factor(Splist$Guild))

# Use the subsetted species list for the site to extract the corresponding functional trait data
SiteTraits <- Mtraits[match(Msplist$Unique_Name, rownames(Mtraits)),]
#rownames(SiteTraits) <-paste("sp",1:dim(SiteTraits)[1], sep=".") #Alternate (shorter) species labels
## NB For traits that are factors, all levels must be represented in data to obtain results using dbFD()
#Reclassify SiteTraits list so that only new levels are used
SiteTraits <- cbind(SiteTraits[,1:3], droplevels(SiteTraits[,4:5]))
#SiteTraits

####### Calculate Functional Diversity using the FD package
#Calculate unweighted functional diversity
#traitFD <- dbFD(SiteTraits, corr="cailliez")
#Use object wpi_weights to weight functional trait calculations by occupancy estimates (psi.mean or psi.median)
#Alternatively, read in wpi weights from csv file
wpi_weights <- read.csv(file="wpi_weights.csv")
wpi_use <- wpi_weights[wpi_weights$Site.Code==Data.use$Site.Code[1], ]
psi_weights <- wpi_use$psi.median[match(rownames(SiteTraits), wpi_use$bin)]
names(psi_weights) <- rownames(SiteTraits)
psi_use <- na.omit(psi_weights)
SiteTraits <- SiteTraits[match(names(psi_use), rownames(SiteTraits)),]
SiteTraits <- cbind(SiteTraits[,1:3], droplevels(SiteTraits[,4:5]), psi_use)
traitFD_w <- dbFD(SiteTraits[,1:dim(SiteTraits)[2]-1], a=psi_use[1:dim(SiteTraits)[1]], corr="cailliez", calc.FRic=FALSE)

# MANUAL STORAGE
# Store site/year specific values for extraction
# MAMMALS and BIRDS for each site

# use 2012 data
#CAXtraitFD_w <- traitFD_w
#PSHtraitFD_w <- traitFD_w
#YAStraitFD_w <- traitFD_w
#BIFtraitFD_w <- traitFD_w

# use 2011 data
#BBStraitFD_w <- traitFD_w
#BCItraitFD_w <- traitFD_w
#COUtraitFD_w <- traitFD_w
#KRPtraitFD_w <- traitFD_w
#MAStraitFD_w <- traitFD_w
#NNNtraitFD_w <- traitFD_w
#RNFtraitFD_w <- traitFD_w
#UDZtraitFD_w <- traitFD_w
#VBtraitFD_w <- traitFD_w
#YANtraitFD_w <- traitFD_w

# Store all data for extraction (or calculate using the loop below)
#CTSite_FD <- list(BBStraitFD_w, BCItraitFD_w, BIFtraitFD_w, CAXtraitFD_w, COUtraitFD_w, KRPtraitFD_w, MAStraitFD_w,
#                  NNNtraitFD_w, PSHtraitFD_w, RNFtraitFD_w, UDZtraitFD_w, VBtraitFD_w, YANtraitFD_w, YAStraitFD_w) 



########################## CALCULATE FD FOR ALL SITES #####################################
# Extract relevant data and create a list to loop over
# Ensure that all relevant data objects are included in the Data.Use object to loop over (and are in the desired output order - i.e. alphabetical)
CAX.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="CAX",]
PSH.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="PSH",]
YAS.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="YAS",]
BBS.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="BBS",]
BCI.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="BCI",]
BIF.Use <- allevents[allevents$Sampling.Period=="2012.01" & allevents$Site.Code=="BIF",]
COU.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="COU",]
KRP.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="KRP",]
MAS.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="MAS",]
NNN.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="NNN",]
RNF.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="RNF",]
UDZ.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="UDZ",]
VB.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="VB-",]
YAN.Use <- allevents[allevents$Sampling.Period=="2011.01" & allevents$Site.Code=="YAN",]

#Data.Use <- list(BBS.Use, BCI.Use, BIF.Use, CAX.Use, COU.Use, KRP.Use, MAS.Use, NNN.Use, PSH.Use, RNF.Use, UDZ.Use, VB.Use, YAN.Use, YAS.Use)
# Reorder input data to match Combine$Site.Code order
Data.Use <- list(CAX.Use, MAS.Use, VB.Use, BIF.Use, BBS.Use, NNN.Use, YAS.Use, RNF.Use, UDZ.Use, BCI.Use, YAN.Use, COU.Use, KRP.Use, PSH.Use)

#Data.Use <- list(BBS.Use, BCI.Use, BIF.Use, CAX.Use, COU.Use, KRP.Use, MAS.Use, NNN.Use, PSH.Use, RNF.Use, UDZ.Use, VB.Use, YAN.Use, YAS.Use)


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

CT.nbsp <- matrix()
CT.sing.sp <- matrix()
CT.FEve <- matrix()
CT.FDiv <- matrix()
CT.FDis <- matrix()
CT.RaoQ <- matrix()
CWM.Mass <- matrix()
CWM.Litter <- matrix()
CWM.GR <- matrix()
CWM.Activity <- matrix()
CWM.Guild <- matrix()

CTweighted <- list()
CTweightedmedian <- list()
hold3 <- vector()

hold1 <- list()
weights1000 <- list() 
for(i in 1:length(unique(WPIalluse$Site.Code))){
  hold1[[i]] <- WPIalluse[WPIalluse$site_id==as.integer(names(table(WPIalluse$site_id)))[[i]],]
  weights1000[[i]] <- unstack(hold1[[i]], psi~iteration)
  rownames(weights1000[[i]]) <- unique(hold1[[i]]$species_id)
  rownames(weights1000[[i]]) <- taxonomy$bin[match(rownames(weights1000[[i]]), taxonomy$id)]
} 
# hold2 provides the psi weights to use for each of the 1000 iterations
# loop over columns in hold2 and calculate FDis using weights from each column to obtain distribution
# note that the Site.Code order is not alphabetical, but instead the same as Combine$Site.Code, hence reordered Data.Use object above
# also note that species (row) order likely also differs from functional trait data

# Loop over sites
for(i in 1:length(Data.Use)){
  
  # Extract trait data
  
    
  sitelist[[i]] <- unique(Data.Use[[i]]$bin)
  sitelist
  sitelist[[i]] <- factor(sitelist[[i]])
  SiteTraits[[i]] <- subset(IncludeTraits, rownames(IncludeTraits) %in%  sitelist[[i]])
  SiteTraits[[i]] <- cbind(SiteTraits[[i]][,1:3], droplevels(SiteTraits[[i]][,4:5])) 
  SiteTraits[[i]] <- SiteTraits[[i]][match(rownames(weights1000[[i]]), rownames(SiteTraits[[i]])),]
  SiteTraits[[i]] <- SiteTraits[[i]][is.na(SiteTraits[[i]]$Mass_c)==FALSE,]
  SiteTraits[[i]] <- cbind(SiteTraits[[i]][,1:3], droplevels(SiteTraits[[i]][,4:5]))
  
  weights1000[[i]] <- weights1000[[i]][match(rownames(SiteTraits[[i]]), rownames(weights1000[[i]])),]
} 

  
    for(i in 1:length(Data.Use)){
      for(j in 1:dim(weights1000[[i]])[2]){
        #for(j in 1:2){
    hold3 <- weights1000[[i]][,j]
    names(hold3) <- rownames(weights1000[[i]])
    CTSite_FD[[j]] <- dbFD(SiteTraits[[i]], a=hold3, corr="cailliez", calc.FRic=FALSE)
   
    CT.nbsp[j] <- CTSite_FD[[j]]$nbsp
    CT.sing.sp[j] <- CTSite_FD[[j]]$sing.sp
    CT.FEve[j] <- CTSite_FD[[j]]$FEve
    CT.FDiv[j] <- CTSite_FD[[j]]$FDiv
    CT.FDis[j] <- CTSite_FD[[j]]$FDis
    CT.RaoQ[j] <- CTSite_FD[[j]]$RaoQ
    CWM.Mass[j] <- CTSite_FD[[j]]$CWM$Mass_c
    CWM.Litter[j] <- CTSite_FD[[j]]$CWM$LitterSize_c
    CWM.GR[j] <- CTSite_FD[[j]]$CWM$GR_Area_c
    CWM.Activity[j] <- CTSite_FD[[j]]$CWM$ActivityCycle_c
    CWM.Guild[j] <- CTSite_FD[[j]]$CWM$Guild_c
      }
  CTweighted[[i]] <- cbind(CT.nbsp, CT.sing.sp, CT.FEve, CT.FDiv, CT.FDis, CT.RaoQ, CWM.Mass, CWM.Litter, CWM.GR)
  CTweighted[[i]] <- as.data.frame(CTweighted[[i]])
  CTweightedmedian[i] <- median(CTweighted[[i]]$CT.FDis)
  
}    

CTweightedmedian
names(CTweightedmedian) <- c("CAX", "MAS", "VB-", "BIF", "BBS", "NNN", "YAS", "RNF", "UDZ", "BCI", "YAN", "COU", "KRP", "PSH")
CTweightedmedian

# Calculate standard deviations of estimates
#CT.FDis.sd <- c(sd(CTweighted$CT.FDis), sd(CTweighted$CT.FDis.1), sd(CTweighted$CT.FDis.2), sd(CTweighted$CT.FDis.3), sd(CTweighted$CT.FDis.4), sd(CTweighted$CT.FDis.5), sd(CTweighted$CT.FDis.6), sd(CTweighted$CT.FDis.7), sd(CTweighted$CT.FDis.8), sd(CTweighted$CT.FDis.9), sd(CTweighted$CT.FDis.10), sd(CTweighted$CT.FDis.11), sd(CTweighted$CT.FDis.12), sd(CTweighted$CT.FDis.13))
CT.FDis.sd <- c(sd(CTweighted[[1]]$CT.FDis), sd(CTweighted[[2]]$CT.FDis), sd(CTweighted[[3]]$CT.FDis), sd(CTweighted[[4]]$CT.FDis), sd(CTweighted[[5]]$CT.FDis), sd(CTweighted[[6]]$CT.FDis), sd(CTweighted[[7]]$CT.FDis), sd(CTweighted[[8]]$CT.FDis), sd(CTweighted[[9]]$CT.FDis), sd(CTweighted[[10]]$CT.FDis), sd(CTweighted[[11]]$CT.FDis), sd(CTweighted[[12]]$CT.FDis), sd(CTweighted[[13]]$CT.FDis), sd(CTweighted[[14]]$CT.FDis))

#CTweighted
save(CTweighted, file="CTweighted_19May2014.gzip",compress="gzip")
write.csv(cbind(unlist(CTweightedmedian), CT.FDis.sd), file="FunctionalDiversity_Overall_Distribution_19May2014.csv", row.names=TRUE)
write.csv(CTweighted, file="FunctionalDiversity_Overall_ListOutput_19May2014.csv", row.names=TRUE)
# Manually add "Site.Code" as first column name for  output file
# Manuall add "-" to "VB-" Site.Code so that later merges will operate correctly