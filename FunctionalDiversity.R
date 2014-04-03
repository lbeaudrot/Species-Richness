# Calculate Functional Diversity

library(FD)

# Connect TEAM master species list to the PanTHERIA mammalian trait data

masterlist<-read.csv("master_species_list.csv",h=T) #master list
pantheria <- read.csv("Pantheria_Data_WR05_Aug2008.csv") # PanTHERIA data

#subset TEAM species list from overall PanTHERIA database
matchedlist <- pantheria[match(masterlist$Unique_Name, pantheria$Binomial),]
splist <- cbind(masterlist, matchedlist)

#subset species to the species in that site and with Include=1
sitelist<-unique(data.use$bin) #site list
newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
subdata<-subset(data.use, data.use$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
subdata<-f.correct.DF(subdata)

mammals <- splist[splist$Class=="MAMMALIA",]
#birds <- splist[splist$Class=="AVES",]

# Determine the number of species for which data are available for each trait
Nspecies <- vector(mode = "numeric", length=dim(mammals)[2])
for(i in 1:length(Nspecies)){
  Nspecies[i] <- sum(table(mammals[,i]))
}

Ntraits <- cbind(colnames(mammals), Nspecies)

# Subset trait data for which >150 species on TEAM list have data

mammalianTraits <- cbind(mammals[1:4], mammals$AdultHeadBodyLen_mm, mammals$LitterSize, mammals$GR_Area_km2, as.factor(mammals$ActivityCycle), as.factor(mammals$HabitatBreadth), as.factor(mammals$DietBreadth), mammals$Guild)
names(mammalianTraits) <- c("Bin", "Mass", "Class", "Family", "BodyLength", "LitterSize", "GR_Area", "ActivityCycle",  "HabitatBreadth", "DietBreadth", "Guild")

# Use "mammalianTraits" as input data for the f.family.avg function, which uses the following helper functions:
f.family.AC <- function(data){ #provide only data that are factors as input data
     hold <- table(data$Family, data$ActivityCycle)
     ActivityCycle_m <- vector(mode="numeric", length=dim(hold)[1])
  
      for(i in 1:dim(hold)[1]){
        ActivityCycle_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3], 1, 
                      ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3], 2, 
                             ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2], 3, NA)))
  }
  ActivityCycle_m
}
  
f.family.HB <- function(data){ #provide only data that are factors as input data
  hold <- table(data$Family, data$HabitatBreadth)
  HabitatBreadth_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    HabitatBreadth_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3] & hold[i,1] > hold[i,4], 1, 
                                 ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4], 2, 
                                        ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4], 3, 
                                            ifelse(hold[i,4] > hold[i,1] & hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3], 4, NA))))
  }
  HabitatBreadth_m
}


f.family.DB <- function(data){ 
  hold <- table(data$Family, data$DietBreadth)
  DietBreadth_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    DietBreadth_m[i] <- ifelse(hold[i,1] > hold[i,2] & hold[i,1] > hold[i,3] & hold[i,1] > hold[i,4] & hold[i,1] > hold[i,5] & hold[i,1] > hold[i,6] & hold[i,1] > hold[i,7], 1, 
                                  ifelse(hold[i,2] > hold[i,1] & hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4] & hold[i,2] > hold[i,5] & hold[i,2] > hold[i,6] & hold[i,2] > hold[i,7], 2, 
                                         ifelse(hold[i,3] > hold[i,1] & hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4] & hold[i,3] > hold[i,5] & hold[i,3] > hold[i,6] & hold[i,3] > hold[i,7], 3, 
                                                ifelse(hold[i,4] > hold[i,1] & hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3] & hold[i,4] > hold[i,5] & hold[i,4] > hold[i,6] & hold[i,4] > hold[i,7], 4, 
                                                       ifelse(hold[i,5] > hold[i,1] & hold[i,5] > hold[i,2] & hold[i,5] > hold[i,3] & hold[i,5] > hold[i,4] & hold[i,5] > hold[i,6] & hold[i,5] > hold[i,7], 5, 
                                                              ifelse(hold[i,6] > hold[i,1] & hold[i,6] > hold[i,2] & hold[i,6] > hold[i,3] & hold[i,6] > hold[i,4] & hold[i,6] > hold[i,5] & hold[i,6] > hold[i,7], 6, 
                                                                     ifelse(hold[i,7] > hold[i,1] & hold[i,7] > hold[i,2] & hold[i,7] > hold[i,3] & hold[i,7] > hold[i,4] & hold[i,7] > hold[i,5] & hold[i,7] > hold[i,6], 7, NA)))))))
  }
  DietBreadth_m
}


f.family.G <- function(data){ 
  hold <- table(data$Family, data$Guild)
  Guild_m <- vector(mode="numeric", length=dim(hold)[1])
  
  for(i in 1:dim(hold)[1]){
    Guild_m[i] <- ifelse(hold[i,2] > hold[i,3] & hold[i,2] > hold[i,4] & hold[i,2] > hold[i,5], 1, 
                               ifelse(hold[i,3] > hold[i,2] & hold[i,3] > hold[i,4] & hold[i,3] > hold[i,5], 2, 
                                      ifelse(hold[i,4] > hold[i,2] & hold[i,4] > hold[i,3] & hold[i,4] > hold[i,5], 3, 
                                             ifelse(hold[i,5] > hold[i,2] & hold[i,5] > hold[i,3] & hold[i,5] > hold[i,4], 3, NA))))
  }
  Guild_m
}


f.family.avg <- function(data){
  Mass_m <- tapply(data$Mass, droplevels(data$Family), median, na.rm=TRUE)
  BodyLength_m <- tapply(data$BodyLength, droplevels(data$Family), median, na.rm=TRUE)
  LitterSize_m <-tapply(data$LitterSize, droplevels(data$Family), median, na.rm=TRUE)
  GR_Area_m <- tapply(data$GR_Area, droplevels(data$Family), median, na.rm=TRUE)
  medians <- cbind(Mass_m, BodyLength_m, LitterSize_m, GR_Area_m)
  medians
  ActivityCycle_m <- f.family.AC(data)
  HabitatBreadth_m <- f.family.HB(data)
  DietBreadth_m <- f.family.HB(data)
  Guild_m <- f.family.G(data)
  modes <- cbind(ActivityCycle_m, HabitatBreadth_m, DietBreadth_m, Guild_m)
  rownames(modes) <- levels(data$Family)
  modes <- modes[match(rownames(medians), rownames(modes)),]
  fam_avg <- cbind(medians, modes)
  fam_avg
}
fam_avg <- f.family.avg(mammalianTraits)
fam_avg <- as.data.frame(fam_avg)
# Fill in missing values using family level averages (mode for factors; median for continuous variables)
# Replace missing values in mammalianTraits with Family-level averages (object "fam_avg") so that they are "corrected" (objects end in "_c")


Mass_c <- ifelse(is.na(mammalianTraits$Mass)==TRUE, fam_avg$Mass_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$Mass)
BodyLength_c <- ifelse(is.na(mammalianTraits$BodyLength)==TRUE, fam_avg$BodyLength_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$BodyLength)
LitterSize_c <- ifelse(is.na(mammalianTraits$LitterSize)==TRUE, fam_avg$LitterSize_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$LitterSize)
GR_Area_c <- ifelse(is.na(mammalianTraits$GR_Area)==TRUE, fam_avg$GR_Area_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$GR_Area)
ActivityCycle_c <- ifelse(is.na(mammalianTraits$ActivityCycle)==TRUE, fam_avg$ActivityCycle_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$ActivityCycle)
HabitatBreadth_c <- ifelse(is.na(mammalianTraits$HabitatBreadth)==TRUE, fam_avg$HabitatBreadth_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$HabitatBreadth)
DietBreadth_c <- ifelse(is.na(mammalianTraits$DietBreadth)==TRUE, fam_avg$DietBreadth_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$DietBreadth)
Guild_c <- ifelse(is.na(mammalianTraits$Guild)==TRUE, fam_avg$Guild_m[match(mammalianTraits$Family, rownames(fam_avg))],mammalianTraits$Guild)


Mtraits <- cbind(Mass_c, BodyLength_c, LitterSize_c, GR_Area_c, ActivityCycle_c, HabitatBreadth_c, DietBreadth_c, Guild_c)
rownames(Mtraits) <- mammalTraits$Bin
Mtraits <- as.data.frame(Mtraits)
Mtraits$ActivityCycle_c <- as.factor(Mtraits$ActivityCycle_c)
Mtraits$HabitatBreadth_c <- as.factor(Mtraits$HabitatBreadth_c)
Mtraits$DietBreadth_c <- as.factor(Mtraits$DietBreadth_c)
Mtraits$Guild_c <- as.factor(Mtraits$Guild_c)
str(Mtraits)


##### CODE FROM EARLIER
mammalTraits <- cbind(mammals[1:2], mammals$AdultHeadBodyLen_mm, mammals$LitterSize, mammals$GR_Area_km2, as.factor(mammals$ActivityCycle), as.factor(mammals$HabitatBreadth), as.factor(mammals$DietBreadth), mammals$Guild)
names(mammalTraits) <- c("Bin", "Mass", "BodyLength", "LitterSize", "GR_Area", "ActivityCycle",  "HabitatBreadth", "DietBreadth", "Guild")

mTraits <- mammalTraits[,2:9]
mTraits[,1] <- as.numeric(mTraits[,1])
rownames(mTraits) <- mammalTraits$Bin
#rownames(mTraits) <-paste("sp",1:242, sep=".") #Alternate (shorter) species labels




####### Calculate Functional Diversity using the FD package (try again once missing values are filled in with family averages)

gowdis(mTraits)
dbFD(x=mammalTraits)
