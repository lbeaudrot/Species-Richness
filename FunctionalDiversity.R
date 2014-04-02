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

f.family.avg <- function(data){
Mass_m <- tapply(data$Mass, droplevels(data$Family), median, na.rm=TRUE)
BodyLength_m <- tapply(data$BodyLength, droplevels(data$Family), median, na.rm=TRUE)
LitterSize_m <-tapply(data$LitterSize, droplevels(data$Family), median, na.rm=TRUE)
GR_Area_m <- tapply(data$GR_Area, droplevels(data$Family), median, na.rm=TRUE)
medians <- cbind(Mass_m, BodyLength_m, LitterSize_m, GR_Area_m)
ActivityCycle_m <- ifelse(apply(table(data$Family, data$ActivityCycle), MARGIN=1, FUN=max)>0,apply(table(data$Family, data$ActivityCycle), MARGIN=1, FUN=max), NA) 
HabitatBreadth_m <- ifelse(apply(table(data$Family, data$HabitatBreadth), MARGIN=1, FUN=max)>0,apply(table(data$Family, data$HabitatBreadth), MARGIN=1, FUN=max), NA) 
DietBreadth_m <- ifelse(apply(table(data$Family, data$DietBreadth), MARGIN=1, FUN=max)>0,apply(table(data$Family, data$DietBreadth), MARGIN=1, FUN=max), NA) 
Guild_m <- ifelse(apply(table(data$Family, data$Guild), MARGIN=1, FUN=max)>0,apply(table(data$Family, data$Guild), MARGIN=1, FUN=max), NA) 
modes <- cbind(ActivityCycle_m, HabitatBreadth_m, DietBreadth_m)
modes <- modes[match(rownames(medians), rownames(modes)),]
fam_avg <- cbind(medians, modes)
fam_avg
}

#Mass_m <- tapply(mammalianTraits$Mass, droplevels(mammalianTraits$Family), median, na.rm=TRUE)
#BodyLength_m <- tapply(mammalianTraits$BodyLength, droplevels(mammalianTraits$Family), median, na.rm=TRUE)
#LitterSize_m <-tapply(mammalianTraits$LitterSize, droplevels(mammalianTraits$Family), median, na.rm=TRUE)
#GR_Area_m <- tapply(mammalianTraits$GR_Area, droplevels(mammalianTraits$Family), median, na.rm=TRUE)
#medians <- cbind(Mass_m, BodyLength_m, LitterSize_m, GR_Area_m)


f.family.mode <- function()

hold <- table(mammalianTraits$Family, mammalianTraits$ActivityCycle)

for(i in 1:length(hold)){
  hold[i] <- max(i)
  
}



mammalTraits <- cbind(mammals[1:2], mammals$AdultHeadBodyLen_mm, mammals$LitterSize, mammals$GR_Area_km2, as.factor(mammals$ActivityCycle), as.factor(mammals$HabitatBreadth), as.factor(mammals$DietBreadth), mammals$Guild)
names(mammalTraits) <- c("Bin", "Mass", "BodyLength", "LitterSize", "GR_Area", "ActivityCycle",  "HabitatBreadth", "DietBreadth", "Guild")

mTraits <- mammalTraits[,2:9]
mTraits[,1] <- as.numeric(mTraits[,1])
rownames(mTraits) <- mammalTraits$Bin
#rownames(mTraits) <-paste("sp",1:242, sep=".") #Alternate (shorter) species labels

# Fill in missing values using family level averages (mode for factors; median for continuous variables)

table(mamm)


# Calculate Functional Diversity using the FD package

gowdis(mTraits)
dbFD(x=mammalTraits)
