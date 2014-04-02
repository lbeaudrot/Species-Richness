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

mammalTraits <- cbind(mammals[1:6], as.factor(mammals$ActivityCycle), mammals$AdultHeadBodyLen_mm, as.factor(mammals$HabitatBreadth), as.factor(mammals$DietBreadth), mammals$LitterSize, mammals$GR_Area_km2)
names(mammalTraits) <- c("Bin", "Mass", "Class", "Family", "Guild", "Include", "ActivityCycle", "BodyLength", "HabitatBreadth", "DietBreadth", "LitterSize", "GR_Area")

# Calculate Functional Diversity using the FD package

gowdis()
