# Extract species specific occupancy data for each site to use as weights in functional diversity measurements
# For each species at each site in years 2011 and 2012, calculate the mean and median psi value

year_effects <- read.csv("year_effects.csv")
all_effects <- read.csv("all_effects.csv")
WPIoutput <- read.csv("psi_species_model_binomial_last1000.csv")
WPIsimple <- read.csv("psi_species_simplemodel_last1000.csv")
taxonomy <- read.csv("taxonomy_scientific_name_wpi_20140414_LB.csv")
bin <- paste(taxonomy$genus, taxonomy$species, sep=" ")
taxonomy <- cbind(taxonomy, bin)

# Site ID 
#1 <- "CAX" (American)
#2 <- "CSN" (American)
#3 <- "MAS" (American)
#5 <- "VB-"
#7 <- "BIF" (African)
#10 <- "BBS" (Asian)
#11 <- "NAK" (Asian)
#13 <- "NNN" (African) 
#19 <- "YAS" (American) 
#25 <- "RNF" (African)
#26 <- "UDZ" (African)
#27 <- "BCI" (American)
#28 <- "YAN" (American) 
#29 <- "COU" (American)
#30 <- "KRP" (African)
#32 <- "PSH" (Asian)
Site.Code <- c("CAX", "CSN", "MAS", "VB-", "BIF", "BBS", "NAK", "NNN", "YAS", "RNF", "UDZ", "BCI", "YAN", "COU", "KRP", "PSH")
Site.ID <- unique(all_effects$site_id)
Combine <- cbind(Site.ID, Site.Code)[1:16,]
Combine <- as.data.frame(Combine)
Combine$Site.ID <- as.numeric(as.character(Combine$Site.ID))

table(WPIoutput$site_id, WPIoutput$year)
WPI.2011 <- WPIoutput[WPIoutput$year=="2011",2:5]
WPI.2012 <- WPIoutput[WPIoutput$year=="2012",2:5]

by.1=WPI.2011$species_id
by.2=WPI.2011$site_id
psi2011mean <- aggregate(WPI.2011, by=list(by.1, by.2), FUN=mean)
psi2011median <- aggregate(WPI.2011, by=list(by.1, by.2), FUN=median)
psi2011 <- cbind(psi2011mean[,3:6], psi2011median[,6])
colnames(psi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
species2011 <- taxonomy$bin[match(psi2011$species_id, taxonomy$id)]
sites2011 <- Combine$Site.Code[match(psi2011$site_id, Combine$Site.ID)]
psi2011 <- cbind(psi2011, species2011, sites2011)
names(psi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")

by.3=WPI.2012$species_id
by.4=WPI.2012$site_id
psi2012mean <- aggregate(WPI.2012, by=list(by.3, by.4), FUN=mean)
psi2012median <- aggregate(WPI.2012, by=list(by.3, by.4), FUN=median)
psi2012 <- cbind(psi2012mean[,3:6], psi2012median[,6])
colnames(psi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
species2012 <- taxonomy$bin[match(psi2012$species_id, taxonomy$id)]
sites2012 <- Combine$Site.Code[match(psi2012$site_id, Combine$Site.ID)]
psi2012 <- cbind(psi2012, species2012, sites2012)
names(psi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")


WPI.simple.2011 <- WPIsimple[WPIsimple$year=="2011",2:5]
by.5=WPI.simple.2011$species_id
by.6=WPI.simple.2011$site_id
Simplepsi2011mean <- aggregate(WPI.simple.2011, by=list(by.5, by.6), FUN=mean)
Simplepsi2011median <- aggregate(WPI.simple.2011, by=list(by.5, by.6), FUN=median)
Simplepsi2011 <- cbind(Simplepsi2011mean[,3:6], Simplepsi2011median[,6])
colnames(Simplepsi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
S.species2011 <- taxonomy$bin[match(Simplepsi2011$species_id, taxonomy$id)]
S.psi2011 <- cbind(Simplepsi2011, S.species2011)
S.sites2011 <- Combine$Site.Code[match(S.psi2011$site_id, Combine$Site.ID)]
S.psi2011 <- cbind(S.psi2011, S.sites2011)
names(S.psi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")


WPI.simple.2012 <- WPIsimple[WPIsimple$year=="2012",2:5]
by.7=WPI.simple.2012$species_id
by.8=WPI.simple.2012$site_id
Simplepsi2012mean <- aggregate(WPI.simple.2012, by=list(by.7, by.8), FUN=mean)
Simplepsi2012median <- aggregate(WPI.simple.2012, by=list(by.7, by.8), FUN=median)
Simplepsi2012 <- cbind(Simplepsi2012mean[,3:6], Simplepsi2012median[,6])
colnames(Simplepsi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
S.species2012 <- taxonomy$bin[match(Simplepsi2012$species_id, taxonomy$id)]
S.psi2012 <- cbind(Simplepsi2012, S.species2012)
S.sites2012 <- Combine$Site.Code[match(S.psi2012$site_id, Combine$Site.ID)]
S.psi2012 <- cbind(S.psi2012, S.sites2012)
names(S.psi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")

wpi_2011 <- rbind(psi2011, S.psi2011)
wpi_2012 <- rbind(psi2012, S.psi2012)

wpi_weights <- rbind(wpi_2012[wpi_2012$Site.Code=="PSH"|wpi_2012$Site.Code=="CAX"|wpi_2012$Site.Code=="YAS",],
wpi_2011[wpi_2011$Site.Code!="PSH"&wpi_2011$Site.Code!="CAX"&wpi_2011$Site.Code!="YAS",])
