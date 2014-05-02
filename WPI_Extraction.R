# Extract species specific occupancy data for each site to use as weights in functional diversity measurements
# For each species at each site in years 2011 and 2012, calculate the mean and median psi value

year_effects <- read.csv("year_effects.csv")
all_effects <- read.csv("all_effects.csv")
WPIoutput <- read.csv("psi_species_model_binomial_last1000.csv")
WPIsimple <- read.csv("psi_species_simplemodel_last1000.csv")
WPIconst <- read.csv("psi_species_model_const-phi-gam-p_last1000.csv")
WPIall <- rbind(WPIoutput, WPIsimple, WPIconst)
taxonomy <- read.csv("taxonomy_scientific_name_wpi_20140414_LB.csv")
bin <- paste(taxonomy$genus, taxonomy$species, sep=" ")
taxonomy <- cbind(taxonomy, bin)

Site.Code <- c("CAX", "CSN", "MAS", "VB-", "BIF", "BBS", "NAK", "NNN", "YAS", "RNF", "UDZ", "BCI", "YAN", "COU", "KRP", "PSH")
Site.ID <- unique(all_effects$site_id)
Combine <- cbind(Site.ID, Site.Code)[1:16,]
Combine <- as.data.frame(Combine)
Combine$Site.ID <- as.numeric(as.character(Combine$Site.ID))


table(WPIoutput$site_id, WPIoutput$year)

WPIall2011 <- WPIall[WPIall$year=="2011", 2:5]
by2011a <- WPIall2011$species_id
by2011b <- WPIall2011$site_id
psi2011mean <- aggregate(WPIall2011, by=list(by2011a, by2011b), FUN=mean)
psi2011median <- aggregate(WPIall2011, by=list(by2011a, by2011b), FUN=median)
psi2011 <- cbind(psi2011mean[,3:6], psi2011median[,6])
colnames(psi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
species2011 <- taxonomy$bin[match(psi2011$species_id, taxonomy$id)]
psi2011 <- cbind(psi2011, species2011)
sites2011 <- Combine$Site.Code[match(psi2011$site_id, Combine$Site.ID)]
psi2011 <- cbind(psi2011, sites2011)
names(psi2011) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")


WPIall2012 <- WPIall[WPIall$year=="2012", 2:5]
by2012a <- WPIall2012$species_id
by2012b <- WPIall2012$site_id
psi2012mean <- aggregate(WPIall2012, by=list(by2012a, by2012b), FUN=mean)
psi2012median <- aggregate(WPIall2012, by=list(by2012a, by2012b), FUN=median)
psi2012 <- cbind(psi2012mean[,3:6], psi2012median[,6])
colnames(psi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median")
species2012 <- taxonomy$bin[match(psi2012$species_id, taxonomy$id)]
psi2012 <- cbind(psi2012, species2012)
sites2012 <- Combine$Site.Code[match(psi2012$site_id, Combine$Site.ID)]
psi2012 <- cbind(psi2012, sites2012)
names(psi2012) <- c("species_id", "site_id", "year", "psi.mean", "psi.median", "bin", "Site.Code")


wpi_weights <- rbind(psi2012[psi2012$Site.Code=="PSH"|psi2012$Site.Code=="CAX"|psi2012$Site.Code=="YAS",],
                     psi2011[psi2011$Site.Code!="PSH"&psi2011$Site.Code!="CAX"&psi2011$Site.Code!="YAS",])

wpi_weights2011 <- subset(psi2012, Site.Code=="PSH"|
                        Site.Code=="CAX"|
                        Site.Code=="YAS")

wpi_weights2012 <- subset(psi2011, Site.Code=="BBS"|
                        Site.Code=="BCI"|
                        Site.Code=="BIF"|
                        Site.Code=="COU"|
                        Site.Code=="CSN"|
                        Site.Code=="KRP"|
                        Site.Code=="MAS"|
                        Site.Code=="NAK"|
                        Site.Code=="NNN"|
                        Site.Code=="RNF"|
                        Site.Code=="UDZ"|
                        Site.Code=="VB-"|
                        Site.Code=="YAN")

wpi_weights <- rbind(wpi_weights2011, wpi_weights2012)
#write.csv(wpi_weights, file="wpi_weights.csv", row.names=FALSE)


# Calculate 1000 shannon diversity indices for each site using the wpi occupancy values as abundances. 
# Use mean of 1000 values as diversity index for modeling
WPIall <- WPIall[,1:5]
WPIall <- cbind(WPIall, Combine$Site.Code[match(WPIall$site_id, Combine$Site.ID)])
names(WPIall) <- c("iteration", "species_id", "site_id", "year", "psi", "Site.Code")

WPIalluse <- subset(WPIall, Site.Code=="PSH" & year=="2012"|
                            Site.Code=="CAX" & year=="2012"|
                            Site.Code=="YAS" & year=="2012"| 
                            Site.Code=="BBS" & year=="2011"|
                            Site.Code=="BCI" & year=="2011"|
                            Site.Code=="BIF" & year=="2011"|
                            Site.Code=="COU" & year=="2011"|
                            Site.Code=="CSN" & year=="2011"|
                            Site.Code=="KRP" & year=="2011"|
                            Site.Code=="MAS" & year=="2011"|
                            Site.Code=="NAK" & year=="2011"|
                            Site.Code=="NNN" & year=="2011"|
                            Site.Code=="RNF" & year=="2011"|
                            Site.Code=="UDZ" & year=="2011"|
                            Site.Code=="VB-" & year=="2011"|
                            Site.Code=="YAN" & year=="2011")

WPIalluse <- WPIalluse[WPIalluse$iteration!=1001,]
WPIalluse <- WPIalluse[WPIalluse$iteration!=1002,]
WPIalluse$iteration <- factor(WPIalluse$iteration)

library(reshape)
library(vegan)


unstack(test, psi~iteration)
diversity(trythis, index="shannon", MARGIN=2)
median(trythis, index="shannon", MARGIN=2)



hold1 <- list()
hold2 <- list()
Shannon.Index <- vector()
for(i in 1:length(unique(WPIalluse$Site.Code))){
  hold1[[i]] <- WPIalluse[WPIalluse$site_id==as.integer(names(table(WPIalluse$site_id)))[[i]],]
  hold2[[i]] <- unstack(hold1[[i]], psi~iteration)
  Shannon.Index[i] <- median(diversity(hold2[[i]], index="shannon", MARGIN=2))
}

Shannon.Index <- cbind(Combine, Shannon.Index)[,2:3]
write.csv(Shannon.Index, file="ShannonIndex_Distribution.csv", row.names=FALSE)
