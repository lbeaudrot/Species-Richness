# Calculate above Ground Biomass and carbon density per hectare using Chave et al. 2005 methods for all TEAM sites/plots/years for Alex

# Query vegetation data from database
#Vegdata <- f.teamdb.query("vegetation")
#Vdata <- Vegdata

Vdata <- f.teamdb.query("vegetation")

# Add column with site codes
Site.CodeT <- substr(Vdata$tree$"1haPlotNumber",4,6)
Site.CodeL <- substr(Vdata$liana$"1haPlotNumber",4,6)

Vdata <- list(cbind(Vdata$tree, Site.CodeT), cbind(Vdata$liana, Site.CodeL))
names(Vdata) <- c("tree", "liana")

# Manually clean data (prior to database cleaning) using corrected spellings from Tropicos
# Duplicate tree genera 
Vdata$tree$Genus[Vdata$tree$Genus=="Albizia Durazz."] <- "Albizia"
Vdata$tree$Genus[Vdata$tree$Genus=="Allophyllus"] <- "Allophylus"
Vdata$tree$Genus[Vdata$tree$Genus=="Cassearea"] <- "Casearia"
Vdata$tree$Genus[Vdata$tree$Genus=="Cassipourea Aubl."] <- "Cassipourea"
Vdata$tree$Genus[Vdata$tree$Genus=="Casspourea"] <- "Cassipourea"
Vdata$tree$Genus[Vdata$tree$Genus=="Chrysoclamys"] <- "Chrysochlamys"
Vdata$tree$Genus[Vdata$tree$Genus=="Clerodendron"] <- "Clerodendrum"
Vdata$tree$Genus[Vdata$tree$Genus=="Corida"] <- "Cordia"
Vdata$tree$Genus[Vdata$tree$Genus=="Denrocnide"] <- "Dendrocnide"
Vdata$tree$Genus[Vdata$tree$Genus=="Drypetes Vahl"] <- "Drypetes"
Vdata$tree$Genus[Vdata$tree$Genus=="Dysoxyllum"] <- "Dysoxylum"
Vdata$tree$Genus[Vdata$tree$Genus=="Elaegia"] <- "Elaeagia"
Vdata$tree$Genus[Vdata$tree$Genus=="Erythroxylon"] <- "Erythroxylum"
Vdata$tree$Genus[Vdata$tree$Genus=="Illex"] <- "Ilex"
Vdata$tree$Genus[Vdata$tree$Genus=="Luehopsis"] <- "Lueheopsis"
Vdata$tree$Genus[Vdata$tree$Genus=="Melanochylla"] <- "Melanochyla"
Vdata$tree$Genus[Vdata$tree$Genus=="Neea/Guapira"] <- "Neea"
Vdata$tree$Genus[Vdata$tree$Genus=="Payena Lucida"] <- "Payena"
Vdata$tree$Genus[Vdata$tree$Genus=="Pleurothiryum"] <- "Pleurothyrium"
Vdata$tree$Genus[Vdata$tree$Genus=="Pleurotyrium"] <- "Pleurothyrium"
Vdata$tree$Genus[Vdata$tree$Genus=="Potamea"] <- "Potameia"
Vdata$tree$Genus[Vdata$tree$Genus=="Psychotria Mahonii"] <- "Psychotria"
Vdata$tree$Genus[Vdata$tree$Genus=="Querqus"] <- "Quercus"
Vdata$tree$Genus[Vdata$tree$Genus=="Rhodmnia"] <- "Rhodamnia"
Vdata$tree$Genus[Vdata$tree$Genus=="Rinoerocarpus"] <- "Rinoreocarpus"
Vdata$tree$Genus[Vdata$tree$Genus=="Ritchea"] <- "Ritchiea"
Vdata$tree$Genus[Vdata$tree$Genus=="Rytiginia"] <- "Rytigynia"
Vdata$tree$Genus[Vdata$tree$Genus=="Sapotaceae"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Sygygium"] <- "Syzygium"
Vdata$tree$Genus[Vdata$tree$Genus=="Sygyzium"] <- "Syzygium"
Vdata$tree$Genus[Vdata$tree$Genus=="Symplocoa"] <- "Symplocos"
Vdata$tree$Genus[Vdata$tree$Genus=="Tratinickia"] <- "Trattinnickia"
Vdata$tree$Genus[Vdata$tree$Genus=="Tremma"] <- "Trema"
Vdata$tree$Genus[Vdata$tree$Genus=="Turaea"] <- "Turraea"
Vdata$tree$Genus[Vdata$tree$Genus=="Xantophyllum"] <- "Xanthophyllum"
Vdata$tree$Genus[Vdata$tree$Genus=="Xymolas"] <- "Xymalos"
Vdata$tree$Genus[Vdata$tree$Genus=="Zanthoxylem"] <- "Zanthoxylum"
Vdata$tree$Genus[Vdata$tree$Genus=="Zizyphus"] <- "Ziziphus"
Vdata$tree$Genus[Vdata$tree$Genus=="Gymnosporia (Ex. Maytenus)"] <- "Gymnacranthera"
Vdata$tree$Genus[Vdata$tree$Genus=="Hensenia"] <- "Heinsenia"
Vdata$tree$Genus[Vdata$tree$Genus=="Mycronychia"] <- "Micronychia"
Vdata$tree$Genus[Vdata$tree$Genus=="Podo"] <- "Podocarpus"
Vdata$tree$Genus[Vdata$tree$Genus=="Polycious"] <- "Polyscias"
Vdata$tree$Genus[Vdata$tree$Genus=="Scheflera"] <- "Schefflera"
# Tree misspellings
Vdata$tree$Genus[Vdata$tree$Genus=="Caloxylon"] <- "Merrillia"
Vdata$tree$Genus[Vdata$tree$Genus=="Cavanalesia"] <- "Cavanillesia"
Vdata$tree$Genus[Vdata$tree$Genus=="Chysoceton"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Clochidion"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Cytogononia"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Eonimus"] <- "Euonymus"
Vdata$tree$Genus[Vdata$tree$Genus=="Escalonia"] <- "Escallonia"
Vdata$tree$Genus[Vdata$tree$Genus=="Euricoma"] <- "Eurycoma"
Vdata$tree$Genus[Vdata$tree$Genus=="Ferminiana"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Fragraea"] <- "Fagraea"
Vdata$tree$Genus[Vdata$tree$Genus=="Graffenriedia"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Hyeronima"] <- "Hieronyma"
Vdata$tree$Genus[Vdata$tree$Genus=="Hynocarpus"] <- "Hydnocarpus"
Vdata$tree$Genus[Vdata$tree$Genus=="Julbelniadia"] <- "Julbernardia"
Vdata$tree$Genus[Vdata$tree$Genus=="Lagerstromia"] <- "Lagerstroemia"
Vdata$tree$Genus[Vdata$tree$Genus=="Lauraceae"] <- "Unknown" #Family not genus
Vdata$tree$Genus[Vdata$tree$Genus=="Malanochyla"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Mdumbadumba"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Metrododorea"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Meyogine"] <- "Meiogyne"
Vdata$tree$Genus[Vdata$tree$Genus=="Microsdesmis"] <- "Microdesmis"
Vdata$tree$Genus[Vdata$tree$Genus=="Mnunganunga"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Mollotus"] <- "Mallotus"
Vdata$tree$Genus[Vdata$tree$Genus=="Msengela"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Muntinga"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Neouvaria"] <- "Neo-uvaria"
Vdata$tree$Genus[Vdata$tree$Genus=="Octodes"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Onychapetalum"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Papilionoidea"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Physocalymna"] <- "Physocalymma"
Vdata$tree$Genus[Vdata$tree$Genus=="Rhinorea"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Rhytigynia"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Rnodostemomodaphne"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Saccopethalum"] <- "Saccopetalum"
Vdata$tree$Genus[Vdata$tree$Genus=="Schweilera"] <- "Eschweilera"
Vdata$tree$Genus[Vdata$tree$Genus=="Staphyllea"] <- "Staphylea"
Vdata$tree$Genus[Vdata$tree$Genus=="Ticodendrom"] <- "Ticodendron"
Vdata$tree$Genus[Vdata$tree$Genus=="Vuguierenthus"] <- "Unknown"
Vdata$tree$Genus[Vdata$tree$Genus=="Yrianthera"] <- "Iryanthera"
# Tree families
Vdata$tree$Family[Vdata$tree$Family=="Acanthaceae Juss."] <- "Acanthaceae"
Vdata$tree$Family[Vdata$tree$Family=="Annoniaceae"] <- "Annonaceae"
Vdata$tree$Family[Vdata$tree$Family=="Apocynaceae Juss."] <- "Apocynaceae"
Vdata$tree$Family[Vdata$tree$Family=="Aquifoliaceae Bercht. & J. Presl"] <- "Aquifoliaceae"
Vdata$tree$Family[Vdata$tree$Family=="Asteraceae Bercht. & J. Presl"] <- "Asteraceae"
Vdata$tree$Family[Vdata$tree$Family=="Berseraceae"] <- "Burseraceae"
Vdata$tree$Family[Vdata$tree$Family=="Capparaceae Juss."] <- "Capparaceae"
Vdata$tree$Family[Vdata$tree$Family=="Capparidaceae"] <- "Capparaceae"
Vdata$tree$Family[Vdata$tree$Family=="Caryocaceae"] <- "Caryocaraceae"
Vdata$tree$Family[Vdata$tree$Family=="Celastomataceae"] <- "Melastomataceae"
Vdata$tree$Family[Vdata$tree$Family=="Chlorantaceae"] <- "Chloranthaceae"
Vdata$tree$Family[Vdata$tree$Family=="Chrysobalanaceae R. Br."] <- "Chrysobalanaceae"
Vdata$tree$Family[Vdata$tree$Family=="Cluciaceae"] <- "Clusiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Cornaceae Bercht. & J. Presl"] <- "Cornaceae"
Vdata$tree$Family[Vdata$tree$Family=="Eleaocarpaceae"] <- "Elaeocarpaceae"
Vdata$tree$Family[Vdata$tree$Family=="Euphorbiacae"] <- "Euphorbiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Euphorbiacaea"] <- "Euphorbiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Euphorbiaceae Juss."] <- "Euphorbiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Fabaceae-Caesalpinioideae"] <- "Fabaceae"
Vdata$tree$Family[Vdata$tree$Family=="Fabaceae Lindl."] <- "Fabaceae"
Vdata$tree$Family[Vdata$tree$Family=="Fabaceae-Mimosoideae"] <- "Fabaceae"
Vdata$tree$Family[Vdata$tree$Family=="Fabaceae-Papilionoideae"] <- "Fabaceae"
Vdata$tree$Family[Vdata$tree$Family=="Familia Incogn."] <- "Unknown"
Vdata$tree$Family[Vdata$tree$Family=="Hippocastenacea"] <- "Hippocastanaceae"
Vdata$tree$Family[Vdata$tree$Family=="Labiateae"] <- "Lamiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Lacistemaceae"] <- "Lacistemataceae"
Vdata$tree$Family[Vdata$tree$Family=="Malvaceae Juss."] <- "Malvaceae"
Vdata$tree$Family[Vdata$tree$Family=="Meliaceae Juss."] <- "Meliaceae"
Vdata$tree$Family[Vdata$tree$Family=="Melianthaceae Horan."] <- "Melianthaceae"
Vdata$tree$Family[Vdata$tree$Family=="Mirsinaceae"] <- "Myrsinaceae"
Vdata$tree$Family[Vdata$tree$Family=="Monimiaceae Juss."] <- "Monimiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Moraceae Gaudich."] <- "Moraceae"
Vdata$tree$Family[Vdata$tree$Family=="Myrtaceae Juss."] <- "Myrtaceae"
Vdata$tree$Family[Vdata$tree$Family=="Olacaceae R. Br."] <- "Olacaceae"
Vdata$tree$Family[Vdata$tree$Family=="Oleaceae Hoffmanns. & Link"] <- "Oleaceae"
Vdata$tree$Family[Vdata$tree$Family=="Phyllantaceae"] <- "Phyllanthaceae"
Vdata$tree$Family[Vdata$tree$Family=="Phytollacaceae"] <- "Phytolaccaceae"
Vdata$tree$Family[Vdata$tree$Family=="Podocarpaceae Endl."] <- "Podocarpaceae"
Vdata$tree$Family[Vdata$tree$Family=="Primulaceae Batsch Ex Borkh."] <- "Primulaceae"
Vdata$tree$Family[Vdata$tree$Family=="Putranjivaceae Meisn."] <- "Putranjivaceae"
Vdata$tree$Family[Vdata$tree$Family=="Rhamnaceae Juss."] <- "Rhamnaceae"
Vdata$tree$Family[Vdata$tree$Family=="Rhizophoraceae Pers."] <- "Rhizophoraceae"
Vdata$tree$Family[Vdata$tree$Family=="Rosaceae Juss"] <- "Rosaceae"
Vdata$tree$Family[Vdata$tree$Family=="Rosaceae Juss."] <- "Rosaceae"
Vdata$tree$Family[Vdata$tree$Family=="Rubiaceae Juss."] <- "Rubiaceae"
Vdata$tree$Family[Vdata$tree$Family=="Rutaceae Juss."] <- "Rutaceae"
Vdata$tree$Family[Vdata$tree$Family=="Sapindaceae Juss."] <- "Sapindaceae"
Vdata$tree$Family[Vdata$tree$Family=="Sapotaceae Juss."] <- "Sapotaceae"
Vdata$tree$Family[Vdata$tree$Family=="Staphylaceae"] <- "Staphyleaceae"
Vdata$tree$Family[Vdata$tree$Family=="Torrecilliaceae"] <- "Unknown"
Vdata$tree$Family[Vdata$tree$Family=="Urticaceae Juss."] <- "Urticaceae"
# Duplicate liana genera 
Vdata$liana$Genus[Vdata$liana$Genus=="Adenocalymna"] <- "Adenocalymma"
Vdata$liana$Genus[Vdata$liana$Genus=="Agleaea"] <- "Agelaea"
Vdata$liana$Genus[Vdata$liana$Genus=="Arrabidea"] <- "Arrabidaea"
Vdata$liana$Genus[Vdata$liana$Genus=="Davilia"] <- "Davilla"
Vdata$liana$Genus[Vdata$liana$Genus=="Heteropteris"] <- "Heteropterys"
Vdata$liana$Genus[Vdata$liana$Genus=="Hiprocatea"] <- "Hippocratea"
Vdata$liana$Genus[Vdata$liana$Genus=="Prionostema"] <- "Prionostemma"
Vdata$liana$Genus[Vdata$liana$Genus=="UNKNOWN"] <- "Unknown"
# Liana misspellings
Vdata$liana$Genus[Vdata$liana$Genus=="Ampleocissus"] <- "Ampelocissus"
Vdata$liana$Genus[Vdata$liana$Genus=="Williugbeia"] <- "Willughbeia"
# Liana families
Vdata$liana$Family[Vdata$liana$Family=="Amaranthaceae Juss."] <- "Amaranthaceae"
Vdata$liana$Family[Vdata$liana$Family=="Bignoniaceae Juss."] <- "Bignoniaceae"
Vdata$liana$Family[Vdata$liana$Family=="Cheiloclinium"] <- "Celastraceae"
Vdata$liana$Family[Vdata$liana$Family=="Menispermecae"] <- "Menispermaceae"
Vdata$liana$Family[Vdata$liana$Family=="UNKNOWN"] <- "Unknown"

# Inspect (but do not remove) stems with DBH below 10 cm
#Vdata$tree[Vdata$tree$Diameter>=10,]
#Vdata$liana[Vdata$liana$Diameter>=10,]

Trees <- Vdata$tree

#Remove dead trees
aliveT <- grep(pattern="K", x=Trees$ConditionCodes, invert=TRUE)
Trees <- Trees[aliveT,]

# Add decimal KRP plot with inflated diameters until issue is resolved in the database
Trees$Diameter[Trees$"1haPlotNumber"=="VG-KRP-1" & Trees$SamplingPeriod=="2011.01"] <- Trees$Diameter[Trees$"1haPlotNumber"=="VG-KRP-1"& Trees$SamplingPeriod=="2011.01"]/10

# Add decimal to 2012 BCI stems with inflated diameters (all >100) until issue is resolved in the database

# Add decimal to 6 stems in plot VG-COU-5 that have inflated diameters until issue is resolved in the database
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==392] <- 39.2
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==525] <- 52.5
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==564] <- 56.4
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==603] <- 60.3
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==723] <- 72.3
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter==1120] <- 112.0
# Add decimal to outlier diameter in plot VG-YAS-1
Trees$Diameter[Trees$"1haPlotNumber"=="VG-YAS-1" & Trees$Diameter==420] <- 42.0
# Add decimal to outlier (999) values in plots VG-CSN-1 and VG-CSN-5
Trees$Diameter[Trees$"1haPlotNumber"=="VG-CSN-1" & Trees$Diameter==999] <- 99.9
Trees$Diameter[Trees$"1haPlotNumber"=="VG-CSN-5" & Trees$Diameter==999] <- 99.9

# Check to be sure that no other years had outlier values for these stems
Trees$Diameter[Trees$"1haPlotNumber"=="VG-COU-5" & Trees$Diameter>300] 
############### END DATA CLEANING ###############

############### BEGIN CARBON CALCULATIONS ######
# Data Exploration
# Examine plots at each site
#table(Vtrees$"1haPlotNumber", Vtrees$Site.CodeT)
#table(Vlianas$"1haPlotNumber", Vlianas$Site.CodeL)

# Inspect % of trees identified to family level in each plot
PlotFamilyStemsT <- table(Trees$"1haPlotNumber", Trees$Family)
TotalStems <- rowSums(PlotFamilyStemsT)
FamilyUnknown <- PlotFamilyStemsT[,colnames(PlotFamilyStemsT)=="Unknown"]
PercentUnknown <- (FamilyUnknown/TotalStems)*100
ExcludePlots <- PercentUnknown[PercentUnknown>20]


# Import wood density data
WDdata <- read.csv("GlobalWoodDensityData.csv")
WDdata <- WDdata[,2:5]
Genus <- WDdata$Binomial
Genus <- sub(" .*","", Genus)
WDdata <- cbind(WDdata, Genus)

# Calculate genus and family level wood densities
gWD <- aggregate(WDdata$WD, by=list(WDdata$Genus), FUN=mean)
names(gWD) <- c("Genus", "WD")

fWD <- aggregate(WDdata$WD, by=list(WDdata$Family), FUN=mean)
names(fWD) <- c("Family", "WD")

# Extract WD values for genera in TEAM plots
genusWD <- gWD$WD[match(colnames(PlotGenusStemsT), gWD$Genus)]
names(genusWD) <- colnames(PlotGenusStemsT)

# Fill in missing genus values with family wood density value
# Families for each genus
families <- Trees$Family[match(sort(unique(Trees$Genus)), Trees$Genus)]
WD <- ifelse(is.na(genusWD)==TRUE, fWD$WD[match(families, fWD$Family)], genusWD)

nsites <- length(levels(Vtraits$Site.CodeT))

# Assign sites to "dry" (<1500 mm/year precipitation) or "moist" (1500-3500 mm/year) designation for carbon storage calculations
# UDZ and BIF are dry; all others moist according to TEAM CRU rain data

# Match WD to all Trees in dataset based on genus names
StemWD <- WD[match(Trees$Genus, names(WD))]
Trees <- cbind(Trees, StemWD)

# Calculate biomass for individual trees using the following equations:
#ABGdry <- WD * exp((-2/3) + 1.784*ln(D) + 0.207*ln(D)^2 - 0.0281*ln(D)^3)
#ABGmoist <- WD * exp(-1.499 + 2.148*ln(D) + 0.207*ln(D)^2 - 0.0281*ln(D)^3)
# For missing values (neither genus nor family available), use plot mean


# Inspect sampling periods for each site
table(Trees$Site.CodeT, Trees$SamplingPeriod)
# Drop second sampling period from 2005
Trees <- Trees[Trees$SamplingPeriod!="2005.02", ]
# Create column for each Site and Sampling.Period to aggregate on
SiteYear <- as.factor(paste(Trees$Site.CodeT, Trees$SamplingPeriod, sep="."))
PlotSiteYear <- as.factor(paste(Trees$Site.CodeT, Trees$SamplingPeriod, Trees$"1haPlotNumber", sep="."))
#Trees <- cbind(Trees, SiteYear)
Trees <- cbind(Trees, PlotSiteYear)

# Calculate above ground biomass using Chave et al. 2005 equations
ABG <- ifelse(Trees$Site.CodeT=="UDZ" | Trees$Site.CodeT=="BIF", 
              Trees$StemWD * exp((-2/3) + 1.784 * log(Trees$Diameter) + 0.207 * log(Trees$Diameter)^2 - 0.0281 * log(Trees$Diameter)^3),
              Trees$StemWD * exp(-1.499 + 2.148 * log(Trees$Diameter) + 0.207 * log(Trees$Diameter)^2 - 0.0281 * log(Trees$Diameter)^3))
Trees <- cbind(Trees, ABG)
#plotyearABG <- aggregate(Trees$ABG ~ Trees$"1haPlotNumber" + Trees$SiteYear, FUN=mean, na.omit=TRUE)

plotyearABG <- aggregate(Trees$ABG ~ Trees$PlotSiteYear, FUN=mean, na.omit=TRUE)
names(plotyearABG) <- c("PlotSiteYear", "ABG")
#allABG <- ifelse(is.na(Trees$ABG)==TRUE, plotABG$ABG[match(Trees$"1haPlotNumber", plotABG$Plot)], Trees$ABG)
allABG <- ifelse(is.na(Trees$ABG)==TRUE, plotyearABG$ABG[match(Trees$PlotSiteYear, plotyearABG$PlotSiteYear)], Trees$ABG)

Trees$ABG <- allABG
# Multiply ABG by 0.5 to convert to carbon storage following Chave et al. 2005
Cstorage <- Trees$ABG*0.5
Trees <- cbind(Trees, Cstorage)

Cplot <- aggregate(Trees$Cstorage ~ Trees$PlotSiteYear, FUN=sum, na.omit=TRUE)
names(Cplot) <- c("PlotSiteYear", "Cstorage")

write.table(Cplot, "CarbonAllSitesAllYears.csv", sep=",", row.names=FALSE)

###################  END 2005 CARBON CALCULATIONS ####################




################### CODE FOR CHAVE ET AL. 2014 METHODS #####################

# If desired, use updated equations from Chave et al. In Press from Global Change Biology to re-calculate carbon storage
# Data for "E" layer can be downloaded from http://chave.ups-tlse.fr/chave/pantropical_allometry.htm or extracted by:
#source("http://chave.ups-tlse.fr/chave/pantropical/readlayers.r")
#LatLon <- read.csv(file="SiteLatitudeLongitude.csv")
#coord <- cbind(LatLon$Longitude, LatLon$Latitude)
#colnames(coord) <- c("longitude", "latitude")
#E_value <- retrieve_raster("E",coord)


# Match E value to each stem in Trees to use in calculation
E <- E_value$E[match(Trees$Site.CodeT, E_value$Site.Code)]
Trees <- cbind(Trees, E)

#Equation 7: exp( -1.803 - 0.976*E_value$E + 0.976*ln(WD) + 2.673*ln(D) - 0.0299*(ln(D))^2)
ABG2 <- exp(-1.803 - 0.976*Trees$E + 0.976*log(Trees$StemWD) + 2.673*log(Trees$Diameter) - 0.0299*(log(Trees$Diameter))^2)

Trees <- cbind(Trees, ABG2)
plotABG2 <- aggregate(Trees$ABG2 ~ Trees$"1haPlotNumber", FUN=mean, na.omit=TRUE)
names(plotABG2) <- c("Plot", "ABG2")
allABG2 <- ifelse(is.na(Trees$ABG2)==TRUE, plotABG2$ABG2[match(Trees$"1haPlotNumber", plotABG2$Plot)], Trees$ABG2)
Trees$ABG2 <- allABG2
Cstorage2 <- Trees$ABG2*0.5
Trees <- cbind(Trees, Cstorage2)

Cplot2 <- aggregate(Trees$Cstorage2 ~ Trees$"1haPlotNumber", FUN=sum, na.omit=TRUE)
names(Cplot2) <- c("Plot", "Cstorage2")




# Exclude plots that have less than 80% of stems identified to family level
ExcludePlots <- names(ExcludePlots)
plant.covs <- plant.covs[-na.omit(match(ExcludePlots, plant.covs$plot)),]

CV <- function(data){
  sd(data)/mean(data)
}

aggregate(plant.covs$Cstorage ~ plant.covs$PlotCodes, FUN=mean)
aggregate(plant.covs$Cstorage ~ plant.covs$PlotCodes, FUN=CV)



