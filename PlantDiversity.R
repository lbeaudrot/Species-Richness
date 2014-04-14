# Calculate plant metrics for plants-animals project
# Above Ground Biomass
# Species Richness
# Species Diversity
# Functional Diversity 


# Query vegetation data from database
#Vegdata <- f.teamdb.query("vegetation")
Vdata <- Vegdata

# Add column with site codes
Site.CodeT <- substr(Vdata$tree$"1haPlotNumber",4,6)
Site.CodeL <- substr(Vdata$liana$"1haPlotNumber",4,6)

Vdata <- list(cbind(Vdata$tree, Site.CodeT), cbind(Vdata$liana, Site.CodeL))
names(Vdata) <- c("tree", "liana")
# Clean genus names for trees and lianas
#Vdataclean <- table(V$tree$Genus)
#write.table(Vdataclean, file="Vdataclean.csv", sep=",")
#Vdataclean.liana <- table(Vdata$liana$Genus)
#write.table(Vdataclean.liana, file="Vdataclean.liana.csv", sep=",")

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


# Inspect (but do not remove) stems with DBH below 10 cm
#Vdata$tree[Vdata$tree$Diameter>=10,]
#Vdata$liana[Vdata$liana$Diameter>=10,]

# Limit data to one year from each site so that data correspond to CT metrics
# 2012 for CAX, YAS, and PSH
# 2011 for all other sites

Trees <-  rbind(Vtrees[Vtrees$Site.CodeT=="CAX" & Vtrees$SamplingPeriod=="2012.01",],
          Vtrees[Vtrees$Site.CodeT=="YAS" & Vtrees$SamplingPeriod=="2012.01",],
          Vtrees[Vtrees$Site.CodeT=="PSH" & Vtrees$SamplingPeriod=="2012.01",],
          Vtrees[Vtrees$Site.CodeT=="BBS" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="BCI" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="BIF" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="COU" & Vtrees$SamplingPeriod=="2011.01",],
          #Vtrees[Vtrees$Site.CodeT=="CSN" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="KRP" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="MAS" & Vtrees$SamplingPeriod=="2011.01",],
          #Vtrees[Vtrees$Site.CodeT=="NAK" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="NNN" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="RNF" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="UDZ" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="VB-" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="YAN" & Vtrees$SamplingPeriod=="2011.01",])
#Remove rows with missing unique identification values (NA rows)
Trees <- Trees[is.na(Trees$Id)==FALSE,]
#Remove dead trees
aliveT <- grep(pattern="K", x=Trees$ConditionCodes, invert=TRUE)
Trees <- Trees[aliveT,]

Lianas <-  rbind(Vlianas[Vlianas$Site.CodeL=="CAX" & Vlianas$SamplingPeriod=="2012.01",],
                 Vlianas[Vlianas$Site.CodeL=="YAS" & Vlianas$SamplingPeriod=="2012.01",],
                 Vlianas[Vlianas$Site.CodeL=="PSH" & Vlianas$SamplingPeriod=="2012.01",],
                 Vlianas[Vlianas$Site.CodeL=="BBS" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="BCI" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="BIF" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="COU" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="CSN" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="KRP" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="MAS" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="NAK" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="NNN" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="RNF" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="UDZ" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="VB-" & Vlianas$SamplingPeriod=="2011.01",],
                 Vlianas[Vlianas$Site.CodeL=="YAN" & Vlianas$SamplingPeriod=="2011.01",])
#Remove rows with missing unique identification values (NA rows)
Lianas <- Lianas[is.na(Lianas$Id)==FALSE,]
#Remove dead lianas
aliveL <- grep(pattern="K", x=Lianas$ConditionCodes, invert=TRUE)
Lianas <- Lianas[aliveL,]

############### END DATA CLEANING ###############

############### BEGIN PLANT DIVERSITY CALCULATIONS ######
# Data Exploration
# Examine plots at each site
table(Vtrees$"1haPlotNumber", Vtrees$Site.CodeT)
table(Vlianas$"1haPlotNumber", Vlianas$Site.CodeL)

# CALCULATE PLOT-LEVEL PLANT DIVERSITY METRICS
# Once complete, combine into a dataframe to calculate site averages and site variability

# Format data with plots as rows and Genera as columns
# Remove unknown stems (for diversity calculations only)
PlotGenusStemsT <- table(Trees$"1haPlotNumber", Trees$Genus)
PlotGenusStemsT <- PlotGenusStemsT[,colnames(PlotGenusStemsT)!="Unknown"]
PlotGenusStemsL <- table(Lianas$"1haPlotNumber", Lianas$Genus)
PlotGenusStemsL <- PlotGenusStemsL[,colnames(PlotGenusStemsL)!="Unknown"]

# Calculate GENUS RICHNESS for each plot at each site for trees and for lianas
TRich <- rowSums(ifelse(PlotGenusStemsT>0,1,0))
LRich <- rowSums(ifelse(PlotGenusStemsL>0,1,0))

# Calculate GENUS TAXONOMIC DIVERSITY (SHANNON INDEX) for each plot at each site for trees and lianas
library(vegan)
TShan <- diversity(PlotGenusStemsT, index="shannon", MARGIN=1, base=exp(1))
LShan <- diversity(PlotGenusStemsL, index="shannon", MARGIN=1, base=exp(1))


# Create dataframe with plot level richness and diversity values
Richness <- merge(TRich, LRich, by="row.names", all=TRUE)
names(Richness) <- c("Plot", "TRich", "LRich")
Diversity <- merge(TShan, LShan, by="row.names", all=TRUE)
names(Diversity) <- c("Plot", "TShan", "LShan")

Plot.RD <- merge(Richness, Diversity, by="Plot", all=TRUE)
#write.table(Plot.RD, file="Plot.RD.csv", col.names=TRUE, sep=",")

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

# Determine the maxium diameter value for a genus at a site from data used in this study
maxD <- aggregate(Trees$Diameter, by=list(Trees$Site.CodeT, Trees$Genus), FUN=max)
names(maxD) <- c("Site.CodeT", "Genus", "maxD")

# Remove unknown genera
maxD <- maxD[maxD$Genus!="Unknown",]


# Match wood density values to site specific maximum diameter values for each genus
WD2 <- WD[match(maxD$Genus, names(WD))]


# Format functional trait data for FD calculations
# function dbFD requires a data frame of functional traits with genus row names
# Note that CSN and NAK have very low genus trait value richness (16 and 0, respectively); discard

Vtraits <- cbind(maxD, WD2)
nsites <- length(levels(Vtraits$Site.CodeT))




#Extract sites by looping (needs work)
Vsites <-list()
for(i in 1:nsites)
#  Vsites[[i]]<-f.matrix.creator2(data,year[i])  
#spnames<-names(mat[[1]])

for(i in levels(Vtraits$Site.CodeT)){
  Vsites[[i]] <- Vtraits[Vtraits$Site.CodeT==levels(Vtraits$Site.CodeT)[i],]
  rownames(Vsites[[i]]) <- Vsites[[i]]$Genus
           Vsites
}

# Extract sites manually
BBS <- Vtraits[Vtraits$Site.CodeT=="BBS",]
rownames(BBS) <- BBS$Genus
BBS <- BBS[,3:4]

BCI <- Vtraits[Vtraits$Site.CodeT=="BCI",]
rownames(BCI) <- BCI$Genus
BCI <- BCI[,3:4]

BIF <- Vtraits[Vtraits$Site.CodeT=="BIF",]
rownames(BIF) <- BIF$Genus
BIF <- BIF[,3:4]

CAX <- Vtraits[Vtraits$Site.CodeT=="CAX",]
rownames(CAX) <- CAX$Genus
CAX <- CAX[,3:4]

COU <- Vtraits[Vtraits$Site.CodeT=="COU",]
rownames(COU) <- COU$Genus
COU <- COU[,3:4]

CSN <- Vtraits[Vtraits$Site.CodeT=="CSN",]
rownames(CSN) <- CSN$Genus
CSN <- CSN[,3:4]

KRP <- Vtraits[Vtraits$Site.CodeT=="KRP",]
rownames(KRP) <- KRP$Genus
KRP <- KRP[,3:4]

MAS <- Vtraits[Vtraits$Site.CodeT=="MAS",]
rownames(MAS) <- MAS$Genus
MAS <- MAS[,3:4]

NAK <- Vtraits[Vtraits$Site.CodeT=="NAK",]
rownames(NAK) <- NAK$Genus
NAK <- NAK[,3:4]

NNN <- Vtraits[Vtraits$Site.CodeT=="NNN",]
rownames(NNN) <- NNN$Genus
NNN <- NNN[,3:4]

PSH <- Vtraits[Vtraits$Site.CodeT=="PSH",]
rownames(PSH) <- PSH$Genus
PSH <- PSH[,3:4]

RNF <- Vtraits[Vtraits$Site.CodeT=="RNF",]
rownames(RNF) <- RNF$Genus
RNF <- RNF[,3:4]

UDZ <- Vtraits[Vtraits$Site.CodeT=="UDZ",]
rownames(UDZ) <- UDZ$Genus
UDZ <- UDZ[,3:4]

VB- <- Vtraits[Vtraits$Site.CodeT=="VB-",]
rownames(VB-) <- VB-$Genus
VB- <- VB-[,3:4]

YAN <- Vtraits[Vtraits$Site.CodeT=="YAN",]
rownames(YAN) <- YAN$Genus
YAN <- YAN[,3:4]

YAS <- Vtraits[Vtraits$Site.CodeT=="YAS",]
rownames(YAS) <- YAS$Genus
YAS <- YAS[,3:4]

[1] "BBS" "BCI" "BIF" "CAX" "COU" "CSN" "KRP" "MAS" "NAK" "NNN" "PSH" "RNF" "UDZ" "VB-" "YAN" "YAS"

# Calculate GENUS FUNCTIONAL DIVERSITY for each site (need to re-run for each plot?)
library(FD)
TFunc.BBS <- dbFD(BBS, corr="cailliez")
TFunc.BCI <- dbFD(BCI, corr="sqrt")
TFunc.BIF <- dbFD(BIF, corr="cailliez")
TFunc.CAX <- dbFD(CAX, corr="sqrt")
TFunc.COU <- dbFD(COU, corr="cailliez")
#TFunc.CSN <- dbFD(CSN, corr="sqrt")
TFunc.KRP <- dbFD(KRP, corr="cailliez")
TFunc.MAS <- dbFD(MAS, corr="sqrt")
#TFunc.NAK <- dbFD(NAK, corr="sqrt")
TFunc.NNN <- dbFD(NNN, corr="cailliez")
TFunc.PSH <- dbFD(PSH, corr="cailliez")
TFunc.RNF <- dbFD(RNF, corr="sq")
TFunc.UDZ <- dbFD(UDZ)
TFunc.VB- <- dbFD(VB-)
TFunc.YAN <- dbFD(YAN)
TFunc.YAS <- dbFD(YAS)



