# Calculate plant metrics for plants-animals project
# Above Ground Biomass
# Species Richness
# Species Diversity
# Functional Diversity 


# Query vegetation data from database
#Vegdata <- f.teamdb.query("vegetation")
#Vdata <- Vegdata

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

# Manually clean data (prior to database cleaning)
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


Vdata$liana$Genus[Vdata$liana$Genus=="Adenocalymma"] <- "Adenocalymna"
Vdata$liana$Genus[Vdata$liana$Genus=="Agleaea"] <- "Agelaea"
Vdata$liana$Genus[Vdata$liana$Genus=="Arrabidea"] <- "Arrabidaea"
Vdata$liana$Genus[Vdata$liana$Genus=="Davilia"] <- "Davilla"
Vdata$liana$Genus[Vdata$liana$Genus=="Heteropteris"] <- "Heteropterys"
Vdata$liana$Genus[Vdata$liana$Genus=="Hiprocatea"] <- "Hippocratea"
Vdata$liana$Genus[Vdata$liana$Genus=="Prionostema"] <- "Prionostemma"
Vdata$liana$Genus[Vdata$liana$Genus=="UNKNOWN"] <- "Unknown"

# Remove stems with DBH below 10 cm

Vtrees <- Vdata$tree[Vdata$tree$Diameter>=10,]
Vlianas<- Vdata$liana[Vdata$liana$Diameter>=10,]

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
          Vtrees[Vtrees$Site.CodeT=="CSN" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="KRP" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="MAS" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="NAK" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="NNN" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="RNF" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="UDZ" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="VB-" & Vtrees$SamplingPeriod=="2011.01",],
          Vtrees[Vtrees$Site.CodeT=="YAN" & Vtrees$SamplingPeriod=="2011.01",])

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

############### END DATA CLEANING ###############

# Data Exploration

# Examine plots at each site
table(Vtrees$"1haPlotNumber", Vtrees$Site.CodeT)
table(Vlianas$"1haPlotNumber", Vlianas$Site.CodeL)

# CALCULATE PLOT-LEVEL PLANT DIVERSITY METRICS
# Once complete, combine into a dataframe to calculate site averages and site variability

# Calculate GENUS RICHNESS for each plot at each site for trees and for lianas

TRich <- colSums((ifelse(table(Vtrees$Genus, Vtrees$"1haPlotNumber")>0,1,0)))
LRich <- colSums((ifelse(table(Vlianas$Genus, Vlianas$"1haPlotNumber")>0,1,0)))

