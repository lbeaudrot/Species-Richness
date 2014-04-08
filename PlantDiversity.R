# Calculate plant metrics for plants-animals project
# Above Ground Biomass
# Species Richness
# Species Diversity
# Functional Diversity 


# Query vegetation data from database
#Vdata <- f.teamdb.query("vegetation")
#Vegdata <- Vdata

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
Vdata$liana$Genus[Vdata$liana$Genus==""] <- ""
Vdata$liana$Genus[Vdata$liana$Genus==""] <- ""






