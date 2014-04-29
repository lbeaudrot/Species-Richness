# Modeling terrestrial vertebrate species richness and functional diversity as a function of site level vegetation properties and carbon storage

#Use PlantDiversity.R to obtain TEAM site level plant covariates.
#See objects plot.VGmean and plot.VGvar for mean and variance of plant covariates
#plot.VGmean <- cbind(rownames(plot.VGmean), plot.VGmean)
#colnames(plot.VGmean)[1] <- "Site.Code"
#Alternatively, load covariates from csv file
plot.VGmean <- read.csv(file="PlantDiversityCalculations.csv")
plot.VGvar <- read.csv(file="PlantDiversityVariances.csv")

# MERGE MAMMAL AND BIRD DATA
#Use EstimateSpeciesRichness.R and/or RichnessLoop.R to obtain TEAM site level mean, median and mode of terrestrial vertebrate species richness estimates
#Use FunctionalDiversity.R to obtain animal functional and taxonomic diversity

# Merge CT and Veg data into a single table
#CTaverages <- cbind(rownames(CTaverages), CTaverages)
#colnames(CTaverages)[1] <- "Site.Code"

CTaverages <- read.csv("CTaverages_mammal.csv")
MammalFD <- read.csv("FunctionalDiversity_Mammals_29April2014.csv")
model.data <- merge(CTaverages, MammalFD, by.x="Site.Code", by.y="Site.Code", all=FALSE)

CTaveragesB <- read.csv("CTaverages_bird.csv")
BirdFD <- read.csv("FunctionalDiversity_Birds_29April2014.csv")
model.data <- merge(model.data, CTaveragesB, by.x="Site.Code", by.y="Site.Code", all=FALSE)
model.data <- merge(model.data, BirdFD, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# MERGE ANIMAL DATA WITH PLANT DATA
model.data <- merge(model.data, plot.VGmean, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# Calculate precipitation CV and add rainfall data to model.data
rain.data <- read.csv(file="Precipitation_Data.csv")
mo.rain <- as.matrix(rain.data[,2:13])
rain.CV <- vector()
for(i in 1:dim(mo.rain)[1]){
  rain.CV[i] <- sd(mo.rain[i,])/mean(mo.rain[i,])
  rain.CV
}
rain <- data.frame(rain.data$Site.Code, rain.data$total, rain.CV)
colnames(rain) <- c("Site.Code", "RainTotal", "Rain.CV")
model.data <- merge(model.data, rain, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# Define CV function
CV <- function(data){
  sd(data)/mean(data)
}

# Calculate ELEVATION CV and add elevation to model.data
elevation.data <- read.csv("CT_edgedist_elevation_final.csv") 
elevation.mean <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=mean)
elevation.CV <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=CV)[2]
elevation <- cbind(elevation.mean, elevation.CV)
colnames(elevation) <- c("Site.Code", "Elev.Mean", "Elev.CV")
model.data <- merge(model.data, elevation, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# ADD LATITUDE for each TEAM site as a covariate - see EstimateSpeciesRichness.R line 203
# Extract mean CT latitude for each TEAM site to include as a covariate 
#traps <- eventsdata[!duplicated(eventsdata$Sampling.Unit.Name),]
#Latitude <- aggregate(traps$Latitude ~ traps$Site.Code, FUN=mean)
#colnames(Latitude) <- c("Site.Code", "Latitude")
Latitude <- read.csv("Latitude_MeanSiteCT.csv")
model.data <- merge(model.data, Latitude, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# Format merged continuous data as data frame with numeric values and site codes as row names
ModelData <- model.data
rownames(ModelData) <- ModelData$Site.Code
ModelData <- ModelData[,-1]
MData <- matrix(NA, dim(ModelData)[1], dim(ModelData)[2])
for(i in 1:dim(ModelData)[2])  {
  MData[,i] <- as.numeric(as.character(ModelData[,i]))
  MData
}
MData <- as.data.frame(MData)
colnames(MData) <- colnames(ModelData)

# Scale predictor variables
MData <- cbind(MData[,1:24], scale(MData[,25:dim(MData)[2]]))
rownames(MData) <- model.data$Site.Code

# Add categorical variables for random effects
Year <- c(2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012)
Continent1 <- c("Asia", "America", "America", "America", "Africa", "America", "Africa", "Asia", "Africa", "Africa", "America", "America", "America")
Continent2 <- c("Asia", "America", "America", "America", "Africa", "America", "Africa", "Asia", "Madagascar", "Africa", "America", "America", "America")

Mdata <- cbind(MData, Year, Continent1, Continent2)

# Create output table of predictor and response variables for inclusion in paper
output.table <- cbind(model.data, Year, Continent1)
output.table <- merge(output.table, plot.VGvar, by.x="Site.Code", by.y="Site.Code", all=FALSE)
write.csv(output.table, file="Table_PredictorResponseVariables.csv", row.names=FALSE)

################################## END DATA FORMATTING ###########################

# Function to visualize pairwise comparisons
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

## VISUALIZE PREDICTOR CORRELATIONS

pairs(plot.VGmean[,2:12], lower.panel = panel.smooth, upper.panel = panel.cor)

pdf(file="PAIRS_Vegetation.pdf")
  pairs(plot.VGmean[,2:12], lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off()

pairs.data <- MData[,-7]
pairs.data <- pairs.data[,-19]
pairs.data <- pairs.data[,-16]
pairs.data <- pairs.data[,-4]

pdf(file="PAIRS_Mammals.pdf")
  pairs(pairs.data[,1:10], lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off()

pdf(file="PAIRS_Birds.pdf")
  pairs(pairs.data[,11:20], lower.panel = panel.smooth, upper.panel = panel.cor)
dev.off()

PairsCOR <- round(cor(MData[,1:19]), digits=2)
PairsCOR[lower.tri(PairsCOR)] <- NA
diag(PairsCOR) <- NA
PairsCOR
ifelse(abs(PairsCOR)>0.5, PairsCOR, NA)

# VISUALIZE species richness across sites
# MAMMAL RICHNESS
library("fields")
set.panel(3,3)
par(mar=c(2,2,1,1))
hist(Mdata$CT.mean, main="Mean")
hist(Mdata$CT.median, main="Median")
hist(Mdata$CT.mode, main="Mode")
boxplot(Mdata$CT.mean~Mdata$Continent1, ylim=c(0,40))
boxplot(Mdata$CT.median~Mdata$Continent1, ylim=c(0,40))
boxplot(Mdata$CT.mode~Mdata$Continent1, ylim=c(0,40))
boxplot(Mdata$CT.mean~Mdata$Continent2, ylim=c(0,40))
boxplot(Mdata$CT.median~Mdata$Continent2, ylim=c(0,40))
boxplot(Mdata$CT.mode~Mdata$Continent2, ylim=c(0,40))
set.panel()

###### MODEL Terrestrial Vertebrate SPECIES RICHNESS

library(lme4)
library(MASS)

fit1 <- lm(CT.mode ~ V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT + abs(Latitude)*Elev.CV, data=Mdata)
step1 <- stepAIC(fit1, direction="both")
bestfit1 <- lm(CT.mode ~ V.Cstorage + V.FDis + V.NStemsT + V.NStemsL + Rain.CV + 
                 abs(Latitude) + Elev.CV + V.NStemsT:abs(Latitude), data=Mdata)
summary(bestfit1)

fit2 <- lm(CT.median ~ V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT + abs(Latitude)*Elev.CV, data=Mdata)
step2 <- stepAIC(fit2, direction="both")
bestfit2 <- lm(CT.median ~ V.Cstorage + V.FDis + V.NStemsT + V.NStemsL + Rain.CV + 
                 abs(Latitude) + V.TRich + V.NStemsT:abs(Latitude), data=Mdata)
summary(bestfit2)

fit3 <- lmer(CT.mode ~ (1|Continent1) + V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT, data=Mdata)
fit4 <- lmer(CT.median ~ (1|Continent1) + V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT, data=Mdata)

AIC(fit1, fit2, fit3, fit4)

# VISUALIZE MAMMAL FUNCTIONAL DIVERSITY
set.panel(3,2)
par(mar=c(3,2,2,1))
hist(Mdata$CT.FDis, main="Mammal Functional Dispersion")
hist(Mdata$CT.RaoQ, main="Mammal Rao's Quadratic Entropy")
boxplot(Mdata$CT.FDis~Mdata$Continent1, ylim=c(0,0.5))
boxplot(Mdata$CT.RaoQ~Mdata$Continent1, ylim=c(0,0.5))
boxplot(Mdata$CT.FDis~Mdata$Continent2, ylim=c(0,0.5))
boxplot(Mdata$CT.RaoQ~Mdata$Continent2, ylim=c(0,0.5))
set.panel()

###### MODEL MAMMAL Vertebrate FUNCTIONAL DIVERSITY
fit5 <- lm(CT.FDis ~ V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT + abs(Latitude)*Elev.CV, data=MData)
step5 <- stepAIC(fit5, direction="both")
bestfit5 <- lm(CT.FDis ~ V.Cstorage + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + 
                 Elev.CV + abs(Latitude) + V.NStemsT:abs(Latitude) + Elev.CV:abs(Latitude), data=Mdata)

fit6 <- lmer(CT.mode ~ (1|Continent1) + V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT, data=Mdata)

AIC(bestfit5, fit6)

###### VISUALIZE BIRD Vertebrate FUNCTIONAL DIVERSITY
# BIRDS
set.panel(3,2)
par(mar=c(3,2,2,1))
hist(Mdata$B.CT.FDis, main="Bird Functional Dispersion")
hist(Mdata$B.CT.RaoQ, main="Bird Rao's Quadratic Entropy")
boxplot(Mdata$B.CT.FDis~Mdata$Continent1, ylim=c(0,0.5))
boxplot(Mdata$B.CT.RaoQ~Mdata$Continent1, ylim=c(0,0.5))
boxplot(Mdata$B.CT.FDis~Mdata$Continent2, ylim=c(0,0.5))
boxplot(Mdata$B.CT.RaoQ~Mdata$Continent2, ylim=c(0,0.5))
set.panel()

###### MODEL BIRD Vertebrate FUNCTIONAL DIVERSITY

fit7 <- lm(B.CT.FDis ~ V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT + abs(Latitude)*Elev.CV, data=MData)
step7 <- stepAIC(fit7, direction="both")
bestfit7 <- lm(B.CT.FDis ~ V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + 
                 Rain.CV + Elev.CV, data=Mdata)

fit8 <- lmer(B.CT.RaoQ ~ (1|Continent1) + V.Cstorage + V.FDis + V.TRich + V.NStemsT + V.NStemsL + Rain.CV + Elev.CV + abs(Latitude) + abs(Latitude)*V.NStemsT, data=Mdata)

AIC(bestfit7, fit8)

###### VISUALIZE Terrestrial Vertebrate TAXONOMIC DIVERSITY
set.panel(3,2)
par(mar=c(3,2,2,1))
hist(Mdata$CT.Shannon, main="Mammal Taxonomic Diversity")
hist(Mdata$B.CT.Shannon, main="Bird Taxonomic Diversity")
boxplot(Mdata$CT.Shannon~Mdata$Continent1, ylim=c(0,3))
boxplot(Mdata$B.CT.Shannon~Mdata$Continent1, ylim=c(0,3))
boxplot(Mdata$CT.Shannon~Mdata$Continent2, ylim=c(0,3))
boxplot(Mdata$B.CT.Shannon~Mdata$Continent2, ylim=c(0,3))
set.panel()

###### MODEL Terrestrial Vertebrate TAXONOMIC DIVERSITY
