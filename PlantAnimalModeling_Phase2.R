# Modeling terrestrial vertebrate species richness and functional diversity as a function of site level vegetation properties and carbon storage
# For phase two, keep analysis at the site level for all variables 
# In phase three, change analysis to plot level for carbon and plant variables using plant.covs object or file "PlantDiversityCalculations_PlotLevel.csv)

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

CTaverages <- read.csv("CTaverages_overall.csv")
AnimalFD <- read.csv("FunctionalDiversity_Overall_5May2014.csv")
model.data <- merge(CTaverages, AnimalFD, by.x="Site.Code", by.y="Site.Code", all=TRUE)

ShannonDist <- read.csv("ShannonIndex_Distribution.csv")
model.data <- merge(model.data, ShannonDist, by.x="Site.Code", by.y="Site.Code", all=FALSE)

CT.FDisDist <- read.csv("FunctionalDiversity_Overall_Distribution_8May2014.csv")
model.data <- merge(model.data, CT.FDisDist, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# MERGE ANIMAL DATA WITH PLANT DATA
model.data <- merge(model.data, plot.VGmean, by.x="Site.Code", by.y="Site.Code", all=TRUE)

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

elev.range <- function(data){
  max(data) - min(data)
}
# Calculate ELEVATION CV and add elevation to model.data
elevation.data <- read.csv("CT_edgedist_elevation_final.csv") 
elevation.mean <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=mean)
elevation.CV <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=CV)[2]
elevation.range <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=elev.range)[2]
elevation <- cbind(elevation.mean, elevation.CV, elevation.range)
colnames(elevation) <- c("Site.Code", "Elev.Mean", "Elev.CV", "Elev.Range")
model.data <- merge(model.data, elevation, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# ADD LATITUDE for each TEAM site as a covariate - see EstimateSpeciesRichness.R line 203
# Extract mean CT latitude for each TEAM site to include as a covariate 
#traps <- eventsdata[!duplicated(eventsdata$Sampling.Unit.Name),]
#Latitude <- aggregate(traps$Latitude ~ traps$Site.Code, FUN=mean)
#colnames(Latitude) <- c("Site.Code", "Latitude")
Latitude <- read.csv("Latitude_MeanSiteCT.csv")
Latitude$Latitude <- abs(Latitude$Latitude)
model.data <- merge(model.data, Latitude, by.x="Site.Code", by.y="Site.Code", all=FALSE)


# ADD FOREST LOSS data from Alex Zvoleff's calculations
ForestLoss <- read.csv("GFC_Forest_Change_Summary.csv")
names(ForestLoss) <- c("Site.Code", "ForestLossSite", "ForestLossZOI", "PA_area", "SA_area", "ZOI_area")
model.data <- merge(model.data, ForestLoss, by.x="Site.Code", by.y="Site.Code", all=FALSE)


# PERFORM TRANSFORMATIONS OF PREDICTOR VARIABLES 
#model.data$Latitude <- log(model.data$Latitude)
#model.data$Elev.CV <- log(model.data$Elev.CV )
#model.data$V.Cstorage <- log(model.data$V.Cstorage)
#model.data$V.NStemsT <- log(model.data$V.NStemsT)
#model.data$ForestLossZOI <- log(model.data$ForestLossZOI)
#model.data$PA_area <- log(model.data$PA_area)
#model.data$SA_area <- log(model.data$SA_area)
#model.data$ZOI_area <- log(model.data$ZOI_area)
model.data$CT.FDiv <- rep(0, dim(model.data)[1])

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
MData <- cbind(MData[,1:15], scale(MData[,16:dim(MData)[2]]))
#MData <- scale(MData)
rownames(MData) <- model.data$Site.Code
MData <- as.data.frame(MData)

# Add categorical variables for random effects
Year <- c(2011, 2011, 2012, 2012, 2011, 2011, 2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012)
Continent <- c("Asia", "America", "Africa", "America", "America", "Africa", "America", "Africa", "Asia", "Africa", "Africa", "America", "America", "America")

Mdata <- cbind(MData, Year, Continent)

# Create output table of predictor and response variables for inclusion in paper
output.table <- cbind(model.data, Year, Continent)
output.table <- merge(output.table, plot.VGvar, by.x="Site.Code", by.y="Site.Code", all=FALSE)
#write.csv(output.table, file="Table_PredictorResponseVariables_Phase2_8May2014.csv", row.names=FALSE)

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

Examine <- as.data.frame(cbind(Mdata$CT.median, Mdata$CT.FDis, Mdata$CT.Shannon, Mdata$V.Cstorage, MData$V.TRich, Mdata$V.NStemsT, Mdata$V.NStemsL, Mdata$RainTotal, Mdata$Rain.CV, Mdata$Elev.CV, Mdata$ForestLossZOI, Mdata$Latitude, Mdata$SA_area))
names(Examine) <- c("CT.median", "CT.FDis", "CT.Shannon", "V.Cstorage", "V.TRich", "V.NStemsT", "V.NStemsL", "RainTotal", "Rain.CV", "Elev.CV", "ForestLossZOI", "Latitude", "SA_area")
pairs(Examine, lower.panel = panel.smooth, upper.panel = panel.cor)

# VISUALIZE species richness across sites
# SPECIES RICHNESS
library("fields")
set.panel(2,3)
par(mar=c(2,2,1,1))
hist(Mdata$CT.mean, main="Mean")
hist(Mdata$CT.median, main="Median")
hist(Mdata$CT.mode, main="Mode")
boxplot(Mdata$CT.mean~Mdata$Continent, ylim=c(-1,1))
boxplot(Mdata$CT.median~Mdata$Continent, ylim=c(-1,1))
boxplot(Mdata$CT.mode~Mdata$Continent, ylim=c(-1,1))
set.panel()

###### MODEL Terrestrial Vertebrate SPECIES RICHNESS

library(lme4)
library(MASS) 
library(MuMIn)



fitRich <- lm(CT.median ~  V.Cstorage + V.TShan + V.NStemsT + RainTotal + Rain.CV + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata)
stepRich <- stepAIC(fitRich, direction="both") 
bfRich <- lm(CT.median ~ V.NStemsT + V.NStemsL + RainTotal + Rain.CV + Elev.CV + 
               ForestLossZOI + Latitude + PA_area, data=Mdata)
summary(bfRich)
hist(resid(bfRich))
plot(resid(bfRich), Mdata$CT.median)

#bfRich.int <- lm(CT.median ~ , data=Mdata)
bfRich.ran <- lmer(CT.median ~ V.NStemsT + V.NStemsL + RainTotal + Rain.CV + Elev.CV + 
                     ForestLossZOI + Latitude + PA_area + (1|Continent), data=Mdata)
#bfRich.int.ran <- lmer(CT.median ~ , data=Mdata)
AIC(bfRich, bfRich.ran)
summary(bfRich.ran)

allRich.dredge <- dredge(fitRich, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
allRich <- model.avg(allRich.dredge, beta=TRUE, fit=TRUE)
summary(allRich)

# VISUALIZE  FUNCTIONAL DIVERSITY
set.panel(2,2)
par(mar=c(3,2,2,1))
hist(Mdata$CT.FDis, main="Functional Dispersion")
hist(Mdata$CT.RaoQ, main="Rao's Quadratic Entropy")
boxplot(Mdata$CT.FDis~Mdata$Continent, ylim=c(0.2,0.4))
boxplot(Mdata$CT.RaoQ~Mdata$Continent, ylim=c(0,0.2))
set.panel()

###### MODEL MAMMAL Vertebrate FUNCTIONAL DIVERSITY

fitFD <- lm(CT.FDis ~ V.Cstorage + V.TShan + V.NStemsT + RainTotal + Rain.CV + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata)
stepFD <- stepAIC(fitFD, direction="both")
bfFD <- lm(CT.FDis ~ V.Cstorage + V.TShan + V.NStemsT + V.NStemsL + RainTotal + 
             Elev.CV + ForestLossZOI, data=Mdata)
summary(bfFD)
#bfFD.int <- lm(CT.FDis ~ , data=Mdata)
bfFD.ran <- lmer(CT.FDis ~ RainTotal + Elev.CV + ForestLossZOI + (1|Continent), data=Mdata)
#bfFD.int.ran <- lmer(CT.FDis ~  (1|Continent), data=Mdata)
AIC(bfFD, bfFD.ran)
summary(bfFD.ran)

allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
allFD <- model.avg(allFD.dredge, beta=TRUE, fit=TRUE)
summary(allFD)

###### VISUALIZE Terrestrial Vertebrate TAXONOMIC DIVERSITY
set.panel(2,1)
par(mar=c(3,2,2,1))
hist(Mdata$CT.Shannon, main="Taxonomic Diversity")
boxplot(Mdata$CT.Shannon~Mdata$Continent, ylim=c(0,4))
set.panel()

###### MODEL Terrestrial Vertebrate TAXONOMIC DIVERSITY

fitShan <- lm(CT.Shannon ~ V.Cstorage + V.TShan + V.NStemsT + RainTotal + Rain.CV + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata)
stepShan <- stepAIC(fitShan, direction="both")
bfShan <- lm(CT.Shannon ~ V.TShan + V.NStemsT + RainTotal + Rain.CV + Elev.CV + 
               ForestLossZOI + Latitude, data=Mdata)
summary(bfShan)
bfShan.int <- lm(CT.Shannon ~ V.NStemsT + V.NStemsL + RainTotal + Elev.CV + V.NStemsT:Latitude, data=Mdata)
bfShan.ran <- lmer(CT.Shannon ~ V.NStemsT + V.NStemsL + RainTotal + Elev.CV + (1|Continent), data=Mdata)
bfShan.int.ran <- lmer(CT.Shannon ~ V.NStemsT + V.NStemsL + RainTotal + Elev.CV + V.NStemsT:Latitude + (1|Continent), data=Mdata)
AIC(bfShan, bfShan.int, bfShan.ran, bfShan.int.ran)
summary(bfShan)


allShan.dredge <- dredge(fitShan, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
allShan <- model.avg(allShan.dredge, beta=TRUE, fit=TRUE)
summary(allShan)


############ Create Plots #########
set.panel(1,3)
par(mar=c(5, 5, 1, 0))
plot(model.data$V.Cstorage/1000, Mdata$CT.median, las=1, ylab="Terrestrial Vertebrate Species Richness", xlab="", bty="n", xlim=c(100, 300), ylim=c(15,50), cex.lab=1.5, pch=19)
legend("bottom", expression(R^2* " = -0.07, df=12, p=0.69"), cex=1.2, bty="n")
plot(model.data$V.Cstorage/1000, Mdata$CT.FDisMedian, las=1, ylab="Functional Diversity (FDis)", xlab="", bty="n", xlim=c(100, 300), ylim=c(0.24,0.34), cex.lab=1.5, pch=19)
legend("bottom", expression(R^2* " = -0.04, df=12, p=0.51"), cex=1.2, bty="n")
plot(model.data$V.Cstorage/1000, Mdata$Shannon.Index, las=1, ylab="Species Diversity (Shannon Index)", xlab="", bty="n", xlim=c(100, 300), ylim=c(2.4, 3.4), cex.lab=1.5, pch=19)
legend("bottom", expression(R^2* " = -0.01, df=12, p=0.37"), cex=1.2, bty="n")
mtext("               Aboveground Carbon Storage (Mg C sq ha)", side=1, outer=TRUE, line=-2)

set.panel(1,3)
par(mar=c(3, 5, 2, 0))
boxplot(Mdata$CT.median~Mdata$Continent, ylab="Species Richness")
boxplot(Mdata$CT.FDisMedian~Mdata$Continent, ylab="Functional Diversity (FDis)")
boxplot(Mdata$Shannon.Index~Mdata$Continent, ylab="Species Diversity (Shannon Index)")

library(maps)
map('world', interior=FALSE, xlim=c(-90, 155), ylim=c(-60, 37), col="gray60")
LatLon <- read.csv(file="SiteLatitudeLongitude.csv")
points(LatLon$Longitude, LatLon$Latitude, col="green4", pch=19, cex=0.9)