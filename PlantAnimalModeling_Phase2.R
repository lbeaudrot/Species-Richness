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
#model.data$V.Cstorage <- log(model.data$V.Cstorage)
#model.data$V.TShan <- log(model.data$V.TShan)
#model.data$V.NStemsT <- log(model.data$V.NStemsT)
#model.data$RainTotal <- log(model.data$RainTotal)
#model.data$Elev.CV <- log(model.data$Elev.CV )
#model.data$Latitude <- log(model.data$Latitude)
model.data$ForestLossZOI <- log(model.data$ForestLossZOI)
model.data$PA_area <- log(model.data$PA_area)

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
MData <- cbind(MData[,1:17], scale(MData[,18:dim(MData)[2]]))
#MData <- scale(MData)
rownames(MData) <- model.data$Site.Code
MData <- as.data.frame(MData)

# Add categorical variables for random effects
Year <- c(2011, 2011, 2012, 2012, 2011, 2011, 2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012)
Continent <- c("Asia", "America", "Africa", "America", "America", "Africa", "America", "Africa", "Asia", "Africa", "Africa", "America", "America", "America")

Mdata <- cbind(MData, Year, Continent)

# Bring in sd values from CT_overall_model_summaries_5May2014 to use as weights in weighted least squares regression
CT.Rich.sd <- c(10.044, 6.280, 18.513, 5.167, 9.519, 10.394, 3.509, 11.639, 9.676, 3.044, 14.641, 9.584, 4.872, 6.392)
Mdata <- cbind(Mdata, CT.Rich.sd)

# Create output table of predictor and response variables for inclusion in paper
output.table <- cbind(model.data, Year, Continent, CT.Rich.sd)
output.table <- merge(output.table, plot.VGvar, by.x="Site.Code", by.y="Site.Code", all=FALSE)
#write.csv(output.table, file="Table_PredictorResponseVariables_Phase2_15May2014.csv", row.names=FALSE)

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

Examine <- as.data.frame(cbind(Mdata$CT.median, Mdata$CT.FDisMedian, Mdata$Shannon.Index, Mdata$V.Cstorage, Mdata$V.Cstorage2, MData$V.TShan, Mdata$V.NStemsT, Mdata$RainTotal, Mdata$Elev.CV, Mdata$ForestLossZOI, Mdata$Latitude, Mdata$PA_area))
names(Examine) <- c("CT.median", "FDis", "Shannon", "V.Cstorage", "V.Cstorage2", "V.TShan", "V.NStemsT", "RainTotal", "Elev.CV", "ForestLossZOI", "Latitude", "PA_area")
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

fitRich <- lm(CT.median ~  V.Cstorage2 + V.TShan + V.NStemsT + RainTotal + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata, weights=(1/(CT.Rich.sd^2)))
allRich.dredge <- dredge(fitRich, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
#allRich <- model.avg(allRich.dredge, beta=TRUE, fit=TRUE)
#allRich.sel <- model.sel(allRich.dredge)
#summary(allRich)

Richconfset.95p <- get.models(allRich.dredge, cumsum(weight) <= .95)
allRich <- model.avg(Richconfset.95p, beta=TRUE, fit=TRUE)
Rich95output <- model.sel(Richconfset.95p)
#write.csv(Rich95output, file="ModelAveraging_Rich95output.csv", row.names=FALSE)
summary(allRich)


plot(Mdata$CT.median, resid(fitRich), xlab="Global Model Residuals", ylab="Predicted Species Richness")
hist(resid(fitRich), main="", xlab="Global Model Residuals")

# VISUALIZE  FUNCTIONAL DIVERSITY
set.panel(2,2)
par(mar=c(3,2,2,1))
hist(Mdata$CT.FDis, main="Functional Dispersion")
hist(Mdata$CT.RaoQ, main="Rao's Quadratic Entropy")
boxplot(Mdata$CT.FDis~Mdata$Continent, ylim=c(0.2,0.4))
boxplot(Mdata$CT.RaoQ~Mdata$Continent, ylim=c(0,0.2))
set.panel()

###### MODEL MAMMAL Vertebrate FUNCTIONAL DIVERSITY

fitFD <- lm(CT.FDisMedian ~ V.Cstorage2 + V.TShan + V.NStemsT + RainTotal + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata)
allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
#allFD <- model.avg(allFD.dredge, beta=TRUE, fit=TRUE)
#summary(allFD)

FDconfset.95p <- get.models(allFD.dredge, cumsum(weight) <= .95)
allFD <- model.avg(FDconfset.95p, beta=TRUE, fit=TRUE)
FD95output <- model.sel(FDconfset.95p)
#write.csv(FD95output, file="ModelAveraging_FD95output.csv", row.names=FALSE)
summary(allFD)

plot(resid(fitFD), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Functional Diversity")
hist(resid(fitFD), main="", xlab="Global Model Residuals")


###### VISUALIZE Terrestrial Vertebrate TAXONOMIC DIVERSITY
set.panel(2,1)
par(mar=c(3,2,2,1))
hist(Mdata$CT.Shannon, main="Taxonomic Diversity")
boxplot(Mdata$CT.Shannon~Mdata$Continent, ylim=c(0,4))
set.panel()

###### MODEL Terrestrial Vertebrate TAXONOMIC DIVERSITY

fitShan <- lm(Shannon.Index ~ V.Cstorage2 + V.TShan + V.NStemsT + RainTotal + Elev.CV + ForestLossZOI + Latitude + PA_area, data=Mdata)
allShan.dredge <- dredge(fitShan, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE)
#model.sel(allShan.dredge)
#allShan <- model.avg(allShan.dredge, beta=TRUE, fit=TRUE)

#confset.1p <- get.models(allShan.dredge, weight >=.01)
Shanconfset.95p <- get.models(allShan.dredge, cumsum(weight) <= .95)
allShan <- model.avg(Shanconfset.95p, beta=TRUE, fit=TRUE)
Shan95output <- model.sel(Shanconfset.95p)
#write.csv(Shan95output, file="ModelAveraging_Shan95output.csv", row.names=FALSE)

summary(allShan)

#confint(allShan)


plot(resid(fitShan), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Taxonomic Diversity")
hist(resid(fitShan), main="", xlab="Global Model Residuals")



# All diagnostics together

set.panel(2,3)
par(mar=c(5,5,2,1))
plot(resid(fitRich), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Species Richness", main="Species Richness")
hist(resid(fitRich), main="", xlab="Global Model Residuals")

plot(resid(fitFD), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Functional Diversity", main="Functional Diversity")
hist(resid(fitFD), main="", xlab="Global Model Residuals")

plot(resid(fitShan), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Taxonomic Diversity", main="Taxonomic Diversity")
hist(resid(fitShan), main="", xlab="Global Model Residuals")



plot(Mdata$CT.median, resid(fitRich), ylab="Global Model Residuals", xlab="Predicted Species Richness", main="Species Richness")
plot(Mdata$CT.median, resid(fitFD), ylab="Global Model Residuals", xlab="Predicted Functional Diversity", main="Functional Diversity")
plot(Mdata$CT.median, resid(fitShan), ylab="Global Model Residuals", xlab="Predicted Taxonomic Diversity", main="Taxonomic Diversity")

hist(resid(fitRich), main="", xlab="Global Model Residuals")
hist(resid(fitFD), main="", xlab="Global Model Residuals")
hist(resid(fitShan), main="", xlab="Global Model Residuals")


############ Create Plots #########
pdf(file="AnimalDiversity_Carbon2_Plots.pdf", height=2.7)
set.panel(1,3)
par(mar=c(5, 5, 1, 1))
plot(model.data$V.Cstorage2/1000, Mdata$CT.median, las=1, ylab="Species Richness", xlab="", bty="n", xlim=c(100, 250), ylim=c(10,50), cex.lab=1.2, pch=19)
legend("bottomleft", expression(R^2* " = -0.08, df=12, p=0.77"), cex=1, bty="n")
plot(model.data$V.Cstorage2/1000, Mdata$Shannon.Index, las=1, ylab="Taxonomic Diversity", xlab="", bty="n", xlim=c(100, 250), ylim=c(2.2, 3.4), cex.lab=1.2, pch=19)
legend("bottomleft", expression(R^2* " = -0.05, df=12, p=0.56"), cex=1, bty="n")
plot(model.data$V.Cstorage2/1000, Mdata$CT.FDisMedian, las=1, ylab="Functional Diversity", xlab="", bty="n", xlim=c(100, 250), ylim=c(0.24,0.32), cex.lab=1.2, pch=19)
legend("bottomleft", expression(R^2* " = 0.05, df=12, p=0.22"), cex=1, bty="n")
mtext("               Aboveground Carbon Storage (Mg C sq ha)", side=1, outer=TRUE, line=-2)
dev.off()


set.panel(1,3)
par(mar=c(3, 5, 2, 0))
boxplot(Mdata$CT.median~Mdata$Continent, ylab="Species Richness")
boxplot(Mdata$CT.FDisMedian~Mdata$Continent, ylab="Functional Diversity (FDis)")
boxplot(Mdata$Shannon.Index~Mdata$Continent, ylab="Species Diversity (Shannon Index)")

library(maps)

LatLon <- read.csv(file="SiteLatitudeLongitude.csv")
LatLon <- LatLon[-9,]
LatLon <- LatLon[-6,]
set.panel()
pdf(file="TEAM_Map_14Sites.pdf")
par(mar=c(0,0,0,0))
map('world', interior=FALSE, xlim=c(-132, 155), ylim=c(-60, 37), col="gray60")
points(LatLon$Longitude, LatLon$Latitude, col="green4", pch=c(1:14), cex=1)
legend(x=-132, y=37, legend=LatLon$Site.Code, pch=c(1:14), border="transparent", col="green4", bg="white", box.col="transparent", title="TEAM Sites", title.adj=0.12, cex=0.66)
dev.off()

RelVar <- read.csv("RelativeVariableImportance.csv")
barplotdata <- t(RelVar[,2:4])
#colnames(barplotdata) <- c("Stem Density", "Elevation", "Area", "Latitude", "Rainfall", "Carbon", "Forest Loss", "Tree Diversity")
colnames(barplotdata) <- c("Stem Density", "Elevation CV", "Tree Diversity", "Annual Rain", "Forest Loss", "Protected Area", "Latitude", "Carbon")
pdf(file="RelativeVariableImportance_BarPlot.pdf", height=5)
par(mar=c(8,5,2,2))
barplot(barplotdata, beside=TRUE, horiz=FALSE, las=2, ylab="Relative Variable Importance", cex.lab=1, cex.axis=1)
legend("right", pch=22, pt.cex=1.5, legend=c("Species Richness", "Taxonomic Diversity", "Functional Diversity"), col=c("black"), pt.bg=c("black", "gray", "gray92"), bty="n")
dev.off()


## Coefficient Plot
# Code modified from https://gist.github.com/dsparks/818976
library(ggplot2)

Rich.coef <- summary(allRich)[[3]]
rownames(Rich.coef) <- c("(Intercept)", "Elevation CV", "Stem Density", "Rainfall", "PA Size", "Latitude", "Carbon", "Tree Diversity", "Forest Loss")

Shan.coef <- summary(allShan)[[3]]
rownames(Shan.coef) <- c("(Intercept)", "Elevation CV", "Rainfall", "Stem Density", "Latitude", "Tree Diversity", "Forest Loss", "Carbon", "PA Size")

FD.coef <- summary(allFD)[[3]]
rownames(FD.coef) <- c("(Intercept)", "Forest Loss", "Tree Diversity", "Stem Density", "PA Size", "Latitude", "Rainfall", "Carbon", "Elevation CV")

#graphmodels <- list(summary(allRich)[[3]], summary(allShan)[[3]], summary(allFD)[[3]])
graphmodels <- list(Rich.coef, Shan.coef, FD.coef)
names(graphmodels) <- c("Species Richness", "Taxonomic Diversity", "Functional Diversity")

OutputPlot <- qplot(rownames(test2), test2[,1], ymin = test2[,4],
                    ymax = test2[,5], data = test2, geom = "pointrange",
                    ylab = NULL, xlab = NULL)
OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()


CoefficientPlot <- function(models, modelnames = ""){
  # models must be a list()
  
  CoefficientTables <- graphmodels
  TableRows <- unlist(lapply(CoefficientTables, nrow))
  
  if(modelnames[1] == ""){
    ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
  } else {
    ModelNameLabels <- rep(modelnames, TableRows)
  }
  
  MatrixofModels <- cbind(do.call(rbind, CoefficientTables), ModelNameLabels)
  MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))
  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "AdjSE", "LowerCI", "UpperCI", "ModelName")
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
  MatrixofModels[, -c(1, 7)] <- apply(MatrixofModels[, -c(1, 7)], 2, function(x){as.numeric(as.character(x))})
  
  OutputPlot <- qplot(IV, Estimate, ymin = LowerCI,
                      ymax = UpperCI, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL)
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
  return(OutputPlot)
}

pdf(file="CoefficientPlot.pdf")
CoefficientPlot(graphmodels, modelnames=c("Species Richness", "Taxonomic Diversity", "Functional Diversity"))
dev.off()