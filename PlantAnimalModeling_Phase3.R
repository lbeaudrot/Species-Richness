# Modeling terrestrial vertebrate species richness and functional diversity as a function of site level vegetation properties and carbon storage
# For phase two, keep analysis at the site level for all variables 
# In phase three, update analysis with suggested revisions from collaborators following circulation of first draft of REDD+ manuscript 

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
AnimalFD <- read.csv("FunctionalDiversity_Overall_20May2014.csv")
model.data <- merge(CTaverages, AnimalFD, by.x="Site.Code", by.y="Site.Code", all=TRUE)

ShannonDist <- read.csv("ShannonIndex_Distribution.csv")
model.data <- merge(model.data, ShannonDist, by.x="Site.Code", by.y="Site.Code", all=FALSE)

CT.FDisDist <- read.csv("FunctionalDiversity_Overall_Distribution_19May2014.csv")
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


# Add annual rainfall from Worldclim extracted at 2.5 arc-minutes resolution
rainWC <- read.csv("SiteLatitudeLongitude.csv")
AnnualRainWC <- rainWC[,-3]
AnnualRainWC <- AnnualRainWC[,-2]
model.data <- merge(model.data, AnnualRainWC, by.x="Site.Code", by.y="Site.Code", all=FALSE)

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
Latitude_orig <- read.csv("Latitude_MeanSiteCT.csv")
Latitude_match <- Latitude_orig
Latitude_match$Latitude <- abs(Latitude_match$Latitude)
model.data <- merge(model.data, Latitude_match, by.x="Site.Code", by.y="Site.Code", all=FALSE)


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
Continent <- c("Asia", "America", "Africa", "America", "America", "Africa", "America", "Africa", "Asia", "Madagascar", "Africa", "America", "America", "America")

Mdata <- cbind(MData, Year, Continent)

# Bring in sd values from CT_overall_model_summaries_5May2014 to use as weights in weighted least squares regression
CT.Rich.sd <- c(10.044, 6.280, 18.513, 5.167, 9.519, 10.394, 3.509, 11.639, 9.676, 3.044, 14.641, 9.584, 4.872, 6.392)
Mdata <- cbind(Mdata, CT.Rich.sd)

Asia <- c(1,0,0,0,0,0,0,0,1,0,0,0,0,0)
Africa <- c(0,0,1,0,0,1,0,1,0,0,1,0,0,0)
Madagascar <- c(0,0,0,0,0,0,0,0,0,1,0,0,0,0)

Mdata <- cbind(Mdata, Asia, Africa, Madagascar)

extras <- as.data.frame(cbind(ForestLoss$ForestLossZOI, ForestLoss$PA_area, Latitude_orig$Latitude))
extras <- cbind(Latitude_orig$Site.Code, extras)
extras <- extras[-9,]
extras <- extras[-6,]
colnames(extras) <- c("Site.Code2", "ForestLossZOI2", "PA_area2", "Latitude2")
# Create output table of predictor and response variables for inclusion in paper
output.table <- cbind(model.data, Year, Continent, CT.Rich.sd, extras, Asia, Africa)
output.table <- merge(output.table, plot.VGvar, by.x="Site.Code", by.y="Site.Code", all=FALSE)
#write.csv(output.table, file="Table_PredictorResponseVariables_Phase3_21June2014.csv", row.names=FALSE)

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

Examine <- as.data.frame(cbind(Mdata$CT.median, Mdata$CT.FDisMedian, Mdata$Shannon.Index,  Mdata$V.Cstorage2, MData$V.TShan, Mdata$V.NStemsT,  Mdata$Elev.Mean, Mdata$Elev.CV, Mdata$ForestLossZOI, Mdata$Latitude, Mdata$PA_area, Mdata$WC_Bio12))
names(Examine) <- c("SpRichness", "FuncDiv", "TaxonDiv", "Carbon", "Tree Diversity", "Stem Density",  "Elev.Mean", "Elev.CV", "ForestLoss", "Latitude", "PA Size", "Rainfall")
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
boxplot(Mdata$CT.median~Mdata$Continent, ylim=c(10,50))
boxplot(Mdata$CT.mode~Mdata$Continent, ylim=c(-1,1))
set.panel()

###### MODEL Terrestrial Vertebrate SPECIES RICHNESS

library(lme4)
library(MASS) 
library(MuMIn)

fitRich <- lm(CT.median ~  V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, data=Mdata)
#fitRich2 <- lm(CT.median ~  V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, data=Mdata, weights=(1/(CT.Rich.sd^2)))
allRich.dredge <- dredge(fitRich, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allRich.dredge <- dredge(fitRich, fixed=c("Africa", "Asia", "Madagascar"), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allRich.dredge <- dredge(fitRich, subset=dc(Africa, Asia, Madagascar), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))

# try also (instead of fixed) subset=dc("Africa", "Asia", "Madagascar")
#allRich <- model.avg(allRich.dredge, beta=TRUE, fit=TRUE)
#allRich.sel <- model.sel(allRich.dredge)
#summary(allRich)

Richconfset.95p <- get.models(allRich.dredge, cumsum(weight) <= .95)
allRich <- model.avg(Richconfset.95p, beta=TRUE, fit=TRUE)
Rich95output <- model.sel(Richconfset.95p)
RichAlloutput <- model.sel(allRich.dredge)
#write.csv(RichAlloutput, file="ModelAveraging_RichAlloutput_WLS_WCBio12_DummyVariables_21June2014.csv", row.names=FALSE)
#write.csv(Rich95output, file="ModelAveraging_Rich95output_WLS_WCBio12_DummyVariables_4August2014.csv", row.names=FALSE)
summary(allRich)

fitRichbest <- lm(CT.median ~  Elev.CV  + V.NStemsT + WC_Bio12, data=Mdata, weights=(1/(CT.Rich.sd^2)))
fitRichbest2 <- lm(CT.median ~  Elev.CV + Madagascar, data=Mdata)
summary(fitRichbest2)


plot(Mdata$CT.median, resid(fitRich), xlab="Global Model Residuals", ylab="Predicted Species Richness")
hist(resid(fitRich), main="", xlab="Global Model Residuals")

# VISUALIZE  FUNCTIONAL DIVERSITY
set.panel(2,2)
par(mar=c(3,2,2,1))
hist(Mdata$CT.FDis, main="Functional Dispersion")
hist(Mdata$CT.RaoQ, main="Rao's Quadratic Entropy")
boxplot(Mdata$CT.FDisMedian~Mdata$Continent, ylim=c(0.25,0.35))
boxplot(Mdata$CT.RaoQ~Mdata$Continent, ylim=c(0,0.2))
set.panel()

###### MODEL MAMMAL Vertebrate FUNCTIONAL DIVERSITY

fitFD <- lm(CT.FDisMedian ~ V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, data=Mdata)
#fitFD2 <- lm(CT.FDisMedian ~ V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, weights=(1/(CT.FDis.sd^2)), data=Mdata)
allFD.dredge <- dredge(fitFD, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allFD.dredge <- dredge(fitFD, fixed=c("Africa", "Asia", "Madagascar"), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allFD.dredge <- dredge(fitFD, subset=dc(Africa, Asia, Madagascar), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))

#allFD <- model.avg(allFD.dredge, beta=TRUE, fit=TRUE)
#summary(allFD)

FDconfset.95p <- get.models(allFD.dredge, cumsum(weight) <= .95)
allFD <- model.avg(FDconfset.95p, beta=TRUE, fit=TRUE)
FD95output <- model.sel(FDconfset.95p)
FDAlloutput <- model.sel(allFD.dredge)
#write.csv(FD95output, file="ModelAveraging_FD95output_WCBio12_DummyVariables_4August2014.csv", row.names=FALSE)
summary(allFD)

plot(resid(fitFD), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Functional Diversity")
hist(resid(fitFD), main="", xlab="Global Model Residuals")

fitFDbest <- lm(CT.FDisMedian ~ Africa + Elev.CV, weights=(1/(CT.FDis.sd^2)), data=Mdata)
fitFDbest2 <- lm(CT.FDisMedian ~ Africa, data=Mdata)
summary(fitFDbest)

###### VISUALIZE Terrestrial Vertebrate TAXONOMIC DIVERSITY
set.panel(2,1)
par(mar=c(3,2,2,1))
hist(Mdata$CT.Shannon, main="Taxonomic Diversity")
boxplot(Mdata$CT.Shannon~Mdata$Continent, ylim=c(2,4))
set.panel()

###### MODEL Terrestrial Vertebrate TAXONOMIC DIVERSITY

fitShan <- lm(Shannon.Index ~  V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, data=Mdata)
#fitShan2 <- lm(Shannon.Index ~ V.Cstorage2 + V.TShan + V.NStemsT + WC_Bio12 + Elev.CV + ForestLossZOI + Latitude + PA_area + Africa + Asia + Madagascar, weights=(1/(Shannon.Index.sd^2)), data=Mdata)
allShan.dredge <- dredge(fitShan, beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allShan.dredge <- dredge(fitShan, fixed=c("Africa", "Asia", "Madagascar"), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#allShan.dredge <- dredge(fitShan, subset=dc(Africa, Asia), beta=TRUE, evaluate=TRUE, rank="AICc", trace=TRUE, extra=list("confint", "adjR^2"))
#model.sel(allShan.dredge)
#allShan <- model.avg(allShan.dredge, beta=TRUE, fit=TRUE)

#confset.1p <- get.models(allShan.dredge, weight >=.01)
Shanconfset.95p <- get.models(allShan.dredge, cumsum(weight) <= .95)
allShan <- model.avg(Shanconfset.95p, beta=TRUE, fit=TRUE)
Shan95output <- model.sel(Shanconfset.95p)
ShanAlloutput <- model.sel(allShan.dredge)
#write.csv(Shan95output, file="ModelAveraging_ShanAlloutput_WCBio12_DummyVariables_4August2014.csv", row.names=FALSE)

summary(allShan)

#confint(allShan)

fitShanbest <- lm(Shannon.Index ~ Elev.CV, weights=(1/(Shannon.Index.sd^2)), data=Mdata)
fitShanbest2 <- lm(Shannon.Index ~ Elev.CV + V.NStemsT + V.TShan + Asia, data=Mdata)
summary(fitShanbest2)


plot(resid(fitShan), Mdata$CT.median, xlab="Global Model Residuals", ylab="Predicted Taxonomic Diversity")
hist(resid(fitShan), main="", xlab="Global Model Residuals")


# Examine model outputs using Robust SE. Code from http://drewdimmery.com/robust-ses-in-r/ 
library(sandwich)
library(lmtest)


robust.se <- function(model, cluster){
  require(sandwich)
  require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}

cluster.var <- Continent
Model <- fitShanbest2
robust.se(Model, cluster.var)

# Modify robust SE function as a first step to incorporate robust SEs in model selection; modified function incorporates Continent variable
robust.se2 <- function(model){
  require(sandwich)
  require(lmtest)
  M <- length(unique(Continent))
  N <- length(Continent)
  K <- model$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(model), 2, function(x) tapply(x, Continent, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  rcse.se <- coeftest(model, rcse.cov)
  return(list(rcse.cov, rcse.se))
}


#To save only the variance-covariance matrix
vcov<-robust.se(model,clustervar)[[1]]
#To save the coeftest object,
# which is a table containing the results
# of a test of the coefficients for significance
# using the corrected SEs
coefs<-robust.se(model,clustervar)[[2]]




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


summary(lm(Mdata$CT.median ~ Mdata$V.Cstorage2))

summary(lm(Mdata$CT.FDisMedian ~ Mdata$V.Cstorage2))

summary(lm(Mdata$Shannon.Index ~ Mdata$V.Cstorage2))


############ Create Plots #########
pdf(file="AnimalDiversity_Carbon2_Plots_ByRegionColor.pdf", height=2.7)
set.panel(1,3)
par(mar=c(4, 5, 0.5, 0.5))
plot(model.data$V.Cstorage2/1000, Mdata$CT.median, las=1, ylab="Species Richness", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(10,50), cex.lab=1.4, cex.axis=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.5)
legend("bottomleft", expression(R^2*"=-0.03, df=12, p=0.47"), cex=1.15, bty="n")
plot(model.data$V.Cstorage2/1000, Mdata$Shannon.Index, las=1, ylab="Taxonomic Diversity", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(2.2, 3.4), cex.lab=1.4, cex.axis=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.5)
legend("bottomleft", expression(R^2*"=-0.02, df=12, p=0.40"), cex=1.15, bty="n")
plot(model.data$V.Cstorage2/1000, Mdata$CT.FDisMedian, las=1, ylab="", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(0.24,0.32), cex.lab=1.4, cex.axis=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.5)
mtext("Trait Diversity", side=2, line=4)
legend("bottomleft", expression(R^2*"=0.05, df=12, p=0.22"), cex=1.15, bty="n")
legend("topright", legend=c("Asia", "Americas", "Africa", "Madagascar"), col=c("red", "green3", "blue","black"), 
       pch=c(17,16,15,18),  box.col="transparent", cex=1.15)
mtext("               Aboveground Carbon Storage (Mg C per ha)", side=1, outer=TRUE, line=-1.5)
dev.off()

# Change to vertical orientation 
pdf(file="AnimalDiversity_Carbon2_Plots_ByRegionColor_Vertical.pdf", width=2.7)
set.panel(3, 1)
par(oma=c(2, 0, 0, 0), mar=c(2, 5, 0, 1))
plot(model.data$V.Cstorage2/1000, Mdata$CT.median, las=1, ylab="Species Richness", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(10,50), cex.lab=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.3)
legend("bottomleft", expression(R^2* " = -0.03, df=12, p=0.47"), cex=1, bty="n")
legend(x=185, y=27, legend=c("Asia", "Americas", "Africa", "Madagascar"), col=c("red", "green3", "blue","black"), 
       pch=c(17,16,15,18),  box.col="transparent", cex=1)
plot(model.data$V.Cstorage2/1000, Mdata$Shannon.Index, las=1, ylab="Taxonomic Diversity", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(2.2, 3.4), cex.lab=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.3)
legend("bottomleft", expression(R^2* " = -0.02, df=12, p=0.40"), cex=1, bty="n")
plot(model.data$V.Cstorage2/1000, Mdata$CT.FDisMedian, las=1, ylab="Trait Diversity", xlab="", bty="n", xlim=c(100, 250), 
     ylim=c(0.24,0.32), cex.lab=1.2, pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], 
     col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], cex=1.3)
legend("bottomleft", expression(R^2* " = 0.05, df=12, p=0.22"), cex=1, bty="n")
#legend("topright", legend=c("Asia", "Americas", "Africa", "Madagascar"), col=c("red", "green3", "blue","black"), 
       pch=c(17,16,15,18),  box.col="transparent", cex=1)
mtext("       Aboveground Carbon Storage (Mg C per ha)", side=1, outer=TRUE, line=0.5, cex=0.7)
dev.off()


# Examine relationship between functional diversity and species richness & taxonomic diversity within continents

summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="Africa"] ~ Mdata$Shannon.Index[Mdata$Continent=="Africa"]))
summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="America"] ~ Mdata$Shannon.Index[Mdata$Continent=="America"]))
AB <- coef(summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="America"] ~ Mdata$Shannon.Index[Mdata$Continent=="America"])))[1:2]

summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="Africa"] ~ Mdata$CT.median[Mdata$Continent=="Africa"]))
summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="America"] ~ Mdata$CT.median[Mdata$Continent=="America"]))
CD <- coef(summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="America"] ~ Mdata$CT.median[Mdata$Continent=="America"])))[1:2]


pdf(file="FunctionalDiversity_OtherDiversity_Plots_ByRegionColor.pdf", height=4)
set.panel(1,2)
plot(Mdata$CT.median, Mdata$CT.FDisMedian, col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], 
     pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], xlab="Species Richness", ylab="Functional Diversity")
abline(coef=CD, lty=1, lwd=2, col="green3")

legend("topleft", legend=c("Asia", "Americas", "Africa", "Madagascar"), col=c("red", "green3", "blue","black"), 
       pch=c(17,16,15,18),  box.col="transparent", cex=0.8)
plot(Mdata$Shannon.Index, Mdata$CT.FDisMedian, col=c("blue","green3","red","black")[unclass(as.factor(Mdata$Continent))], 
     pch=c(15,16,17,18)[unclass(as.factor(Mdata$Continent))], xlab="Taxonomic Diversity", ylab="Functional Diversity")
abline(coef=AB, lty=2, lwd=2, col="green3")
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

pdf(file="TEAM_Map_14Sites_Labeled.pdf", height=3)
par(mar=c(0,0,0,0))
map('world', interior=FALSE, xlim=c(-132, 155), ylim=c(-60, 37), col="gray60")
#points(LatLon$Longitude, LatLon$Latitude, col="black", pch=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), cex=0.01)
#text(LatLon$Longitude, LatLon$Latitude, col="black", labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), cex=1)
#legend(x=-132, y=37, legend=LatLon$Site.Code, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), border="transparent", col="black", bg="white", box.col="transparent", title="TEAM Sites", title.adj=0.12, cex=0.66)
points(LatLon$Longitude, LatLon$Latitude, col="black", pch=20, cex=1.6)
text(22, -50, "TEAM Network Sites")
text(LatLon$Longitude[1]-8, LatLon$Latitude[1], col="black", labels=LatLon$Site.Code[1], cex=0.7)
text(LatLon$Longitude[2]-5, LatLon$Latitude[2]-4, col="black", labels=LatLon$Site.Code[2], cex=0.7)
text(LatLon$Longitude[3]-6, LatLon$Latitude[3], col="black", labels=LatLon$Site.Code[3], cex=0.7)
text(LatLon$Longitude[4]-5, LatLon$Latitude[4]+4, col="black", labels=LatLon$Site.Code[4], cex=0.7)
text(LatLon$Longitude[5]+5, LatLon$Latitude[5]-4, col="black", labels=LatLon$Site.Code[5], cex=0.7)
text(LatLon$Longitude[6]-2, LatLon$Latitude[6]+4, col="black", labels=LatLon$Site.Code[6], cex=0.7)
text(LatLon$Longitude[7], LatLon$Latitude[7]-4, col="black", labels=LatLon$Site.Code[7], cex=0.7)
text(LatLon$Longitude[8], LatLon$Latitude[8]+4, col="black", labels=LatLon$Site.Code[8], cex=0.7)
text(LatLon$Longitude[9]+6, LatLon$Latitude[9]+3, col="black", labels=LatLon$Site.Code[9], cex=0.7)
text(LatLon$Longitude[10]+8, LatLon$Latitude[10], col="black", labels=LatLon$Site.Code[10], cex=0.7)
text(LatLon$Longitude[11]-8, LatLon$Latitude[11], col="black", labels=LatLon$Site.Code[11], cex=0.7)
text(LatLon$Longitude[12]-7, LatLon$Latitude[12], col="black", labels="VB", cex=0.7)
text(LatLon$Longitude[13]-8, LatLon$Latitude[13], col="black", labels=LatLon$Site.Code[13], cex=0.7)
text(LatLon$Longitude[14]-7, LatLon$Latitude[14], col="black", labels=LatLon$Site.Code[14], cex=0.7)
dev.off()

pdf(file="TEAM_Map_14Sites_Numbered.pdf", height=3)
par(mar=c(0,0,0,0))
map('world', interior=FALSE, xlim=c(-132, 155), ylim=c(-60, 37), col="gray60")
#points(LatLon$Longitude, LatLon$Latitude, col="black", pch=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), cex=0.01)
#text(LatLon$Longitude, LatLon$Latitude, col="black", labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), cex=1)
#legend(x=-132, y=37, legend=LatLon$Site.Code, labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), border="transparent", col="black", bg="white", box.col="transparent", title="TEAM Sites", title.adj=0.12, cex=0.66)
points(LatLon$Longitude, LatLon$Latitude, col="black", pch=20, cex=1.6)
text(22, -50, "TEAM Network Sites")
text(LatLon$Longitude[1]-8, LatLon$Latitude[1], col="black", labels="1", cex=0.7)
text(LatLon$Longitude[2]-4, LatLon$Latitude[2]-4, col="black", labels="2", cex=0.7)
text(LatLon$Longitude[3]-6, LatLon$Latitude[3], col="black", labels="3", cex=0.7)
text(LatLon$Longitude[4]-5, LatLon$Latitude[4]+4, col="black", labels="4", cex=0.7)
text(LatLon$Longitude[5]+5, LatLon$Latitude[5]-4, col="black", labels="5", cex=0.7)
text(LatLon$Longitude[6]-2, LatLon$Latitude[6]+4, col="black", labels="6", cex=0.7)
text(LatLon$Longitude[7], LatLon$Latitude[7]-5, col="black", labels="7", cex=0.7)
text(LatLon$Longitude[8], LatLon$Latitude[8]+4, col="black", labels="8", cex=0.7)
text(LatLon$Longitude[9]+6, LatLon$Latitude[9]+3, col="black", labels="9", cex=0.7)
text(LatLon$Longitude[10]+8, LatLon$Latitude[10], col="black", labels="10", cex=0.7)
text(LatLon$Longitude[11]-8, LatLon$Latitude[11], col="black", labels="11", cex=0.7)
text(LatLon$Longitude[12]-7, LatLon$Latitude[12], col="black", labels="12", cex=0.7)
text(LatLon$Longitude[13]-7, LatLon$Latitude[13], col="black", labels="13", cex=0.7)
text(LatLon$Longitude[14]-7, LatLon$Latitude[14], col="black", labels="14", cex=0.7)
dev.off()

#RelVar <- read.csv("RelativeVariableImportance_WCBio12_DummyVariables.csv")

Variable <- c("Elevation CV", "Madagascar", "Africa", "Tree Diversity", "Stem Density", "Forest Loss", "PA Size", "Asia", "Latitude", "Rainfall", 
              "Carbon")
RelVar.Rich <- c(0.88, 0.71, 0.07, 0.06, 0.35, 0.05, 0.12, 0.06, 0.08, 0.08, 0.06)
RelVar.Shan <- c(0.6, 0.26, 0.15, 0.58, 0.4, 0.23, 0.17, 0.15, 0.14, 0.07, 0.07)
RelVar.FD <- c(0.1, 0.11, 0.65, 0.36, 0.27, 0.29, 0.08, 0.15, 0.12, 0.1, 0.07)
RelVar <- cbind(RelVar.Rich, RelVar.Shan, RelVar.FD)
rownames(RelVar) <- Variable

barplotdata <- t(RelVar)
colnames(barplotdata) <- Variable
pdf(file="RelativeVariableImportance_BarPlot_WCBio12_DummyVariables_4August2014.pdf", height=5)
par(mar=c(8,5,2,2))
barplot(barplotdata, beside=TRUE, horiz=FALSE, las=2, ylab="Relative Variable Importance", cex.lab=1, cex.axis=1, ylim=c(0,1))
legend("topright", pch=22, pt.cex=1.5, legend=c("Species Richness", "Taxonomic Diversity", "Trait Diversity"), col=c("black"), pt.bg=c("black", "gray", "gray92"), bty="n")
dev.off()

RelRich <- summary(allRich)[[6]]
RelShan <- summary(allShan)[[6]]
RelFD <- summary(allFD)[[6]]

RelRich[charmatch(names(RelRich), names(RelShan))]
RelShan[charmatch(names(RelFD), names(RelShan))]

## Coefficient Plot
# Code modified from https://gist.github.com/dsparks/818976
library(ggplot2)

Rich.coef <- summary(allRich)[[3]]
rownames(summary(allRich)[[3]])[1:12]
rownames(Rich.coef) <- c("(Intercept)", "Elevation CV", "Madagascar", "Stem Density",  "Africa", "PA Size", "Asia", "Tree Diversity", "Carbon", "Forest Loss", "Rainfall", "Latitude")
  
Shan.coef <- summary(allShan)[[3]]
rownames(summary(allShan)[[3]])[1:12]
rownames(Shan.coef) <- c("(Intercept)",  "Elevation CV", "Stem Density", "Tree Diversity", "Asia", "Madagascar", "Africa", "Forest Loss", 
                         "PA Size", "Latitude", "Rainfall", "Carbon")
  
FD.coef <- summary(allFD)[[3]]
rownames(summary(allFD)[[3]])[1:12]
rownames(FD.coef) <- c("(Intercept)", "Africa",  "Stem Density", "Forest Loss", "Tree Diversity", "Latitude",
                       "Asia", "Madagascar", "Elevation CV", "Carbon", "PA Size", "Rainfall")
  
#graphmodels <- list(summary(allRich)[[3]], summary(allShan)[[3]], summary(allFD)[[3]])
graphmodels <- list(Rich.coef[-1,], Shan.coef[-1,], FD.coef[-1,])#remove intercept from figure by removing its row here
names(graphmodels) <- c("Species Richness", "Taxonomic Diversity", "Trait Diversity")


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
  #MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = c("(Intercept)", "Madagascar", "Africa", "Asia", "Carbon", "Rainfall", "Latitude", "PA Size", "Forest Loss", "Tree Diversity", "Stem Density", "Elevation CV"))
  
  MatrixofModels[, -c(1, 7)] <- apply(MatrixofModels[, -c(1, 7)], 2, function(x){as.numeric(as.character(x))})
  
  MatrixofModels$ModelName <- factor(MatrixofModels$ModelName, levels=c("Species Richness", "Taxonomic Diversity", "Trait Diversity"))
  
  OutputPlot <- qplot(IV, Estimate, ymin = LowerCI,
                      ymax = UpperCI, data = MatrixofModels, geom = "pointrange",
                      ylab = NULL, xlab = NULL)
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + theme_bw()
  return(OutputPlot)
}

pdf(file="CoefficientPlot_4August2014.pdf")
CoefficientPlot(graphmodels, modelnames=c("Species Richness", "Taxonomic Diversity", "Trait Diversity"))
dev.off()


# Compare observed and predicted species richness estimates with a paired t-test
#observed <- c(20, 20, 21, 26, 32, 25, 28, 25, 23, 15, 25, 18, 19, 32)
observed <- c(24, 28, 22, 29, 36, 26, 29, 32, 31, 16, 27, 20, 23, 38)
expected <- Mdata$CT.median
t.test(expected, observed, paired=TRUE)

# Examine relationship between carbon storage and diversity within continents
summary(lm(Mdata$CT.median[Mdata$Continent=="Africa"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Africa"]))
summary(lm(Mdata$CT.median[Mdata$Continent=="America"] ~ Mdata$V.Cstorage2[Mdata$Continent=="America"]))
summary(lm(Mdata$CT.median[Mdata$Continent=="Asia"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Asia"]))

summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="Africa"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Africa"]))
summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="America"] ~ Mdata$V.Cstorage2[Mdata$Continent=="America"]))
summary(lm(Mdata$CT.FDisMedian[Mdata$Continent=="Asia"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Asia"]))

summary(lm(Mdata$Shannon.Index[Mdata$Continent=="Africa"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Africa"]))
summary(lm(Mdata$Shannon.Index[Mdata$Continent=="America"] ~ Mdata$V.Cstorage2[Mdata$Continent=="America"]))
summary(lm(Mdata$Shannon.Index[Mdata$Continent=="Asia"] ~ Mdata$V.Cstorage2[Mdata$Continent=="Asia"]))


# Examine relationship between IUCN status, elevation variability and stem density

