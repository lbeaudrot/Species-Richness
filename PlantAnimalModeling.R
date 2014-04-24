# Modeling terrestrial vertebrate species richness and functional diversity as a function of site level vegetation properties and carbon storage


#Use EstimateSpeciesRichness.R to obtain TEAM site level mean, median and mode of terrestrial vertebrate species richness estimates
#See object CTaverages for species richness estimates

#Use PlantDiversity.R to obtain TEAM site level plant covariates. 
#See objects plot.VGmean and plot.VGvar for mean and variance of plant covariates

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
pairs(plot.VGmean, lower.panel = panel.smooth, upper.panel = panel.cor)
pairs(plot.VGvar, lower.panel = panel.smooth, upper.panel = panel.cor)

# Merge CT and Veg data into a single table
CTaverages <- cbind(rownames(CTaverages), CTaverages)
colnames(CTaverages)[1] <- "Site.Code"
plot.VGmean <- cbind(rownames(plot.VGmean), plot.VGmean)
colnames(plot.VGmean)[1] <- "Site.Code"
model.data <- merge(CTaverages, plot.VGmean, by.x="Site.Code", by.y="Site.Code", all=FALSE)

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

# Calculate elevation CV and add elevation to model.data
elevation.data <- read.csv("CT_edgedist_elevation_final.csv") 
elevation.mean <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=mean)
elevation.CV <- aggregate(elevation.data$Elevation ~ elevation.data$Site.Code, FUN=CV)[2]
elevation <- cbind(elevation.mean, elevation.CV)
colnames(elevation) <- c("Site.Code", "Elev.Mean", "Elev.CV")
model.data <- merge(model.data, elevation, by.x="Site.Code", by.y="Site.Code", all=FALSE)

# Add latitude for each TEAM site as a covariate - see EstimateSpeciesRichness.R line 203
# Extract mean CT latitude for each TEAM site to include as a covariate 
traps <- eventsdata[!duplicated(eventsdata$Sampling.Unit.Name),]
Latitude <- aggregate(traps$Latitude ~ traps$Site.Code, FUN=mean)
colnames(Latitude) <- c("Site.Code", "Latitude")
model.data <- merge(model.data, Latitude, by.x="Site.Code", by.y="Site.Code", all=FALSE)


head(model.data)
str(model.data)
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
MData <- cbind(MData[,1:3], scale(MData[,4:dim(MData)[2]]))
rownames(MData) <- model.data$Site.Code
pairs(MData, lower.panel = panel.smooth, upper.panel = panel.cor)

library(lme4)
library(MASS)

Year <- c(2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012, 2011, 2011, 2011, 2011, 2012)
Continent <- c("Asia", "America", "America", "America", "Africa", "America", "Africa", "Asia", "Africa", "Africa", "America", "America", "America")
Mdata <- cbind(MData, Year, Continent)


###### Model Terrestrial Vertebrate Species Richness
fit1 <- lm(CT.mode ~ 
             Cstorage + FDis + TRich + NStemsT + 
             NStemsL + Rain.CV + Elev.CV + abs(Latitude) + 
             abs(Latitude)*NStemsT + abs(Latitude)*Elev.CV + TRich*Rain.CV, 
                data=Mdata)

step1 <- stepAIC(fit1, direction="both")

lmer(CT.mode ~ (1|Year) + (1|Continent) + Cstorage + FDis + TRich + TShan + NStemsT + NStemsL + RainTotal + Rain.CV + Elev.Mean + Elev.CV + abs(Latitude), data=Mdata)

m1 <- lm(CT.mode ~ FDis + TRich + TShan + NStemsT + Rain.CV + Elev.Mean + Elev.CV + abs(Latitude), data=Mdata)

m2 <- lmer(CT.mode ~ (1|Continent)+ FDis + TRich + TShan + NStemsT + Rain.CV + Elev.Mean + Elev.CV + abs(Latitude), data=Mdata)


###### Model Terrestrial Vertebrate Functional Diversity
MammalFD <- read.csv("MammalFD.csv")
fit2 <- lm(MammalFD$FDis ~ Cstorage + FDis + TRich + TShan + NStemsT + NStemsL + RainTotal + Rain.CV + Elev.Mean + Elev.CV + abs(Latitude), data=MData)
step2 <- stepAIC(fit2, direction="both")


###### Model Terrestrial Vertebrate Taxonomic Diversity
