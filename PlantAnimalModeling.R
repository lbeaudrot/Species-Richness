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

model.data <- merge(CTaverages, plot.VGmean, by.x="Site.Code", by.y="Site.Code", all=TRUE)


# Calculate precipitation CV and add rainfall data to model.data
rain <- read.csv(file="Precipitation_Data.csv")
mo.rain <- as.matrix(rain[,2:13])
rain.CV <- vector()
for(i in 1:dim(mo.rain)[1]){
  rain.CV[i] <- sd(mo.rain[i,])/mean(mo.rain[i,])
  rain.CV
}
rain.data <- data.frame(rain$Site.Code, rain$total, rain.CV)
colnames(rain.data) <- c("Site.Code", "RainTotal", "Rain.CV")

model.data <- merge(model.data, rain.data, by.x="Site.Code", by.y="Site.Code", all=FALSE)
