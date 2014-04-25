# For use in analysis of TEAM site species richness
# Pull latest data from SDSU
# Format data
# Subset appropriate site and year
# Run Dorazio et al. 2006 (or alternatively 2012) species richness model

# Go to http://vertica1.team.sdsc.edu:8787 and login to access SDSU server
# Use database query by Eric to pull latest data from server "team_db_query.R" in GitHub - teamcode - tools
# Use current helper functions by Jorge to format data in GitHub - teamcode - cameratrapping - "camera trap analysis functions.R"

####### PROCESS DATA to subset for various models - time consuming so don't run unless necessary ###########

#ctdata <- f.teamdb.query("camera trap")
#load(ctdata)
#alldata <- ctdata

#alldata<-f.fix.data2(alldata)
#Site.Code <- substr(alldata$Sampling.Unit.Name,4,6)
#alldata <- cbind(alldata, Site.Code)

# Set threshold to one day (24 hours * 60 minutes = 1440 minutes) and the number of replicates for each camera trap as 30 days
# Note that f.separate.events takes a good bit of time to run on complete CT dataset. Avoid replicating where possible.
#alldata <- f.order.data(alldata)
#eventsdata <- f.separate.events(alldata, thresh=(1440))
#allevents <- f.separate.events(alldata, thresh=(1))
#dataCleanTable <- table(allevents$Site.Code, allevents$Sampling.Period, allevents$bin)
#write.table(dataCleanTable, file="dataCleanTable.csv", row.names=TRUE, col.names=TRUE, sep=",")

table(alldata$Site.Code, alldata$Sampling.Period)


######## Start here to EXTRACT INPUT DATA FOR MODELS using Dorazio et al. 2006 JAGS code ###############
# Use "Sampling.Period" look at species richness separately for each year and "Site.Code" to designate which site to include
CAX.use <- eventsdata[eventsdata$Sampling.Period=="2012.01" & eventsdata$Site.Code=="CAX",]
PSH.use <- eventsdata[eventsdata$Sampling.Period=="2012.01" & eventsdata$Site.Code=="PSH",]
YAS.use <- eventsdata[eventsdata$Sampling.Period=="2012.01" & eventsdata$Site.Code=="YAS",]
BBS.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="BBS",]
BCI.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="BCI",]
#BIF.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="BIF",]
COU.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="COU",]
KRP.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="KRP",]
MAS.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="MAS",]
NNN.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="NNN",]
RNF.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="RNF",]
UDZ.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="UDZ",]
VB.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="VB-",]
YAN.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="YAN",]

data.use <- list(BBS.use, BCI.use, CAX.use, COU.use, KRP.use, MAS.use, NNN.use, PSH.use, RNF.use, UDZ.use, VB.use, YAN.use, YAS.use)

#subset species to the species in that site and with Include=1
splist <- read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist <- list()
  for(i in 1:length(data.use)){
    sitelist[[i]] <- unique(data.use[[i]]$bin)
    sitelist
  }  # Extract all observed CT species list for each site 

newsplist <- list()
  for(i in 1:length(data.use)){ 
    newsplist[[i]] <- subset(splist,splist$Unique_Name %in% sitelist[[i]] & splist$Include==1)
    newsplist
  } # Reduce species list and trait data to include only species

subdata1 <- list()
  for(i in 1:length(data.use)){
    subdata1[[i]] <- subset(data.use[[i]], data.use[[i]]$bin %in% newsplist[[i]]$Unique_Name) #this is the original camera trap data subsetted to these species
    subdata1
  } # Subset original camera trap data to species to include only

#### SPECIFY WHETHER SPECIES RICHNESS MODELS SHOULD BE RESTRICTED to MAMMALS or BIRDS

subdata <- subdata1
for(i in 1:length(data.use)) {
  subdata[[i]] <- subdata[[i]][subdata[[i]]$Class=="AVES",]
  subdata
} # Limit species by Class

####### Remove excess factor levels

subdata <- subdata
for(i in 1:length(data.use)) {
  subdata[[i]]$bin <- factor(subdata[[i]]$bin)
  subdata
} # Remove excess species from bin factor levels


events.use <- list()
  for(i in 1:length(data.use)){
    events.use[[i]] <- subdata[[i]][!duplicated(as.character(subdata[[i]]$grp)),]
    events.use
  } # Extract unique events based on the threshold set in f.separate.events

CTresults <- list()
for(i in 1:length(data.use)){
####### Set up for Dorazio et al. 2006 method for estimating species richness without covariates using JAGS #######
# Input data requires a table of the number of sampling events for each species ("bin") at each camera trap ("Sampling.Unit.Name")
# Input data requires the number of replicates in which each species was detected (i.e. max=nrepls)
X <- table(events.use[[i]]$bin, events.use[[i]]$Sampling.Unit.Name)
X <- as.matrix(X)  

# Control for the number of sampling day per camera trap
effortdays <- as.matrix(cbind(data.use[[i]]$Sampling.Unit.Name, f.start.minus.end(data.use[[i]])))
effortdays <- as.matrix(unique(effortdays))
effortdays <- effortdays[match(colnames(X), effortdays[,1]),]
edays <- as.numeric(effortdays[,2])
X <- round(t(t(X)*(edays/30)))

# augment data matrix with an arbitrarily large number of zero row vectors
nrepls=30
nzeroes = 125
n = dim(X)[1]
nsites = dim(X)[2]
Xaug = rbind(X, matrix(0, nrow=nzeroes, ncol=nsites))

# create arguments for bugs() or JAGS
sp.data = list(n=n, nzeroes=nzeroes, J=nsites, K=nrepls, X=Xaug)

# for bugs
# sp.params = list('alpha', 'beta', 'rho', 'sigma.u', 'sigma.v', 'omega', 'N')
# for JAGS
sp.params = c("alpha", "beta", "rho", "sigma.u", "sigma.v", "omega", "N")
Xinits <- ifelse(Xaug>0,1,0)

sp.inits = function() {
  omegaGuess = runif(1, n/(n+nzeroes), 1)
  psi.meanGuess = runif(1, .25,1)
  theta.meanGuess = runif(1, .25,1)
  rhoGuess = runif(1, 0,1)
  sigma.uGuess = 1
  sigma.vGuess = 1
  list(omega=omegaGuess, psi.mean=psi.meanGuess, theta.mean=theta.meanGuess, tau.u=1/(sigma.uGuess^2), tau.v=1/(sigma.vGuess^2), rho=rhoGuess,
       w=c(rep(1, n), rbinom(nzeroes, size=1, prob=omegaGuess)),
       phi=rnorm(n+nzeroes, log(psi.meanGuess/(1.-psi.meanGuess)), sigma.uGuess),
       eta=rnorm(n+nzeroes, log(theta.meanGuess/(1.-theta.meanGuess)), sigma.vGuess),
       Z = Xinits
       #Z = matrix(rbinom((n+nzeroes)*nsites, size=1, prob=psi.meanGuess), nrow=(n+nzeroes))
  )
}

library(rjags)

# Parallelize code to run on server
# Parallelize code 
# Load functions from bugsParallel.r 
n.chains <- 4
n.iter <- as.integer(250000)
n.burnin <- as.integer(125000)
n.thin <- 1
n.sims <- as.integer(20000)

fitparallel <- bugsParallel(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, model.file="/home/lbeaudrot/work/Species-Richness/MultiSpeciesSiteOccModel.txt", 
                            n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, n.sims=n.sims, digits=3, program=c("JAGS"))

print(fitparallel, digits=3)

sp.mean <- round(mean(fitparallel$sims.list$N), digits=2)
sp.median <- median(fitparallel$sims.list$N)
sp.mode <- as.numeric(names(sort(table(fitparallel$sims.list$N), decreasing=TRUE))[1])

sp.mean
sp.median
sp.mode

hist(fitparallel$sims.list$N, breaks=150, xlab="Bird Species Richness", 
     main=paste(events.use[[i]]$Site.Code[1], substr(events.use[[i]]$Sampling.Period[1],1,4), sep=" "), 
     sub=paste("Chains = ", n.chains, ",  Iterations =", n.iter, ",  Burnin =", n.burnin, ",  Thin =", n.thin, sep=" "))
text(40, 10000, paste("Mean", sp.mean, sep=" = "))
text(40, 7000, paste("Median", sp.median, sep=" = "))
text(40, 4000, paste("Mode", sp.mode, sep=" = "))

CTresults[[i]] <- fitparallel
CTresults
}

# Save model outputs
#save(CTresults, file="CTresults_bird.gzip",compress="gzip")
#save(CTresults, file="CTresults_mammal.gzip",compress="gzip")


# Create matrix to hold site level averages
CTnames <- vector(mode="character", length=length(data.use))
for(i in 1:length(data.use)){
  CTnames[[i]] <- paste(data.use[[i]]$Site.Code[1])
  CTnames
}

cols <- c("mean", "median", "mode")
CTaverages <- matrix(nrow=length(CTresults), ncol=3, dimnames=list(CTnames, cols))

# Function to calculate site level averages
f.sp.averages <- function(data) {
  for(i in 1:length(CTresults)){
    CTaverages[i,] <- cbind(round(mean(data[[i]]$sims.list$N), digits=2),
                            median(data[[i]]$sims.list$N),
                            as.numeric(names(sort(table(data[[i]]$sims.list$N), decreasing=TRUE))[1]))
  }
  CTaverages
}

CTaverages <- f.sp.averages(CTresults)
colnames(CTaverages) <- paste("CT", colnames(CTaverages), sep=".")

# Re-graph plots using results

for(i in 1:length(data.use)){
  hist(CTresults[[i]]$sims.list$N, breaks=150, xlab="Bird Species Richness", 
       main=paste(events.use[[i]]$Site.Code[1], substr(events.use[[i]]$Sampling.Period[1],1,4), sep=" "), 
       sub=paste("Chains = ", n.chains, ",  Iterations =", n.iter, ",  Burnin =", n.burnin, ",  Thin =", n.thin, sep=" "))
  legend("bottomright", legend=c(paste("Mean", CTaverages[i,1], sep=" = "), 
                                 paste("Median", CTaverages[i,2], sep=" = "), 
                                 paste("Mode", CTaverages[i,3], sep=" = ")), bty="n")
}

# Write output table to use in modeling
#write.csv(CTaverages, file="CTaverages_bird.csv", row.names=TRUE, col.names=TRUE)
#write.csv(CTaverages, file="CTaverages_mammal.csv", row.names=TRUE, col.names=TRUE)

