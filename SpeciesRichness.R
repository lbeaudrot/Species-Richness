# For use in analysis of TEAM site species richness
# Pull latest data from SDSU
# Format data
# Subset appropriate site and year
# Run Dorazio 2006 species richness model

# Go to http://vertica1.team.sdsc.edu:8787 and login to access SDSU server
# Use database query by Eric to pull latest data from server "team_db_query.R" in GitHub - teamcode - tools
# Use current helper functions by Jorge to format data in GitHub - teamcode - cameratrapping - "camera trap analysis functions.R"

#ctdata <- f.deamdb.query(dataset=="camera trap")
#load(ctdata)
data <- ctdata

data<-f.fix.data2()
data <- f.order.data(data)
# Set threshold to one day (24 hours * 60 minutes = 1440 minutes) and the number of replicates for each camera trap as 30 days
data <- f.separate.events(data, thresh=(1440))
Site.Code <- substr(ctdata$Sampling.Unit.Name,4,6)
data <- c(data, Site.Code)

#subset species to the species in that site and with Include=1
splist<-read.csv("master_species_list.csv",h=T) #master list
sitelist<-unique(data$bin) #site list
newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
subdata<-subset(data,data$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
subdata<-f.correct.DF(subdata)

# Use "Sampling.Period" look at species richness separately for each year
data.use <- subdata[subdata$Sampling.Period=="2011.01",]
# Use "Site.Code" to designate which site to include. Eventually write a loop over all sites.
data.use <- data.use[data.use$Site.Code=="BCI",]


# The following line identifies unique events based on the threshold set in f.separate.events
events.use <- data.use[!duplicated(as.character(data.use$grp)),]

# Input data requires a table of the number of sampling events for each species ("bin") at each camera trap ("Sampling.Unit.Name")
# WinBUGS code appears to be failing for cells where the number of events is greater than the number of sampling replicates (nrepls)
# This suggests that quantities in input matrix should be the counts of the sampling periods in which the species was observed as present, not the number of events
# If correct, we should later check to see how sensitive the results are to variation in nrepls

# Input data requires the number of replicates in which each species was detected (i.e. max=nrepls)
X <- table(events.use$bin, events.use$Sampling.Unit.Name)
X <- as.matrix(X)  


# augment data matrix with an arbitrarily large number of zero row vectors
nzeroes = 125
n = dim(X)[1]
nsites = dim(X)[2]
Xaug = rbind(X, matrix(0, nrow=nzeroes, ncol=nsites))

# create arguments for bugs()
sp.data = list(n=n, nzeroes=nzeroes, J=nsites, K=nrepls, X=Xaug)

sp.params = list('alpha', 'beta', 'rho', 'sigma.u', 'sigma.v', 'omega', 'N')

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
       Z = matrix(rbinom((n+nzeroes)*nsites, size=1, prob=psi.meanGuess), nrow=(n+nzeroes))
  )
}

nrepls = 30
n.chains=4
n.iter=250000
n.burnin=125000 
n.thin=3

fit = bugs(sp.data, sp.inits, sp.params,
           model.file='MultiSpeciesSiteOccModel.txt',
           debug=F, n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)
print(fit, digits=3)
plot(fit)

sp.mean <- round(mean(fit$sims.list$N), digits=2)
sp.median <- median(fit$sims.list$N)
sp.mode <- as.numeric(names(sort(table(fit$sims.list$N), decreasing=TRUE))[1])

hist(fit$sims.list$N, breaks=150, xlab="Species Richness", 
     main=paste(events.use$Site.Name[1], events.use$Sampling.Period[1], sep=" "), 
     sub=paste("Chains = ", n.chains, ",  Iterations =", n.iter, ",  Burnin =", n.burnin, ",  Thin =", n.thin, sep=" "))
text(100, 3000, paste("Mean", sp.mean, sep=" = "))
text(100, 2000, paste("Median", sp.median, sep=" = "))
text(100, 1000, paste("Mode", sp.mode, sep=" = "))

# Need to save model outputs
