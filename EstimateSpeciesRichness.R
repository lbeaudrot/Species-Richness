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


######## Start here to CHANGE INPUT DATA FOR MODELS using Dorazio et al. 2006 JAGS code ###############
# Use "Sampling.Period" look at species richness separately for each year and "Site.Code" to designate which site to include
data.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="BIF",]

#subset species to the species in that site and with Include=1
splist<-read.csv("master_species_list_updated_7April2014.csv",h=T) #master list
sitelist<-unique(data.use$bin) #site list
newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
subdata<-subset(data.use, data.use$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
subdata<-f.correct.DF(subdata)


# The following line identifies unique events based on the threshold set in f.separate.events
events.use <- subdata[!duplicated(as.character(subdata$grp)),]


####### Set up for Dorazio et al. 2006 method for estimating species richness without covariates using JAGS #######

# Input data requires a table of the number of sampling events for each species ("bin") at each camera trap ("Sampling.Unit.Name")
# Input data requires the number of replicates in which each species was detected (i.e. max=nrepls)
X <- table(events.use$bin, events.use$Sampling.Unit.Name)
X <- as.matrix(X)  

# Control for the number of sampling day per camera trap
effortdays <- as.matrix(cbind(data.use$Sampling.Unit.Name, f.start.minus.end(data.use)))
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
n.thin <- 3
n.sims <- as.integer(20000)

fitparallel <- bugsParallel(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, model.file="/home/lbeaudrot/work/Species-Richness/MultiSpeciesSiteOccModel.txt", 
                             n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, n.sims=n.sims, digits=3, program=c("JAGS"))
#plot.chains(jmodparallel)


#fit <- jags.model(file='MultiSpeciesSiteOccModel.txt', data=sp.data, inits=sp.inits, n.chains=3, n.adapt=1000)

#update(fit, n.iter=125000, by=100, progress.bar='text')   # burn in

#post = coda.samples(fit, sp.params, n.iter=125000, thin=3)  # posterior sample
#mypost = as.matrix(post, chain=TRUE)

#post2 = coda.samples(fit, params, n.iter=125000, thin=3) # another posterior sample
#mypost2 = as.matrix(post2, chain=TRUE)


#fit = bugs(sp.data, sp.inits, sp.params,
#model.file='MultiSpeciesSiteOccModel.txt',
#debug=F, n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)

print(fitparallel, digits=3)
#plot(fitparallel)

sp.mean <- round(mean(fitparallel$sims.list$N), digits=2)
sp.median <- median(fitparallel$sims.list$N)
sp.mode <- as.numeric(names(sort(table(fitparallel$sims.list$N), decreasing=TRUE))[1])

sp.mean
sp.median
sp.mode

hist(fitparallel$sims.list$N, breaks=150, xlab="Species Richness", 
     main=paste(events.use$Site.Code[1], substr(events.use$Sampling.Period[1],1,4), sep=" "), 
     sub=paste("Chains = ", n.chains, ",  Iterations =", n.iter, ",  Burnin =", n.burnin, ",  Thin =", n.thin, sep=" "))
text(100, 5000, paste("Mean", sp.mean, sep=" = "))
text(100, 3000, paste("Median", sp.median, sep=" = "))
text(100, 1000, paste("Mode", sp.mode, sep=" = "))


 # Temporarily store site specific outputs

#BBSfit <- fitparallel
#BCIfit <- fitparallel
BIFfit <- fitparallel
#CAXfit <- fitparallel
#COUfit <- fitparallel
#KRPfit <- fitparallel
#MASfit <- fitparallel
#NNNfit <- fitparallel
#PSHfit <- fitparallel
#RNFfit <- fitparallel
#UDZfit <- fitparallel
#VBfit <- fitparallel
#YANfit <- fitparallel
#YASfit <- fitparallel

# Save model outputs
save(BIFfit, file="BIFfit.gzip",compress="gzip")


CTresults <- list(BBSfit, BCIfit, CAXfit, COUfit, KRPfit, MASfit, NNNfit, PSHfit, RNFfit, UDZfit, VBfit, YANfit, YASfit)
CTnames <- c("BBS", "BCI", "CAX", "COU", "KRP", "MAS", "NNN", "PSH", "RNF", "UDZ", "VB-", "YAN", "YAS")
cols <- c("mean", "median", "mode")
CTaverages <- matrix(nrow=length(CTresults), ncol=3, dimnames=list(CTnames, cols))

sp.averages <- function(data) {
  for(i in 1:length(CTresults)){
    CTaverages[i,] <- cbind(round(mean(data[[i]]$sims.list$N), digits=2),
        median(data[[i]]$sims.list$N),
        as.numeric(names(sort(table(data[[i]]$sims.list$N), decreasing=TRUE))[1]))
  }
  CTaverages
  }

CTaverages <- sp.averages(CTresults)

write.csv(CTaverages, file="CTaverages.csv", row.names=TRUE, col.names=TRUE)



# Loop over all sites while measuring time

#mfit<-list()
#for(i in 1:length(unique(alldata$Site.Code))){}
# replace with appropriate model code
#timefit<-system.time(mfit[[i]] <- jags(jags.data[[i]], inits[[i]], params, "fullmodel6.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb))


temp<-mfit[[i]]
save(NAKfit,file=paste("fit-",data.use$Site.Code[1],sep=""))
print(paste("Done with site ",levels(alldata$Site.Code)[i],"; took ",round(as.numeric(timefit[3]/60))," minutes.",sep=""))
}

# from db query
#filename= paste("veg_data",sysdate,".gzip",sep="")
#save(result, file=filename,compress="gzip")

