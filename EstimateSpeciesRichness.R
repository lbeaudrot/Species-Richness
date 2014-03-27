# For use in analysis of TEAM site species richness
# Pull latest data from SDSU
# Format data
# Subset appropriate site and year
# Run Dorazio et al. 2006 (or alternatively 2012) species richness model

# Go to http://vertica1.team.sdsc.edu:8787 and login to access SDSU server
# Use database query by Eric to pull latest data from server "team_db_query.R" in GitHub - teamcode - tools
# Use current helper functions by Jorge to format data in GitHub - teamcode - cameratrapping - "camera trap analysis functions.R"

####### PROCESS DATA to subset for various models - time consuming so don't run unless necessary ###########

#ctdata <- f.teamdb.query(dataset=="camera trap")
#load(ctdata)
#alldata <- ctdata

#alldata<-f.fix.data2(alldata)
#Site.Code <- substr(alldata$Sampling.Unit.Name,4,6)
#alldata <- cbind(alldata, Site.Code)

# Set threshold to one day (24 hours * 60 minutes = 1440 minutes) and the number of replicates for each camera trap as 30 days
# Note that f.separate.events takes a good bit of time to run on complete CT dataset. Avoid replicating where possible.
#alldata <- f.order.data(alldata)
#eventsdata <- f.separate.events(alldata, thresh=(1440))



######## Start here to CHANGE INPUT DATA FOR MODELS using Dorazio et al. 2012 JAGS code ###############
# Use "Sampling.Period" look at species richness separately for each year and "Site.Code" to designate which site to include
data.use <- eventsdata[eventsdata$Sampling.Period=="2011.01" & eventsdata$Site.Code=="YAS",]

#subset species to the species in that site and with Include=1
splist<-read.csv("master_species_list.csv",h=T) #master list
sitelist<-unique(data.use$bin) #site list
newsplist<-subset(splist,splist$Unique_Name %in% sitelist & splist$Include==1)
subdata<-subset(data.use, data.use$bin %in% newsplist$Unique_Name) #this is the original camera trap data subsetted to these species
subdata<-f.correct.DF(subdata)


# The following line identifies unique events based on the threshold set in f.separate.events
events.use <- subdata[!duplicated(as.character(subdata$grp)),]


####### Set up for Dorazio et al. 2006 method for estimating species richness without covariates using WinBUGS #######

# Input data requires a table of the number of sampling events for each species ("bin") at each camera trap ("Sampling.Unit.Name")
# Input data requires the number of replicates in which each species was detected (i.e. max=nrepls)
X <- table(events.use$bin, events.use$Sampling.Unit.Name)
X <- as.matrix(X)  

# Control for the number of sampling day per camera trap
effortdays <- as.matrix(cbind(data.use$Sampling.Unit.Name, f.start.minus.end(data.use)))
effortdays <- as.matrix(unique(effortdays))
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

n.chains=4
n.iter=250000
n.burnin=125000 
n.thin=3


# Parallelize code to run on server
# Parallelize code 
# Load functions from bugsParallel.r 
n.chains <- 4
n.iter <- as.integer(100)
n.burnin <- as.integer(50)
n.thin <- 3
n.sims <- as.integer(10)

fitparallel <- bugsParallel(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, model.file="/home/lbeaudrot/work/Species-Richness/MultiSpeciesSiteOccModel.txt", 
                             n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin, n.sims=n.sims, digits=3, program=c("JAGS"))
#plot.chains(jmodparallel)


 library(rjags)
fit <- jags.model(file='MultiSpeciesSiteOccModel.txt', data=sp.data, inits=sp.inits, n.chains=3, n.adapt=1000)

update(fit, n.iter=125000, by=100, progress.bar='text')   # burn in

post = coda.samples(fit, sp.params, n.iter=125000, thin=3)  # posterior sample
mypost = as.matrix(post, chain=TRUE)

#post2 = coda.samples(fit, params, n.iter=125000, thin=3) # another posterior sample
#mypost2 = as.matrix(post2, chain=TRUE)


#fit = bugs(sp.data, sp.inits, sp.params,
#model.file='MultiSpeciesSiteOccModel.txt',
#debug=F, n.chains=n.chains, n.iter=n.iter, n.burnin=n.burnin, n.thin=n.thin)

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





































# Set up for Dorazio et al. 2012 method
#nrepls = 30   # number of replicates per sample site


# Input data requires the number of replicates in which each species was detected (i.e. max=nrepls)
#Ymat <- table(events.use$bin, events.use$Sampling.Unit.Name)
#Ymat <- as.matrix(Ymat)


# Control for the number of sampling day per camera trap
#effortdays <- as.matrix(cbind(data.use$Sampling.Unit.Name, f.start.minus.end(data.use)))
#effortdays <- as.matrix(unique(effortdays))
#edays <- as.numeric(effortdays[,2])
#Ymat <- round(t(t(Ymat)*(edays/30)))

#nsites = dim(Ymat)[2]
#n = dim(Ymat)[1]
#nzeros = 150 - n  #  75 = max species richness

# For null model, set all covariates to zero
#X <- matrix(0,nsites,4)
#ncovs = dim(X)[2]

# For covariate model, set up covariate data:
#Xmat = GetSiteCovariates()
#x1 = as.vector(Xmat[,'lat'])
#x1 = (x1 - mean(x1))/sd(x1)
#x2 = as.vector(Xmat[,'LAI'])
#x2 = (x2 - mean(x2))/sd(x2)
#x3 = as.vector(Xmat[,'GSF'])
#x3 = (x3 - mean(x3))/sd(x3)
#x4 = as.vector(Xmat[,'elev'])
#x4 = (x4 - mean(x4))/sd(x4)
#X = cbind(x1, x2, x3, x4)
#ncovs = dim(X)[2]

# augment data matrix with all-zero row vectors
#Yaug = rbind(Ymat, matrix(0, nrow=nzeros, ncol=nsites))
#data = list(n=n, nzeros=nzeros, R=nsites, J=nrepls, Y=Yaug, X=X, ncovs=ncovs)


# create arguments for JAGS

# .... list of output parameters
#params = c('alpha0', 'beta0', 'rho', 'sigma.b0', 'sigma.a0', 'omega', 'N', 'betaVec', 'sigma.b', 'p', 'b0', 'bVec', 'Z', 'delta')

# .... function to initialize Markov chain
#inits = function() {
# omegaGuess = runif(1, n/(n+nzeros), 1)
# betaVecGuess = rnorm(ncovs, 0,1)
# psi.meanGuess = runif(1, .25,1)
# beta0Guess = log(psi.meanGuess) - log(1-psi.meanGuess)
# p.meanGuess = runif(1, .25,1)
# alpha0Guess = log(p.meanGuess) - log(1-p.meanGuess)
# rhoGuess = runif(1, 0,1)
# tvar.sigma.b0Guess =  rnorm(1,0,1)
# tvar.sigma.a0Guess =  rnorm(1,0,1)
# sigma.b0Guess = abs(tvar.sigma.b0Guess)
# sigma.a0Guess = abs(tvar.sigma.a0Guess)
# tvar.sigma.bGuess = rnorm(ncovs, 0, 1)
# deltaGuess = rbinom(ncovs, size=1, prob=0.5)
# Zguess = matrix(0, nrow=n+nzeros, ncol=nsites)
# ind = Yaug>0
# Zguess[ind] = 1

# list(omega=omegaGuess, beta0=beta0Guess, alpha0=alpha0Guess, betaVec=betaVecGuess, rho=rhoGuess, 
# delta = deltaGuess,
# w=c(rep(1, n), rbinom(nzeros, size=1, prob=omegaGuess)),
# b0=rnorm(n+nzeros, beta0Guess, sigma.b0Guess),
# a0=rnorm(n+nzeros, alpha0Guess, sigma.a0Guess), Z=Zguess,
# tvar.sigma.b0=tvar.sigma.b0Guess, tvar.sigma.a0=tvar.sigma.a0Guess, tvar.sigma.b=tvar.sigma.bGuess
# )
#}


# .... model definition in native JAGS code

#modelFilename = "MultiSpeciesOccModel.txt"

#cat("
#    model {


# prior distributions

#    omega ~ dunif(0,1)
#    rho ~ dunif(-1,1)

#    t.nu <- 7.763179      # Uniform prior
#    t.sigma <- 1.566267   # Uniform prior
#    ## t.nu <- 5.100         # Jeffreys' prior
#    ## t.sigma <- 2.482      # Jeffreys' prior

#    beta0 ~ dt(0, pow(t.sigma,-2), t.nu)
#    alpha0 ~  dt(0, pow(t.sigma,-2), t.nu)


#    tvar.sigma.b0 ~ dt(0,1,1)  # Cauchy distribution
#    sigma.b0 <- abs(tvar.sigma.b0)  # half-Cauchy distribution

#    tvar.sigma.a0 ~ dt(0,1,1)  # Cauchy distribution
#    sigma.a0 <- abs(tvar.sigma.a0)  # half-Cauchy distribution


#    for (j in 1:ncovs) {
#    betaVec[j] ~  dt(0, pow(t.sigma,-2), t.nu)

#    tvar.sigma.b[j] ~ dt(0,1,1)  # Cauchy distribution
#    sigma.b[j] <- abs(tvar.sigma.b[j])  # half-Cauchy distribution

#    delta[j] ~ dbern(0.5)
#    }


# model of inclusion of covariates X
#    for (k in 1:R) {
#    for (j in 1:ncovs) {
#    X.in[k,j] <- delta[j]*X[k,j]
#    }
#    }



# model of species- and site-specific occurrences and detections

#    for (i in 1:(n+nzeros)) {
#    w[i] ~ dbern(omega)
#    b0[i] ~ dnorm(beta0, pow(sigma.b0,-2))

#    a0[i] ~ dnorm(alpha0 + (rho*sigma.a0/sigma.b0)*(b0[i] - beta0),  pow(sigma.a0,-2)/(1 - pow(rho,2)))
#    logit(p[i]) <- a0[i]

#    for (j in 1:ncovs) {
#    bVec[i,j] ~ dnorm(betaVec[j],  pow(sigma.b[j],-2))
#    }

#    for (k in 1:R) {
#    logit(psi[i,k]) <- b0[i] + inprod(bVec[i,], X.in[k,])
#    Z[i,k] ~ dbern(psi[i,k]*w[i])
#    Y[i,k] ~ dbin(p[i]*Z[i,k], J)
#    }
#    }

# calculate derived parameters
#    N <- sum(w)

#    }
#    ", fill=TRUE, file=modelFilename)




# Parallelize code to run on server
# Parallelize code 
# Load functions from bugsParallel.r 
#jmodparallel <- bugsParallel(data=data, inits=inits, parameters.to.save=params, model.file="MultiSpeciesOccModel.txt", 
#                             n.chains=4, n.iter=100, n.burnin=10, n.thin=3, digits=3, program=c("JAGS"))
#plot.chains(jmodparallel)


# fit model to data using JAGS code
#library(rjags)
#load.module('glm')
#jmod = jags.model(file=modelFilename, data=data, inits=inits, n.chains=5, n.adapt=10000)
#update(jmod, n.iter=50000, by=100, progress.bar='text')   # burn in

#post = coda.samples(jmod, params, n.iter=100000, thin=50)  # posterior sample
#mypost = as.matrix(post, chain=TRUE)

#post2 = coda.samples(jmod, params, n.iter=100000, thin=50) # another posterior sample
#mypost2 = as.matrix(post2, chain=TRUE)

# new file name for commit
# This file should now be modified













