# Calcualte Species Richness using Dorazio et al's 2012 JAGS model
# Following initial data processing and set up from EstimateSpeciesRichness.R file

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



