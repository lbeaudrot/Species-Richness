###############################################################################
# bugsParallel
###############################################################################
#
# Version 0.2
#
# Author: Mathias Tobler (matobler@gmx.net)
#
# Description: The bugsParallel function runs multiple MCMC chains on different 
#              CPU cores in parallel. It replaces the bugs() function. 
#              It returns a bugs objedct.
#
# Usage:      out <- bugsParallel(...) 
#
# Argumetns:  The arguments for this function are identical to the arguments 
#             use by the bugs() function in the R2WinBUGS package with the
#             addition of:
#
#             program There is an additional option "JAGS" to run the model in 
#                     JAGS using the rjags package.
#             n.proc  The number of processors to use. The default equals the 
#                     number of chains.
#             type    The cluster type used by the makeCluster() function.
#
# Notes:      I have only tested this on a Windows machine.
#             This function currently only works on a local cluster but it could 
#             be modified for a network cluster by creating the cluster  
#             in the main code and then passing the cluster object to the 
#             function. On a network cluster the model file will need to be
#             accessible by all slaves through the same path or will need to 
#             be passed as a function.
#
# Requires: parallel, R2WinBUGS, rjags
#
#############################################################################



bugsParallel<-function(data, inits, parameters.to.save, model.file = "model.bug", 
    n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
    n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)), 
    n.sims = 1000, bin = (n.iter - n.burnin)/n.thin, debug = FALSE, 
    DIC = TRUE, digits = 5, codaPkg = FALSE, bugs.directory = "c:/Program Files/WinBUGS14/", 
    program = c("WinBUGS", "OpenBUGS", "winbugs", "openbugs","JAGS"), 
    working.directory = NULL, clearWD = FALSE, useWINE = .Platform$OS.type != 
        "windows", WINE = NULL, newWINE = TRUE, WINEPATH = NULL, 
    bugs.seed = NULL, summary.only = FALSE, save.history = !summary.only, 
    over.relax = FALSE,n.proc=n.chains,type="PSOCK"){

    #load parallel package
    if(!require(parallel))stop("Package parallel required for this function.")
    #load R2WinBUGS
    if(!require(R2WinBUGS))stop("Package R2WinBUGS required for this function.")
        
    if(!is.function(model.file))
    {
      if(sum(unlist(gregexpr("/",model.file)))<0 & sum(unlist(gregexpr("\\\\",model.file)))<0)
      {
        model.file<-paste(getwd(),model.file,sep="\\")
      }

    }
    
    program <- match.arg(program)
    
    nproc<-min(detectCores(),n.proc)
    cl <- makeCluster(nproc,type=type)

    #Export variables
#    clusterExport(cl,list("data", "inits", "parameters.to.save", "model.file",
#    "n.chains","n.iter", "n.burnin",
#    "n.thin","n.sims","bin", "debug","DIC","digits", "codaPkg", "bugs.directory",
#    "program","working.directory","clearWD","useWINE","WINE", "newWINE", "WINEPATH",
#    "bugs.seed", "summary.only", "save.history","over.relax"),envir=environment())
    clusterExport(cl,expList <- as.list(objects(pos = globalenv())))
    #set random number generator for cluster
    clusterSetRNGStream(cl)
    
    #WinBUGS or OpenBUGS
    if (program %in% c("WinBUGS", "OpenBUGS", "winbugs", "openbugs")){ 
        if(!is.element("R2WinBUGS", installed.packages()[,1]))stop("Package R2WinBUGS required to run models with ",program,".")
        #load library
        clusterEvalQ(cl,library(R2WinBUGS))
        #run chains on cluster
        bugsresults<-clusterApplyLB(cl,1:n.chains,function(x, ...){bugs(...)},data=data, inits=inits, parameters.to.save=parameters.to.save, model.file=model.file,
        n.chains = 1, n.iter = n.iter, n.burnin = n.burnin,
        n.thin = n.thin, n.sims = n.sims, bin = bin, debug = debug,
        DIC = DIC, digits = digits, codaPkg = FALSE, bugs.directory =  bugs.directory,
        program = program, working.directory =working.directory ,clearWD = clearWD, useWINE = useWINE, newWINE = newWINE, WINEPATH = WINEPATH,
        bugs.seed = bugs.seed, summary.only = FALSE, save.history = !summary.only,
        over.relax = over.relax
        )

        mcmc.list<-vector("list", n.chains)
        strt <- n.burnin + 1
        end <- n.iter
        for(n in 1:n.chains)
        {
          tmp1 <- bugsresults[[n]]$sims.array[,1,]
          mcmc.list[[n]]<- mcmc(tmp1, start = strt, end = end, thin = n.thin)
        }
        mcmc.list<-as.mcmc.list(mcmc.list) 
        mcmc.array<-as.array(mcmc.list)
        mcmc.array<-aperm(mcmc.array,c(1,3,2)) 
        results<-as.bugs.array(mcmc.array,model.file=model.file,program=program,DIC=DIC,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin)
                
    } else if(program %in% c("JAGS")) {
        if(!is.element("rjags", installed.packages()[,1]))stop("Package rjags required to run models with ",program,".")
        #load library
        clusterEvalQ(cl,library(rjags))
        if (DIC) {
          parameters.to.save <- c(parameters.to.save, "deviance")
          clusterEvalQ(cl,load.module("dic", quiet = TRUE))
          clusterEvalQ(cl,load.module("glm", quiet = TRUE))
        }  
        #run chains on cluster
        runJags<-function(data, inits, parameters.to.save, model.file = "model.bug", 
                  n.chains = 3, n.iter = 2000, n.burnin = floor(n.iter/2), 
                  n.thin = max(1, floor(n.chains * (n.iter - n.burnin)/n.sims)))
                  {      
                   jags<-jags.model(file=model.file,data=data,inits=inits,n.chains=n.chains,n.adapt=n.burnin)
                   coda.samples(jags,variable.names=parameters.to.save,n.iter=(n.iter-n.burnin),thin=n.thin)
                   }   
        jagsresults<-clusterApplyLB(cl,1:n.chains,function(x, ...){runJags(...)},data=data, inits=inits, parameters.to.save=parameters.to.save, model.file=model.file,
        n.chains = 1, n.iter = n.iter, n.burnin = n.burnin,
        n.thin = n.thin
        )        
                
        mcmc.list<-list()      
        for(n in 1:length(jagsresults))
        {
          mcmc.list[[n]]<-jagsresults[[n]][[1]]
        }   
        class(mcmc.list)<-"mcmc.list" 
        mcmc.array<-as.array(mcmc.list)
        mcmc.array<-aperm(mcmc.array,c(1,3,2))
        ord <- order.variables(dimnames(mcmc.array)$var)
        mcmc.array<-mcmc.array[,,ord] 
        results<-as.bugs.array(mcmc.array,model.file=model.file,program=program,DIC=DIC,n.iter=n.iter,n.burnin=n.burnin,n.thin=n.thin) 
    }
    
    stopCluster(cl)
    results
}


order.variables<-function(x)
{ 
  a<-vector(mode="numeric",length(x))
  for(n in 1:length(x))
  {
     index<-R2WinBUGS:::decode.parameter.name(x[n])$index
     if(!is.na(index[1]))
     {
      a[n]<-max(index)
     }
  }
  width=length(strsplit(as.character(max(a)),"")[[1]])

  for(n in 1:length(x))
  {
     temp<-R2WinBUGS:::decode.parameter.name(x[n])
     if(!is.na(temp$indexes[1]))
     {
       index<-paste(formatC(temp$indexes,format="f",flag="0",width=width,digits=0),sep="",collapse=",")
       x[n]<-paste(temp$root,"[",index,"]",sep="",collapse=",")
     }
  }
  d<-which(x=="deviance")
  x<-order(x)
  if(length(d)>0)
  {
    x<-x[x!=d]
    x<-c(x,d)
  }
  x
}


plot.chains<-function(x)
{

  sims<-x$sims.array
  samples<-dim(sims)[1] #round((x$n.iter-x$n.burnin)/x$n.thin,0)
  par<-dimnames(sims)[[3]]
  npar<-length(par)
  n.chains<-x$n.chains
  op <- par(mfrow = c(min(ceiling(npar/2),3),2),ask=TRUE)
  for(j in 1:npar)
  {
    plot(sims[,1,j]~c(1:samples), type="n",xlab="iteration",ylab="",  main=par[j],ylim=c(min(sims[,,j])-abs(min(sims[,,j])*0.2),max(sims[,,j])+abs(max(sims[,,j])*.2)))
    for(i in 1:n.chains)
    {
      lines(sims[,i,j]~c(1:samples), type="l",col=rainbow(n.chains)[i])
    }
  }
  par(op)
}
