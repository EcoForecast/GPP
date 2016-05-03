# ##' @param settings    PEcAn settings object
# ##' @param prior       data.frame of model parameter sample (nens X nstate)
# ##' @param obs.mean    data.frame of observations of the mean of variables
# ##' @param obs.sd      data.frame of observations of the sd of variables
# ##' @param processvar  flag for if process variance should be estimated or not
# ##' 
# ##' @description State Variable Data Assimilation: Ensemble Kalman Filter
# ##' 
# ##' @return NONE
# ##' 

##' @param IC          data.frame of initial condition sample (nens X nstate)

FORECAST <- ANALYSIS <- list()
enkf.params <- list()
aqq = array(0,dim=c(nt+1,ncol(IC),ncol(IC)))
bqq = numeric(nt+1)
CI.X1 <- matrix(0,3,nt) ; CI.X2 = CI.X1

wish.df <- function(Om,X,i,j,col){
  n = (Om[i,j]^2 + Om[i,i]*Om[j,j])/var(X[,col])
  return(n)
}

## numerical update of state and process error
AnalysisFilterQ <- "
model{ 
X.mod ~ dmnorm(muf,pf) ## Model Forecast

## add process error
q  ~ dwish(aq,bq)
Q <- inverse(q) 
X  ~ dmnorm(X.mod,q)

## Analysis
Y  ~ dmnorm(X,r)
}"       
  
###-------------------------------------------
### loop over time
###-------------------------------------------
for(t in 1:nt){
  
  ### load output    
  X = matrix(NA, nrow = nrow(IC), ncol = ncol(IC))
  for(i in 1:nens){
    X[i,] <- do.call(my.read.restart,args=list(outdir=outdir,run.id = run.id[[i]],
                                               time = total.time[t],spin.up = spin.up,
                                               X.vec = X[i,]))
  }
  FORECAST[[t]] = X
  
  ### Analysis step
  mu.f = apply(X,2,mean,na.rm=TRUE)
  Pf   = cov(X)
  Y    = t(obs.mean[t,])#obs$mean[t]
  R    = diag(as.numeric(obs.sd[t,])^2)#obs$sd[t]^2
  H    = diag(ncol(obs.mean))
  if(processvar == FALSE){
    K    = Pf%*%t(H)%*%solve(R+H%*%Pf%*%t(H))
    mu.a = mu.f + K%*%(Y-H%*%mu.f)
    Pa   = (diag(ncol(X)) - K%*%H)%*%Pf
  } else { 
    
    #### initial conditions
    bqq[1] <- length(mu.f)
    aqq[1,,] <- diag(length(mu.f))*bqq[1]
    
    ### analysis of model and data
    update = list(Y=Y, muf=mu.f, pf=solve(Pf,tol=0), aq=aqq[t,,], bq=bqq[t], r=solve(R))
    mod <- jags.model(file=textConnection(AnalysisFilterQ),
                      data=update,
                      n.adapt=1000,n.chains=3,
                      init=list(X.mod=as.vector(mu.f))) #inits for q?
    jdat <- coda.samples(mod,variable.names=c("X","q"),n.iter=10000) 
    
    ## update parameters  
    dat = as.matrix(jdat)
    dat = dat[3000:10000,]
    iq = grep("q",colnames(dat))
    iX = grep("X[",colnames(dat),fixed=TRUE)
    mu.a  = colMeans(dat[,iX])
    Pa  = cov(dat[,iX])
    Pa[is.na(Pa)]<- 0 
    
    CI.X1[,t] = quantile(dat[,iX[1]],c(0.025,0.5,0.975))
    CI.X2[,t] = quantile(dat[,iX[2]],c(0.025,0.5,0.975))
    
    mq = dat[,iq] #Omega, Precision
    q.bar = matrix(apply(mq,2,mean),length(mu.f),length(mu.f)) #Mean Omega, Precision
    
    col = matrix(1:length(mu.f)^2,length(mu.f),length(mu.f))
    WV = matrix(0,length(mu.f),length(mu.f))
    for(i in 1:length(mu.f)){
      for(j in 1:length(mu.f)){
        WV[i,j] <- wish.df(q.bar, X = mq, i=i, j=j, col=col[i,j])
      }
    }
    
    n = mean(WV) #n + 1
    if(n < length(mu.f)) n = length(mu.f)
    V = solve(q.bar)*n
    
    #ifelse(eigen(V)$values>0,eigen(V)$values,print("matrix not positive definite"))
    
    aqq[t+1,,] = V
    bqq[t+1] = n
  }
  
  enkf.params[[t]] = list(mu.f = mu.f, Pf = Pf, mu.a = mu.a, Pa = Pa) 
  
  ## update state matrix
  analysis = as.data.frame(rmvnorm(nens,mu.a,Pa,method="svd"))
  #analysis = exp(analysis)
  
  #HACK #not a good assumption #
  analysis[is.na(analysis)] <- 0
  analysis <- abs(analysis)
  #HACK
  names(analysis) = names(X)
  
  ANALYSIS[[t]] = analysis
  
  ### Forecast step
  if(t < nt){
    for(i in 1:nens){
      do.call(my.write.restart,args=list(outdir = outdir, run.id = run.id[[i]],
                                         time = total.time[t], settings = settings,
                                         analysis = analysis[i,c(1,2,4,3)],
                                         RENAME = TRUE,PLOT=FALSE))
    }
    ## start model run
    start.model.runs(settings,settings$database$bety$write)
  }
  
  
}  ## end loop over time
###-------------------------------------------