#'Clustering by fast searching and finding density peaks
#'
#'@examples
#'samp_matrix<-rbind(matrix(rnorm(n=4000,mean=0,sd=1),ncol=4),
#'                   matrix(runif(n=2000,0,3),ncol=4),
#'                   matrix(rnorm(n=1000,mean=0.2,sd=0.5),ncol=4),
#'                   matrix(rnorm(n=800,mean=1.5,sd=0.6),ncol=4))
#'target<-factor(c(rep(0,1500),rep(1,450)),levels=c(1,0))
#'col<-c(rep(1,1000),rep(2,500),rep(3,250),rep(4,200))
#'plot(samp_matrix[,1:2],col=col)
#'
#'x<-runif(1000,-0.9,0.9)
#'y<-sqrt(runif(1000,1,2)-x**2)*sample(c(1,-1),1000,replace=T)
#'y1<-runif(1000,-0.5,0.5)
#'x1<-sqrt(runif(1000,0.55,1)-y1**2)*sample(c(1,-1),1000,replace=T)
#'samp_matrix<-as.matrix(cbind(c(x,x1),c(y,y1)))
#'
#'#unsupervised clustering
#'dist<-dist(samp_matrix)
#'dc<-estimateDc(samp_matrix,dist,pmin=0.005)#when the points from different distribution are too close,the neighbor rate should be smaller
#'param<-estimateParam(samp_matrix,dist=dist,dc=dc,gaussian=TRUE)
#'res<-pcluster(samp_matrix,param=param)
#'summary(res)
#'plot(samp_matrix[,1:2],col=res$cluster)
#'points(res$parameters$peaks$peaks[,1],res$parameters$peaks$peaks[,2],col=4,pch=19)
#'
#'#supervised clustering
#'param1<-estimateParam(x=samp_matrix,y=target,dist=dist,dc=dc,gaussian=TRUE)
#'res1<-pcluster(x=samp_matrix,y=target,param=param1)
#'summary(res1)
#'plot(samp_matrix[,1:2],col=res1$cluster+1)
#'points(res1$parameters$peaks$peaks[,1],res1$parameters$peaks$peaks[,2],col=4,pch=19)
#'
#'@docType package
#'@name petrel pacakge

NULL

#'Calculate distance matrix
#'
#'The function create a memory-map distance matrix via bigmemory,which can handle massive datasets
#'
#'@param x A matrix in which the data is suggested to be standardized
#'
#'@return A big.matrix style distance matrix
#'
#'@export
#'
dist<-function(x){
  #calculate distance matrix
  nrow<-nrow(x)
  ncol<-ncol(x)
  dist_len<-nrow*(nrow+1)/2
  temp_dist<-big.matrix(nrow=dist_len,ncol=1)
  get_dist(x,temp_dist@address,nrow,ncol)
  temp_dist
}

#'@title Estimate distance cutoff value
#'
#'@description This function calculates a distance cutoff value for the distance matrix that
#'makes the avarage neighbor rate (percent of nodes within the distance cutoff value)
#'fall between 0.01 and 0.02
#'
#'@param x A matrix
#'
#'@param dist Big.matrix stype distance matrix which calculate by \code{\link{dist}}
#'
#'@param pmin Minimum of neighbor rate, default value is 0.01
#'
#'@param pmax Maximum of neighbor rate, default value is 0.02
#'
#'@export
estimateDc<-function(x,dist,pmin=0.01,pmax=0.02){
  #estimate Dc
  res<-estimateDc_cpp(dist@address,nrow(x),pmin,pmax)
  res
}

#'@title Estimate peaks and calculate other parameters
#'
#'@description This function does two works: first, uses supplied distance cutoff value, dc to calculate rho and delta. Second
#',detects peaks in rho*delta graphic.
#'
#'@param x A matrix
#'
#'@param y Optional input for supervised clustering, y should be a factor in whose levels the positive target must be declared in the front
#'
#'@param dist Big.matrix stype distance matrix which calculated by \code{\link{dist}}
#'
#'@param thres.rho,thres.delta Optional inputs,the rho and delta thresholds to detecting peaks
#'
#'@param dc distance cutoff value calculated by \code{\link{estimateDc}}
#'
#'@param gaussian default TRUE,caculate rho and delta by estimating gaussian kernel density, otherwise rho is calculated
#'by using a simple summation of points within distance cutoff of the specific points
#'
#'@export
estimateParam<-function(x,y=NULL,dist,thres.rho=NULL,thres.delta=NULL,dc,gaussian=TRUE){
  nrow<-nrow(x)
  
  #calculate rho & delta
  if(!is.null(y)){
    if(!is.factor(y)) cat('The target should be factor\n')
    y<-2-as.numeric(y)
    rho<-get_srho(dist@address,y,dc[[1]],nrow,gaussian)
  }else{
    rho<-get_rho(dist@address,dc[[1]],nrow,gaussian)
  }
  delta<-get_delta(dist@address,rho,nrow)
  
  #estimate peaks
  if(is.null(thres.rho) || is.null(thres.delta)){
    plot(rho,delta)
    cat('Please click on plot to select thresholds\n')
    thres.point<-locator(1)
    if(is.null(thres.rho)) thres.rho<-thres.point$x
    if(is.null(thres.delta)) thres.delta<-thres.point$y
  }
  
  #plot and check the peaks
  peaks.loc<-intersect(which(rho>=thres.rho),which(delta>=thres.delta))
  plot(rho,delta,main='The Decision Graph')
  points(rho[peaks.loc],delta[peaks.loc],col=2:(1+length(peaks.loc)),pch=19)
  
  res<-list(peaks=list(peaks=x[peaks.loc,],peaks.loc=peaks.loc),rho=rho,delta=delta,dc=dc,distanceMatrix=dist)
  class(res)<-'pcluster'
  res
}

#'@title Find clusters
#'
#'@param x A matrix
#'
#'@param y Optional input for supervised clustering
#'
#'@param param Result calculated by \code{\link{estimateParam}}
#'
#'@export
pcluster<-function(x,y=NULL,param){
  seed<-param$peaks$peaks.loc-1
  res<-fast_cluster(param$distanceMatrix@address,seed,param$rho,param$dc$dc,nrow(x))
  
  if(is.null(y)){
    center<-apply(x,2,mean)
    totss<-totss(x,center,nrow(x),ncol(x))
    x1<-x[res>0,]
    center1<-apply(x1,2,mean)
    adj_totss<-totss(x1,center1,nrow(x1),ncol(x1))
    withinss<-withinss(x,seed,res)
    RS<-1-sum(withinss)/totss
    adj.RS<-1-sum(withinss)/sqrt(totss*adj_totss)
    
    rtun<-list(cluster=res,type='unsupervised',parameters=param,evaluation=list(totss=totss,adj_totss=adj_totss,withinss=withinss,RS=RS,adj.RS=adj.RS))
  }else{
    density<-t(prop.table(table(res,y),1)[,1])
    x1<-x[res>0,]
    center<-apply(x1,2,mean)
    adj_totss<-totss(x1,center,nrow(x1),ncol(x1))
    withinss<-withinss(x,seed,res)
    adj.RS<-1-sum(withinss)/adj_totss
    
    rtun<-list(cluster=res,type='supervised',parameters=param,evaluation=list(density=density,adj_totss=adj_totss,withinss=withinss,adj.RS=adj.RS))
  }
  class(rtun)<-'pcluster'
  rtun
}

#'@title Summary clustering result
#'
#'@export
summary.pcluster<-function(res){
  if(res$type=='unsupervised'){
    cat('A unsupervised cluster is done\n\n')
    cat('Number of obervations: ',length(res$cluster),'\n')
    cat('Number of obervations in core: ',length(res$cluster[res$cluster>0]),'\n')
    cat('Distance cutoff: ',res$param$dc$dc,'\n')
    cat('Evaluation of cluster result:\n\n')
    cat('totss                 withinss              RSquare\n')
    cat(formatC(res$evaluation$totss, width=-22), formatC(sum(res$evaluation$withinss), width=-22), res$evaluation$RS,'\n')
    cat('adj.totss                                   adj.Rsquare\n')
    cat(formatC(res$evaluation$adj_totss,width=-44),formatC(res$evaluation$adj.RS),'\n')
  }else{
    cat('A supervised cluster is done\n\n')
    cat('Number of obervations: ',length(res$cluster),'\n')
    cat('Number of obervations in core: ',length(res$cluster[res$cluster>0]),'\n')
    cat('Distance cutoff: ',res$param$dc$dc,'\n')
    cat('Density of each cluster:\n')
    cat(colnames(res$evaluation$density),'\n')
    cat(res$evaluation$density,'\n')
    cat('Evaluation of cluster result:\n\n')
    cat('adj.totss                 withinss              adj.RSquare\n')
    cat(formatC(res$evaluation$adj_totss, width=-22), formatC(sum(res$evaluation$withinss), width=-22), res$evaluation$adj.RS,'\n') 
  }
}
