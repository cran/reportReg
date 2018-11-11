reportReg=function(fit,expTr=TRUE){

  type=as.character(fit$call)

  rst=NULL

  if(type[1]=="polr"){
    fit.sum=summary(fit)
    ctable<-coef(fit.sum)
    ng=length(fit$lev)
    if(nrow(ctable)==(ng-1)){
      stop("Only intercept was included in the model!")
    }else{
      if(expTr==TRUE){
        rname=row.names(ctable)
        p<-pt(abs(ctable[,"t value"]),df=fit$n,lower.tail=FALSE)*2
        ctable<-cbind(ctable,"P"=p)
        ctable=data.frame(ctable)
        rname=rname[1:(nrow(ctable)-(ng-1))]
        ctable<-ctable[1:(nrow(ctable)-(ng-1)),]
        ctable<-cbind(ctable,ci=confint.default(fit))
        ctable[,c(1,5,6)]=exp(ctable[,c(1,5,6)])
        ctable=round(ctable,3)
        rst<-data.frame(OR=ctable[,1],CI=paste("(",ctable[,5],",",ctable[,6],")"),P=ctable[,4])
        row.names(rst)=rname
      }else{
        rname=row.names(ctable)
        p<-pt(abs(ctable[,"t value"]),df=fit$n,lower.tail=FALSE)*2
        ctable<-cbind(ctable,"P"=p)
        ctable=data.frame(ctable)
        rname=rname[1:(nrow(ctable)-(ng-1))]
        ctable<-ctable[1:(nrow(ctable)-(ng-1)),]
        ctable<-cbind(ctable,ci=confint.default(fit))
        ctable=round(ctable,3)
        rst<-data.frame(Beta=ctable[,1],CI=paste("(",ctable[,5],",",ctable[,6],")"),P=ctable[,4])
        row.names(rst)=rname
      }
    }
  }

  if(type[1]=="glm"){
    fit.sum=summary(fit)
    rst=fit.sum$coef
    if(nrow(rst)==1){
      stop("Only intercept was included in the model!")
    }else{
      if(expTr==TRUE){
        ci=confint.default(fit)
        rst[,1]=exp(rst[,1])
        ci=round(exp(ci),3)
        rst=round(rst,3)
        ci=paste("(",ci[,1],",",ci[,2],")")
        rst=cbind(rst,ci)
        rst=rst[,c(1,5,4)]
        rst=data.frame(rst)
        rst=rst[-1,]
        names(rst)=c("OR","CI","P")
      }else{
        ci=confint.default(fit)
        ci=round(ci,3)
        rst=round(rst,3)
        ci=paste("(",ci[,1],",",ci[,2],")")
        rst=cbind(rst,ci)
        rst=rst[,c(1,5,4)]
        rst=data.frame(rst)
        rst=rst[-1,]
        names(rst)=c("Beta","CI","P")
        }
      }
  }

  if(type[1]=="lm"){
    fit.sum=summary(fit)
    rst=fit.sum$coef
    if(nrow(rst)==1){
      message("Only intercept was included in the model!")
      rst=data.frame()
    }else{
      rst=round(rst,3)
      ci=confint.default(fit)
      ci=round(ci,3)
      ci=paste("(",ci[,1],",",ci[,2],")")
      rst=cbind(rst,ci)
      rst=rst[,c(1,5,4)]
      rst=data.frame(rst)
      rst=rst[-1,]
      names(rst)=c("Beta","CI","P")
    }
  }

  if(type[1]=="coxph"){
    fit.sum=summary(fit)
    if(expTr==TRUE){
      ctable=fit.sum$conf.int
      ctable=data.frame(ctable)
      rname=row.names(ctable)
      p=coef(fit.sum)
      p=data.frame(p)
      ctable<-cbind(ctable[,-2],p[,ncol(p)])
      ctable=round(ctable,3)
      rst<-data.frame(HR=ctable[,1],CI=paste("(",ctable[,2],",",ctable[,3],")"),P=ctable[,4])
      row.names(rst)=rname
    }else{
      ctable=fit.sum$coefficients
      ctable=data.frame(ctable)
      p_ncol=ncol(ctable)
      ctable$cilow=ctable[,1]-1.96*ctable[,3]
      ctable$cihigh=ctable[,1]+1.96*ctable[,3]
      ctable=round(ctable,3)
      rst<-data.frame(Beta=ctable[,"coef"],CI=paste("(",ctable[,"cilow"],",",ctable[,"cihigh"],")"),P=ctable[,p_ncol])
      row.names(rst)=rname
    }
  }

  if(type[1]=="FGR"){
    fit.sum=summary(fit)
    ctable<-data.frame(fit.sum$coef,fit.sum$conf.int)
    ctable$cilow=ctable[,1]-1.96*ctable[,3]
    ctable$cihigh=ctable[,1]+1.96*ctable[,3]
    rname=row.names(ctable)
    ctable=round(ctable,3)
    if(expTr==TRUE){
      rst<-data.frame(HR=ctable[,2],CI=paste("(",ctable[,8],",",ctable[,9],")"),P=ctable[,5])
    }else{
      rst<-data.frame(Beta=ctable[,1],CI=paste("(",ctable[,10],",",ctable[,11],")"),P=ctable[,5])
    }
    row.names(rst)=rname
  }

  if(type[1]=="lme.formula"){
    fits=summary(fit)
    fitci=intervals(fit,which="fixed")
    rst=cbind(fits$tTable,data.frame(fitci$fixed))
    rst=round(rst,3)
    rst$ci=paste("(",rst[,6],",",rst[,8],")")
    rst=rst[-1,c(7,9,5)]
    names(rst)=c("Beta","CI","P")
  }

  if(is.null(rst)){stop(paste(type[1],"is not available by now!"))}

  return(rst)
}
