reportReg=function(fit){

  type=as.character(fit$call)

  if(type[1]=="polr"){
    fit.sum=summary(fit)
    ctable<-coef(fit.sum)
    rname=row.names(ctable)
    p<-pt(abs(ctable[,"t value"]),df=fit$n,lower.tail=FALSE)*2
    ctable<-cbind(ctable,"P"=p)
    ctable=data.frame(ctable)
    ng=length(fit$lev)
    rname=rname[1:(nrow(ctable)-(ng-1))]
    ctable<-ctable[1:(nrow(ctable)-(ng-1)),]
    ctable<-cbind(ctable,ci=confint.default(fit))
    ctable[,c(1,5,6)]=exp(ctable[,c(1,5,6)])
    ctable=round(ctable,3)
    rst<-data.frame(OR=ctable[,1],"CI"=paste("(",ctable[,5],",",ctable[,6],")"),P=ctable[,4])
    row.names(rst)=rname
    return(rst)
  }

  if(type[1]=="glm"){
    fit.sum=summary(fit)
    rst=fit.sum$coef
    rst[,1]=exp(rst[,1])
    rst=round(rst,3)
    ci=confint.default(fit)
    ci=round(exp(ci),3)
    ci=paste("(",ci[,1],",",ci[,2],")")
    rst=cbind(rst,ci)
    rst=rst[,c(1,5,4)]
    rst=data.frame(rst)
    rst=rst[-1,]
    names(rst)=c("OR","CI","P")
  }

  if(type[1]=="lm"){

    fit.sum=summary(fit)
    rst=fit.sum$coef
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

  if(type[1]=="coxph"){
    fit.sum=summary(fit)
    ctable=fit.sum$conf.int
    ctable=data.frame(ctable)
    rname=row.names(ctable)
    p=coef(fit.sum)
    p=data.frame(p)
    ctable<-cbind(ctable[,-2],p[,5])
    ctable=round(ctable,3)
    rst<-data.frame(OR=ctable[,1],"CI"=paste("(",ctable[,2],",",ctable[,3],")"),P=ctable[,4])
    row.names(rst)=rname
  }

  return(rst)
}
