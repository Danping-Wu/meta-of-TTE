library(lmerTest)
library(car)
library(multcomp)
library(rcompanion)
##analyse=======
ecopath.data=read.csv("ecopath.data.csv")
ecopath.data=ecopath.data[ecopath.data$TTE<=1&ecopath.data$TTE>0,]
ecopath.data$new_TTE=log(ecopath.data$TTE/(1-ecopath.data$TTE))

###TL====
model=lmer(new_TTE~TL-1+(1|FW), data = ecopath.data)
ss=summary(model)
ss
c=rownames(ss[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=cld[match(c,cld$Group),2]

t=(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2]
con=confint(model, method="Wald")
con_par=con[-(1:2),]
beta=model@beta

n=table(ecopath.data$TL)
study_n=colSums(table(ecopath.data$FW,ecopath.data$TL)!=0)
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)

##Ecosystem=====
model=lmer(new_TTE~Ecosystem-1+(1|FW), data = ecopath.data)
ss=summary(model)
ss
c=rownames(ss[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=c(ano,cld[match(c,cld$Group),2])

t=c(t,(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2])
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)

n=c(n,table(ecopath.data$Ecosystem))
study_n=c(study_n,colSums(table(ecopath.data$FW,ecopath.data$Ecosystem)!=0))
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues))


N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Cat=c("I-II","II-III","III-IV","IV-V","Freshwater","Marine")
Variable=c(rep("TL",4),rep("Ecosystem type",2))
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

colnames(tuk)=c("Estimate","z value","P value")
write.csv(ME,"ecopath_model.csv",row.names = FALSE)
write.csv(tuk,"tuk_ecopath.csv")

##Meta====
newdata=read.csv('mixed model.csv')
newdata$new_eff=log(newdata$eff/(1-newdata$eff))
newdata=newdata[,c(1,6,9,2,13)]
newdata$database="meta"
ecopath.data$FW=paste0("Ecopath",ecopath.data$FW)
ecopath.data$Ecosystem=gsub("marine","Marine",ecopath.data$Ecosystem)
ecopath.data$Ecosystem=gsub("freshwater","Freshwater",ecopath.data$Ecosystem)
ecopath.data$Ecosystem=gsub("terrestrial","Terrestrial",ecopath.data$Ecosystem)
ecopath.data$TL=gsub("1_2","I_II",ecopath.data$TL)
ecopath.data$TL=gsub("2_3","II_III",ecopath.data$TL)
ecopath.data$TL=gsub("3_4","III_IV",ecopath.data$TL)
ecopath.data$TL=gsub("4_5","IV_V",ecopath.data$TL)
ecopath.data$database="ecopath"
colnames(newdata)=colnames(ecopath.data)
data=rbind(ecopath.data,newdata)
model=lmer(new_TTE~Ecosystem+TL+database+(1|FW), data = data)
summary(model)
write.csv(Anova(model, type = "II",test.statistic="F"),"compare.csv") 
