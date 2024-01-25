library(lmerTest)
library(ggplot2)
library(rcompanion)
library(multcomp)
library(car)

###Energy&Nutrient distribution====


dat <- read.csv('distribution.csv', header = T)
dat$new_eff=log(dat$eff/(1-dat$eff))
model=lmer(new_eff~element2-1+(1|new.id), data = dat)

ss=summary(model)
ss

summary(glht(model, linfct=cbind(contrMat(c("Energy"=1,"Nutrient"=1), type="Tukey"))))

m=1/(exp(-ss[["coefficients"]][,1])+1)
t=(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2]
n=table(dat$element2)
P=round(2*pt(-abs(t), df=n-1),6)

###mixed model====
######energy=====
t=c()
con_par=matrix()
beta=c()
n=c()
study_n=c()
dat_energy=dat[which(dat$element2=="Energy"), ]
length(which(dat_energy$eff<0.05))/length(dat_energy$eff)
quantile(dat_energy$eff,0.95)
model_energy=lmer(new_eff~Definition-1+(1|new.id), data = dat_energy)
s=summary(model_energy)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model_energy,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat2=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat2$contrast=gsub(":"," ",dat2$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat2,
               threshold = 0.05 ) 


ano=cld[match(c,cld$Group),2]

t=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
con=confint(model_energy, method="Wald")
con_par=con[-(1:2),]
beta=model_energy@beta

n=table(dat_energy$Definition)
study_n=colSums(table(dat_energy$new.id,dat_energy$Definition)!=0)
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)
write.csv(Anova(model_energy, type = "II",test.statistic="F"),"anova_energy.csv") 

##all==
all_energy=lmer(new_eff~(1|new.id), data = dat_energy)
s=summary(all_energy)
t=c(t,(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2])
con=confint(all_energy, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,all_energy@beta)
n=c(n,nrow(dat_energy))
study_n=c(study_n,length(unique(dat_energy$new.id)))
ano=c(ano,NA)

N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Variable=c(rep("Definition",3),"All")
Cat=c("Ingestion","Assimilation","Production","All")
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

write.csv(ME,"definition_energy.csv",row.names = FALSE)



######nutrient=====
t=c()
con_par=matrix()
beta=c()
n=c()
study_n=c()
dat_nutrient=dat[which(dat$element2=="Nutrient"), ]
dat_nutrient$Definition <- factor(dat_nutrient$Definition,levels = c("Ingestion","Assimilation","Production"))
quantile(dat_nutrient$eff,0.05)

model_nutrient=lmer(new_eff~Definition-1+(1|new.id), data = dat_nutrient)
s=summary(model_nutrient)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model_nutrient,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat2=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat2$contrast=gsub(":"," ",dat2$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat2,
               threshold = 0.05 ) 


ano=cld[match(c,cld$Group),2]
ano=c(ano,NA)
t=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
con=confint(model_nutrient, method="Wald")
con_par=con[-(1:2),]
beta=model_nutrient@beta

n=table(dat_nutrient$Definition)
study_n=colSums(table(dat_nutrient$new.id,dat_nutrient$Definition)!=0)
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)
write.csv(Anova(model_nutrient, type = "II",test.statistic="F"),"anova_nutrient.csv") 

##all==
all_nutrient=lmer(new_eff~(1|new.id), data = dat_nutrient)
s=summary(all_nutrient)
t=c(t,(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2])
con=confint(all_nutrient, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,all_nutrient@beta)
n=c(n,nrow(dat_nutrient))
study_n=c(study_n,length(unique(dat_nutrient$new.id)))


N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Variable=c(rep("Definition",3),"All")
Cat=c("Ingestion","Assimilation","Production","All")
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

write.csv(ME,"definition_nutrient.csv",row.names = FALSE)

##nutrient mixed model====
t=c()
con_par=matrix()
beta=c()
n=c()
study_n=c()
dat_nutrient$element3=dat_nutrient$element
dat_nutrient$element3[dat_nutrient$element3!="N"&dat_nutrient$element3!="P"]="Others"
dat_nutrient$element3 <- factor(dat_nutrient$element3,levels = c("N","P","Others"))

model_nutrient=lmer(new_eff~element3-1+(1|new.id), data = dat_nutrient)
s=summary(model_nutrient)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model_nutrient,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat2=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat2$contrast=gsub(":"," ",dat2$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat2,
               threshold = 0.05 ) 

ano=cld[match(c,cld$Group),2]

t=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
con=confint(model_nutrient, method="Wald")
con_par=con[-(1:2),]
beta=model_nutrient@beta

n=table(dat_nutrient$element3)
study_n=colSums(table(dat_nutrient$new.id,dat_nutrient$element3)!=0)
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)
write.csv(Anova(model_nutrient, type = "II",test.statistic="F"),"anova_nutrient_element.csv") 


N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Variable=rep("Element",3)
Cat=c("N","P","Others")
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

write.csv(ME,"element_nutrient.csv",row.names = FALSE)


