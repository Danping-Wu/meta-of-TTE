library(lmerTest)
library(car)
library(multcomp)
library(rcompanion)
library(stringr)

newdata=read.csv('mixed model.csv')
newdata$new_eff=log(newdata$eff/(1-newdata$eff))
####all model====
model=lmer(new_eff~Ecosystem.origin+Ecosystem.type2+resource+consumer+TL+(1|new.id), data = newdata)
summary(model)

write.csv(Anova(model, type = "II",test.statistic="F"),"all model.csv") 




####ecosystem====
t=beta=n=study_n=ano=c()
con_par=matrix(ncol=2)
tuk=data.frame()

#Ecosystem origin
model=lmer(new_eff~Ecosystem.origin-1+(1|new.id), data = newdata)
ss=summary(model)
ss
c=rownames(ss[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,
               contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=c(ano,cld[match(c,cld$Group),2])
t=c(t,(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2])
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)
n=c(n,table(newdata$Ecosystem.origin))
study_n=c(study_n,colSums(table(newdata$new.id,newdata$Ecosystem.origin)!=0))
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)

#Ecosystem type
model=lmer(new_eff~Ecosystem.type2-1+(1|new.id), data = newdata)
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
n=c(n,table(newdata$Ecosystem.type2))
study_n=c(study_n,colSums(table(newdata$new.id,newdata$Ecosystem.type2)!=0))
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues))

N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Variable=c(rep("Ecosystem origin",2),rep("Ecosystem type",3))
Cat=c("Artificial ecosystem","Natural ecosystem","Freshwater ecosystem","Marine ecosystem","Terrestrial ecosystem")
con_par=con_par[-1,]
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

colnames(tuk)=c("Estimate","z value","P value")
write.csv(ME,"Ecosystem.csv",row.names = FALSE)
write.csv(tuk,"tukey_eco.csv",row.names = TRUE)






####species====
t=beta=n=study_n=ano=c()
con_par=data.frame()
tuk=data.frame()

#resource
model=lmer(new_eff~resource-1+(1|new.id), data = newdata)
ss=summary(model)
ss
c=rownames(ss[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 
c=gsub(" ","",c)



ano=c(ano,cld[match(c,cld$Group),2])

t=c(t,(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2])
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)

n=c(n,table(newdata$resource))
study_n=c(study_n,colSums(table(newdata$new.id,newdata$resource)!=0))
tuk=data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)

#consumer
newdata2=newdata[-which(newdata$consumer=="aggregated"),]

model=lmer(new_eff~consumer-1+(1|new.id), data = newdata2)
ss=summary(model)
ss
c=rownames(ss[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]

cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 

c=gsub(" ","",c)

ano=c(ano,cld[match(c,cld$Group),2])
t=c(t,(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2])
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)

n=c(n,table(newdata2$consumer))
study_n=c(study_n,colSums(table(newdata2$new.id,newdata2$consumer)!=0))
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)) 

#TL
model=lmer(new_eff~TL-1+(1|new.id), data = newdata)
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

n=c(n,table(newdata$TL))
study_n=c(study_n,colSums(table(newdata$new.id,newdata$TL)!=0))
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues)) 

N=paste0("(",study_n,"/",n,")")

t=matrix(t,ncol=1)
P=round(2*pt(-abs(t), df=n-1),6)
sig=rep("",nrow(t))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."
Variable=c(rep("Resource attribute",5),rep("Consumer attribute",3),rep("Trophic level",4))
Cat=c("Animal","Detritus","Plant","Plant+detritus","Plant+animal+detritus","Invertebrate","Vertebrate ectotherm","Vertebrate endotherm","I-II","II-III","III-IV","IV-")
ME=data.frame(Variable,Cat,N,beta,sig,con_par,ano,t,P)
colnames(ME)=c("Variable","Cat","N","b","sig","lb","ub","ano","t","P")

colnames(tuk)=c("Estimate","z value","P value")
Cat=c("Plant","Animal","Detritus","Plant+detritus","Plant+animal+detritus","Vertebrate endotherm","Vertebrate ectotherm","Invertebrate","I-II","II-III","III-IV","IV-")
ME=ME[match(Cat,ME$Cat),]

write.csv(tuk,"tukey_species.csv",row.names = TRUE)

write.csv(ME,"species.csv",row.names = FALSE)










####interaction====
##anova====
##ecosystem origin*resource
model=lmer((new_eff)~Ecosystem.origin*resource-1+(1|new.id), data = newdata)
summary(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "origin_resource.csv") 

##ecosystem origin*consumer
model=lmer((new_eff)~Ecosystem.origin*consumer-1+(1|new.id), data = newdata)
summary(model)
Anova(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "origin_consumer.csv")

##ecosystem origin*TL
model=lmer((new_eff)~Ecosystem.origin*TL-1+(1|new.id), data = newdata)
summary(model)
Anova(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "origin_TL.csv")



##ecosystem type*resource
model=lmer((new_eff)~Ecosystem.type2*resource-1+(1|new.id), data = newdata)
summary(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "type_resource.csv") 

##ecosystem type*consumer
model=lmer((new_eff)~Ecosystem.type2*consumer-1+(1|new.id), data = newdata)
summary(model)
Anova(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "type_consumer.csv")
##ecosystem type*TL
model=lmer((new_eff)~Ecosystem.type2*TL-1+(1|new.id), data = newdata)
summary(model)
Anova(model)
write.csv(Anova(model, type = "II",test.statistic="F"),
          "type_TL.csv")


#model=====
t=beta=n=study_n=ano=inter=cc=cg=c()
con_par=data.frame()
tuk=data.frame()
newdata$TL=sub("-","_",newdata$TL)
##type*resource====
newdata$type_resource=paste(newdata$Ecosystem.type2,newdata$resource)
newdata2=newdata[newdata$type_resource%in%names(table(newdata$type_resource))[table(newdata$type_resource)>1],]

model=lmer((new_eff)~Ecosystem.type2:resource-1+(1|new.id), data = newdata2)
s=summary(model)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=c(ano,cld$Letter[nrow(cld):1])
tt=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
t=c(t,tt)
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)
n=c(n,table(newdata2$type_resource))
study_n=c(study_n,colSums(table(newdata2$new.id,newdata2$type_resource)!=0))
inter=c(inter,c)
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues))

cc=c(cc,sub(":","",c))
cg=c(cg,cld$Group[nrow(cld):1])




##type*consumer====
newdata2=newdata[is.na(newdata$consumer)=="FALSE"&newdata$consumer!="aggregated",]
newdata2$type_consumer=paste(newdata2$Ecosystem.type2,newdata2$consumer)
newdata2=newdata2[newdata2$type_consumer%in%names(table(newdata2$type_consumer))[table(newdata2$type_consumer)>1],]


model=lmer((new_eff)~Ecosystem.type2:consumer-1+(1|new.id), data = newdata2)
s=summary(model)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=c(ano,cld$Letter[nrow(cld):1])
tt=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
t=c(t,tt)
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)
n=c(n,table(newdata2$type_consumer))
study_n=c(study_n,colSums(table(newdata2$new.id,newdata2$type_consumer)!=0))
inter=c(inter,c)
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues))

cc=c(cc,sub(":","",c))
cg=c(cg,cld$Group[nrow(cld):1])

##type*TL====
newdata2=newdata[is.na(newdata$TL)=="FALSE",]
newdata2$type_TL=paste(newdata2$Ecosystem.type2,newdata2$TL)
newdata2=newdata2[newdata2$type_TL%in%names(table(newdata2$type_TL))[table(newdata2$type_TL)>1],]

model=lmer((new_eff)~Ecosystem.type2:TL-1+(1|new.id), data = newdata2)
s=summary(model)
c=rownames(s[["coefficients"]])
gl_re=summary(glht(model,linfct=cbind(contrMat(sapply(c,function (x) x=1), type="Tukey"))))
dat=data.frame(pvalues=gl_re[["test"]]$pvalues,contrast=names(gl_re[["test"]]$coefficients))[order(names(gl_re[["test"]]$coefficients),decreasing = TRUE),]
dat$contrast=gsub(":"," ",dat$contrast)
cld <- cldList(pvalues~ contrast,
               data =dat,
               threshold = 0.05 ) 



ano=c(ano,cld$Letter[nrow(cld):1])
tt=(s[["coefficients"]][,1]-log(1/9))/s[["coefficients"]][,2]
t=c(t,tt)
con=confint(model, method="Wald")
con_par=rbind(con_par,con[-(1:2),])
beta=c(beta,model@beta)
n=c(n,table(newdata2$type_TL))
study_n=c(study_n,colSums(table(newdata2$new.id,newdata2$type_TL)!=0))
inter=c(inter,c)
tuk=rbind(tuk,data.frame(gl_re[["test"]]$coefficients,gl_re[["test"]]$tstat,gl_re[["test"]]$pvalues))

cc=c(cc,sub(":","",c))
cg=c(cg,cld$Group[nrow(cld):1])

####====
N=paste0("(",study_n,"/",n,")")
t=matrix(t,ncol=1)

P=round(2*pt(-abs(t), df=n-1),6)


sig=rep("",length(beta))
sig[which(P<0.001&P>=0)]="***"
sig[which(P<0.01&P>=0.001)]="**"
sig[which(P<0.05&P>=0.01)]="*"
sig[which(P<0.1&P>=0.05)]="."


ME=data.frame(inter,N,beta,sig,con_par,ano,t,P)
ME1=data.frame(cc,beta,sig,con_par,t,P,order=1:length(cc))
ME1$cc=gsub(" ","",ME1$cc)
ME2=data.frame(cg,N,ano)
colnames(ME2)[1]="cc"
ME=merge(ME1,ME2)
ME=ME[order(ME$order),]
colnames(ME)=c("inter","b","sig","lb","ub","t","P","order","N","ano")

ME$inter=gsub("Ecosystem.type2","",ME$inter)
ME$inter=gsub("consumer"," ",ME$inter)
ME$inter=gsub("resource"," ",ME$inter)
ME$inter=gsub("TL"," ",ME$inter)
ME$inter=gsub("_","-",ME$inter)
ME$inter=gsub("consumer"," ",ME$inter)
Type=unlist(str_split(ME$inter, fixed(" ")))[seq(1,52,2)]
Cat=unlist(str_split(ME$inter, fixed(" ")))[seq(2,52,2)]
ME=cbind(ME,Type=Type,Cat=Cat)
write.csv(tuk,"tukey_type_species.csv",row.names = TRUE)
write.csv(ME,"type_species.csv",row.names = FALSE)


