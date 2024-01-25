library(reshape2)
library(metafor)
library(lmerTest)
library(multcomp)
library(agricolae)
library(ggplot2)
library(ggsci)
library(cowplot)
##environmental factors====
C.TTE.data <- read.csv('C.TTE.data.all.csv', header = T)
C.TTE.data$trans[which(C.TTE.data$new.id==135&C.TTE.data$n==1)]=NA
C.TTE.data <- C.TTE.data[which(is.na(as.numeric(as.vector(C.TTE.data$trans)))==FALSE ), ]
C.TTE.data[which(as.numeric(C.TTE.data$trans) < 0), ]$trans=NA
C.TTE.data$SD[which(C.TTE.data$SD.SE.median.interval=="SE")] =C.TTE.data$SD[which(C.TTE.data$SD.SE.median.interval=="SE")]*sqrt(C.TTE.data$n[which(C.TTE.data$SD.SE.median.interval=="SE")])
##nutrient====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Nutrient) %in% c("", NA, "Y"))) , ]


write.csv(newdata, 'Nutrient_data.csv')

nut_m=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Nutrient),mean)
colnames(nut_m)=c("new.id","Scenario","Nutrient","Mean")

nut_n=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Nutrient),length)
colnames(nut_n)=c("new.id","Scenario","Nutrient","N")
nut_v=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Nutrient),var)
colnames(nut_v)=c("new.id","Scenario","Nutrient","var")

nut=merge(nut_m,nut_n)
nut=merge(nut,nut_v)
nut$Scenario=as.numeric(nut$Scenario)
nut=nut[order(nut$new.id,nut$Scenario,nut$Nutrient),]

nut$N[which(nut$new.id%in%unique(newdata$new.id[which(newdata$n!=1)])==TRUE)]=
  newdata$n[which(newdata$n!=1)] 
nut$var[which(nut$new.id%in%unique(newdata$new.id[which(newdata$n!=1)])==TRUE)]=
  newdata$SD[which(newdata$n!=1)]^2 

nut$vn=nut$var/(nut$N*nut$Mean^2)
D=c()
study=c()
v=c()
nutrient=c()

m_1185=nut[which(nut$new.id==11|nut$new.id==85),]
mm=matrix(m_1185$Mean,nrow=2)
D[1:ncol(mm)]=mm[1,]/mm[2,]
study[1:ncol(mm)]=c(rep(11,6),rep(85,2))
vv=matrix(m_1185$vn,nrow=2)
v[1:ncol(mm)]=vv[1,]+vv[2,]
study[1:ncol(mm)]=c(rep(11,6),rep(85,2))
nutrient[1:ncol(mm)]="H_L"

m_87=nut[which(nut$new.id==87),]
mm=m_87$Mean[2:nrow(m_87)]/m_87$Mean[1:(nrow(m_87)-1)]
D[9:14]=mm[-4]
vv=m_87$vn[2:nrow(m_87)]+m_87$vn[1:(nrow(m_87)-1)]
v[9:14]=vv[-4]

mmm=cbind(c(0.297,0.107,0.093,0.07),c(0.172,0.114,0.114,0.052))
D[9:14]=c(mmm[2:4,]/mmm[1:3,])
SE=cbind(c(0.044,0.018,0.022,0.031),c(0.054,0.039,0.015,0))
n=cbind(c(3,5,12,6),c(8,8,11,1))
SD=SE*sqrt(n)
vv=SD^2/(n*mmm)
vv=c(vv[2:4,]+vv[1:3,])
study[9:14]=rep(87,6)
nutrient[9:14]=rep(c("M_L","M_L","H_M"),2)
v[9:14]=vv

m_110=nut[which(nut$new.id==110),]
m_110$Scenario=c(5,4,2,1,3)
m_110=m_110[order(m_110$Scenario),]
mm=m_110$Mean[2:nrow(m_110)]/m_110$Mean[1:(nrow(m_110)-1)]
D[15:18]=mm
vv=m_110$vn[2:nrow(m_110)]+m_110$vn[1:(nrow(m_110)-1)]
v[15:18]=vv
study[15:18]=rep(110,4)
nutrient[15:18]=c("M_L","M_L","H_M","H_M")

m_112=nut[which(nut$new.id==112),]
mm=m_112$Mean[3:4]/m_112$Mean[1:2]
D[19:20]=mm
vv=m_112$vn[3:4]+m_112$vn[1:2]
v[19:20]=0
study[19:20]=112
nutrient[19:20]="H_L"

m_116=nut[which(nut$new.id==116),]
mm=matrix(m_116$Mean,nrow=4)
mm_L=mm[,c(2,4)]/mm[,c(1,3)]
D[21:28]=c(mm_L)
vv=matrix(m_116$vn,nrow=4)
vv_L=vv[,c(2,4)]/vv[,c(1,3)]
v[21:28]=c(vv_L)
study[21:28]=rep(116,8)
nutrient[21:28]=rep("H_L",8)

m_117127=nut[which(nut$new.id==117|nut$new.id==127),]
mm=matrix(m_117127$Mean,nrow=2)
D[29:43]=mm[1,]/mm[2,]
vv=matrix(m_117127$vn,nrow=2)
v[29:43]=vv[1,]/vv[2,]
study[29:43]=c(rep(117,9),rep(127,6))
nutrient[29:43]=rep("H_L",15)

m_135=nut[which(nut$new.id==135),]
mm=m_135$Mean[1:2]/m_135$Mean[2:3]
D[44:45]=mm
vv=m_135$vn[1:2]/m_135$vn[2:3]
v[44:45]=vv
study[44:45]=rep(135,2)
nutrient[44:45]=c("H_M","M_L")


m_140=nut[which(nut$new.id==140),]
m_140$Scenario=m_140$Scenario+c(1,1,1,-3)
m_140=m_140[order(m_140$Scenario),]
mm=matrix(m_140$Mean,nrow=4)
mm_L=mm[2:4,]/mm[1:3,]
D[46:57]=c(mm_L)
vv=matrix(m_140$vn,nrow=4)
vv_L=vv[2:4,]/vv[1:3,]
v[46:57]=c(vv_L)
study[46:57]=rep(140,12)
nutrient[46:57]=rep(c("M_L","M_L","H_M"),4)


data_nut=data.frame(study,D,log(D),v,nutrient)
data_nut222=data_nut[which(data_nut$study==127),]
data_nut=data_nut[-which(data_nut$study==127),]

colnames(data_nut)=c("study","D","LRR","v","nutrient")

write.csv(data_nut, 'Nutrient_data_eff.csv')

m.nutrient=rma.mv(LRR,v,mods=~nutrient-1,random=~1|study,data=data_nut)
summary(m.nutrient)
m.nutrient=rma.mv(LRR,v,random=~1|study,data=data_nut)
summary(m.nutrient)
##light====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Light) %in% c("", NA))) , ]
write.csv(newdata, 'Light_data.csv')

light_m=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Light),mean)
colnames(light_m)=c("new.id","Scenario","Light","Mean")

light_n=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Light),length)
colnames(light_n)=c("new.id","Scenario","Light","N")
light_v=aggregate(newdata$trans, by=list(newdata$new.id,newdata$Scenario,newdata$Light),var)
colnames(light_v)=c("new.id","Scenario","Light","var")

light=merge(light_m,light_n)
light=merge(light,light_v)
light$Scenario=as.numeric(light$Scenario)
light=light[order(light$new.id,light$Scenario,light$Light),]

light$N[which(light$new.id%in%unique(newdata$new.id[which(newdata$n!=1)])==TRUE)]=
  newdata$n[which(newdata$n!=1)] 
light$var[which(light$new.id%in%unique(newdata$new.id[which(newdata$n!=1)])==TRUE)]=
  newdata$SD[which(newdata$n!=1)]^2 

light$vn=light$var/(light$N*light$Mean^2)
D=c()
study=c()
v=c()

m_1185=light[which(light$new.id==11|light$new.id==85),]
mm=matrix(m_1185$Mean,nrow=2)
D[1:8]=c(mm[,c(1,3,5,7)]/mm[,c(2,4,6,8)])
study[1:8]=c(rep(11,6),rep(85,2))
vv=matrix(m_1185$vn,nrow=2)
v[1:8]=c(vv[,c(1,3,5,7)]+vv[,c(2,4,6,8)])

m_116=light[which(light$new.id==116),]
mm=matrix(m_116$Mean,nrow=2)
D[9:16]=c(mm[,c(2,4,6,8)]/mm[,c(1,3,5,7)])
study[9:16]=rep(116,8)
vv=matrix(m_116$vn,nrow=2)
v[9:16]=c(vv[,c(1,3,5,7)]+vv[,c(2,4,6,8)])

m_117=light[which(light$new.id==117),]
mm=matrix(m_117$Mean,nrow=2)
ddd=mm[,1:8]/mm[,2:9]
D[17:28]=c(ddd[,-c(3,6)])
study[17:28]=rep(117,12)
vv=matrix(m_117$vn,nrow=2)
vvv=vv[,1:8]+vv[,2:9]
v[17:28]=c(vvv[,-c(3,6)])

data_light=data.frame(study,D,log(D),v)
colnames(data_light)=c("study","D","LRR","v")
write.csv(data_light, 'Light_data_eff.csv')


m.light=rma.mv(LRR,v,random=~1|study,data=data_light)
summary(m.light)






##DOC====
newdata <- C.TTE.data[which((as.vector(C.TTE.data$DOC) %in% c("N","Y"))) , ]
write.csv(newdata, 'DOC_data.csv')

D=c()
study=c()
v=c()

D=c(newdata$trans[1:4]/newdata$trans[5:8],newdata$trans[c(10,12,14)]/newdata$trans[c(9,11,13)])
newdata$vn=newdata$SD^2/(newdata$n*newdata$trans^2)
v=c(newdata$trans[1:4]+newdata$trans[5:8],newdata$trans[c(10,12,14)]+newdata$trans[c(9,11,13)])
study=c(rep(7,4),10,rep(108,2))


data_DOC=data.frame(study,D,log(D),v)
colnames(data_DOC)=c("study","D","LRR","v")

write.csv(data_DOC, 'DOC_data_eff.csv')

##predator====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Predator) %in% c(""))) , ]
write.csv(newdata, 'predator_data.csv')

D=c()
study=c()
v=c()
newdata2=newdata[which(newdata$new.id==11),]
predator_m=aggregate(newdata2$trans, by=list(newdata2$new.id,newdata2$Scenario,newdata2$Predator),mean)
colnames(predator_m)=c("new.id","Scenario","predator","Mean")

predator_n=aggregate(newdata2$trans, by=list(newdata2$new.id,newdata2$Scenario,newdata2$Predator),length)
colnames(predator_n)=c("new.id","Scenario","predator","N")
predator_v=aggregate(newdata2$trans, by=list(newdata2$new.id,newdata2$Scenario,newdata2$Predator),var)
colnames(predator_v)=c("new.id","Scenario","predator","var")

predator=merge(predator_m,predator_n)
predator=merge(predator,predator_v)
predator$Scenario=as.numeric(predator$Scenario)
predator=predator[order(predator$new.id,predator$Scenario,predator$predator),]

predator$vn=predator$var/(predator$N*predator$Mean^2)


mm=matrix(predator$Mean,nrow=4)
D[1:4]=c(mm[,2]/mm[,1])
study[1:4]=c(rep(11,4))
vv=matrix(predator$vn,nrow=4)
v[1:4]=c(vv[,2]+vv[,1])

predator=newdata[which(newdata$new.id==136|newdata$new.id==137),]
predator=predator[,c("new.id","Scenario","Predator","trans","n","SD")]
predator=predator[which(predator$Scenario!="Cb"),]
predator$vn=predator$SD^2/(predator$n*predator$trans^2)
mm=matrix(predator$trans[1:16],nrow=4)
D[5:12]=c(mm[,c(2,4)]/mm[,c(1,3)])
vv=matrix(predator$vn[1:16],nrow=4)
v[5:12]=c(vv[,c(2,4)]+vv[,c(1,3)])

mm=matrix(predator$trans[17:28],nrow=2)
D[13:18]=c(mm[2,]/mm[1,])
vv=matrix(predator$vn[17:28],nrow=2)
v[13:18]=c(vv[2,]+vv[1,])

study[5:18]=c(rep(136,14))

mm=matrix(predator$trans[29:44],nrow=4)
D[19:26]=c(mm[3:4,]/mm[1:2,])
vv=matrix(predator$vn[29:44],nrow=4)
v[19:26]=c(vv[3:4,]+vv[1:2,])
study[19:26]=c(rep(137,8))

data_predator=data.frame(study,D,log(D),v)
colnames(data_predator)=c("study","D","LRR","v")

write.csv(data_predator, 'predator_data_eff.csv')

##invasion====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Invasion) %in% c("after","pre"))) , ]
write.csv(newdata, 'invasion_data.csv')

D=c()
study=c()
v=c()

D=c(newdata$trans[5:8]/newdata$trans[1:4],newdata$trans[9:12]/newdata$trans[1:4])
study=rep(24,8)


data_Invasion=data.frame(study,D,log(D))
colnames(data_Invasion)=c("study","D","LRR")

write.csv(data_Invasion, 'Invasion_data_eff.csv')


##Environmental stress====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Environmental.stress) %in% c(""))) , ]
write.csv(newdata, 'stress_data.csv')
mm=matrix(newdata$trans,nrow=2)
D=c()
study=c()
v=c()

D=mm[2,]/mm[1,]
study=rep(138,nrow(newdata)/2)


data_stress=data.frame(study,D,log(D))
colnames(data_stress)=c("study","D","LRR")

write.csv(data_stress, 'stress_data_eff.csv')



##Horizontal====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Horizontal) %in% c(""))) , ]
write.csv(newdata, 'Horizontal_data.csv')
D=c()
study=c()
v=c()

D=c(newdata$trans[1:20]/newdata$trans[21:40],newdata$trans[41:52]/newdata$trans[53:64])
study=c(rep(77,20),rep(145,12))


data_Horizontal=data.frame(study,D,log(D))
colnames(data_Horizontal)=c("study","D","LRR")

write.csv(data_Horizontal, 'Horizontal_data_eff.csv')




##PP====
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$PP..primary.or.detritus.) %in% c("",NA))) , ]
write.csv(newdata, 'PP_data.csv')
D=c()
study=c()
v=c()
mm=matrix(newdata$trans[1:27],nrow=3)
D[1:18]=c(mm[1:2,]/mm[2:3,])
D[19:22]=c(newdata$trans[32:35]/newdata$trans[28:31])


study=c(rep(2,18),rep(129,4))


data_PP=data.frame(study,D,log(D))
colnames(data_PP)=c("study","D","LRR")
data_nut222=data_nut222[,1:3]
colnames(data_nut222)<-c("study","D","LRR")
data_PP=rbind(data_PP,data_nut222)
write.csv(data_PP, 'PP_data_eff.csv')





##factor model====
t=beta=n=study_n=ci.lb=ci.ub=P=c()


data_nut=read.csv("Nutrient_data_eff.csv")
data_nut=data_nut[data_nut$v>0,]
m.nutrient=rma.mv(LRR,v,random=~1|study,data=data_nut)
s=summary(m.nutrient)
P=c(P,s$pval)
beta=c(beta,s$beta)
ci.lb=c(ci.lb,s$ci.lb)
ci.ub=c(ci.ub,s$ci.ub)
n=c(n,nrow(data_nut))
study_n=c(study_n,length(unique(data_nut$study)))

data_light=read.csv("Light_data_eff.csv")
m.light=rma.mv(LRR,v,random=~1|study,data=data_light)
s=summary(m.light)
P=c(P,s$pval)
beta=c(beta,s$beta)
ci.lb=c(ci.lb,s$ci.lb)
ci.ub=c(ci.ub,s$ci.ub)
n=c(n,nrow(data_light))
study_n=c(study_n,length(unique(data_light$study)))





data_DOC=read.csv("DOC_data_eff.csv")
m.DOC=rma.mv(LRR,v,random=~1|study,data=data_DOC)
s=summary(m.DOC)
P=c(P,s$pval)
beta=c(beta,s$beta)
ci.lb=c(ci.lb,s$ci.lb)
ci.ub=c(ci.ub,s$ci.ub)
n=c(n,nrow(data_DOC))
study_n=c(study_n,length(unique(data_DOC$study)))


data_PP=read.csv("PP_data_eff.csv")
m.PP=rma.mv(LRR,1,random=~1|study, data = data_PP)
s=summary(m.PP)
P=c(P,s$pval)
beta=c(beta,s$beta)
ci.lb=c(ci.lb,s$ci.lb)
ci.ub=c(ci.ub,s$ci.ub)
n=c(n,nrow(data_PP))
study_n=c(study_n,length(unique(data_PP$study)))

data_predator=read.csv("Predator_data_eff.csv")
m.predator=rma.mv(LRR,v,random=~1|study,data=data_predator)
s=summary(m.predator)
P=c(P,s$pval)
beta=c(beta,s$beta)
ci.lb=c(ci.lb,s$ci.lb)
ci.ub=c(ci.ub,s$ci.ub)
n=c(n,nrow(data_predator))
study_n=c(study_n,length(unique(data_predator$study)))


N=paste0("(",study_n,"/",n,")")

Variable=c("Nutrient","Light","DOC","Primary production","Predator")
ME=data.frame(Variable,N,beta,ci.lb,ci.ub,P)
colnames(ME)=c("Variable","N","b","ci.lb","ci.ub","P")
write.csv(ME,"env_factor.csv",row.names = FALSE)
##figure.all====
ES<- read.csv('env_factor.csv', header = T)
lwd=.pt*72.27/96
colnames(ES)[1]="Variable"
ES$group=ES$Variable


ES$h=(8:1)*0.5
ES$pos=7:0

ES$lab=ES$N

vignette("ggsci")


ALL=
  ggplot(data=ES,aes(y =pos,x =b ), show.legend = FALSE)+
  scale_color_manual(values=c("#008B45FF","#E20613","#3A4696", "#008B45FF","#E20613","#E20613","#3A4696","#E20613" ))+
  
  geom_errorbar(aes(xmin=ci.lb, xmax=ci.ub,y=pos,color=factor(Variable),group=factor(Variable) ),width=0,cex=3,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Variable),group=factor(Variable) ),cex=3,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=Sig.loc,color=factor(Variable)),size=12/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=lab,color=factor(Variable)),size=11/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(y=pos,x=-5,label =Variable,color=factor(Variable)), vjust = 0.5,hjust=0.6,size=13/.pt,lineheight=1)+
  #scale_color_aaas()+scale_fill_aaas()+
  #geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=0,y=-0.5,xend=0,yend=7.5,linetype="longdash",col="gray60",size=1.3/lwd)+
  
  scale_x_continuous(limits = c(-7,4),breaks=seq(-4.5,3,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(-1,8),expand =expansion(add = c(0,0)))+
  xlab("LRR")+
  
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =element_text(size=18),
        axis.title = element_text(hjust=0.5,size=20),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 5, 5, -5, "pt"))

ALL
pdf("env_factor.pdf",width = 8,height = 6)

ALL
dev.off()



ES<- read.csv('env_factor_DOC.csv', header = T)
lwd=.pt*72.27/96
colnames(ES)[1]="Variable"
ES$group=ES$Variable


ES$h=(2:1)*0.5
ES$pos=1:0

ES$lab=ES$N

vignette("ggsci")


ALL=
  ggplot(data=ES,aes(y =pos,x =b ), show.legend = FALSE)+
  scale_color_manual(values=c("#E20613","#E20613" ))+
  
  geom_errorbar(aes(xmin=ci.lb, xmax=ci.ub,y=pos,color=factor(Variable),group=factor(Variable) ),width=0,cex=3,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Variable),group=factor(Variable) ),cex=3,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=Sig.loc,color=factor(Variable)),size=12/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=lab,color=factor(Variable)),size=11/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(y=pos,x=-3.5,label =Variable,color=factor(Variable)), vjust = 0.5,hjust=0.6,size=13/.pt,lineheight=1)+
  #scale_color_aaas()+scale_fill_aaas()+
  #geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=0,y=-0.5,xend=0,yend=7.5,linetype="longdash",col="gray60",size=1.3/lwd)+
  
  scale_x_continuous(limits = c(-4,2.5),breaks=seq(-2.5,1.5,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(-1,1.5),expand =expansion(add = c(0,0)))+
  xlab("LRR")+
  
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =element_text(size=11),
        axis.title = element_text(hjust=0.5,size=12),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 5, 5, -5, "pt"))

ALL
pdf("env_factor_DOC.pdf",width = 8,height = 2)

ALL
dev.off()

##figure====
ES<- read.csv('env_factor2.csv', header = T)
lwd=.pt*72.27/96
colnames(ES)[1]="Variable"
ES$group=ES$Variable


ES$h=(5:1)*0.5
ES$pos=4:0

ES$lab=ES$N

vignette("ggsci")


ALL=
  ggplot(data=ES,aes(y =pos,x =b ), show.legend = FALSE)+
  scale_color_manual(values=c("#E20613","#E20613","#E20613","#3A4696","#E20613" ))+
  
  geom_errorbar(aes(xmin=ci.lb, xmax=ci.ub,y=pos,color=factor(Variable),group=factor(Variable) ),width=0,cex=3,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Variable),group=factor(Variable) ),cex=3,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=Sig.loc,color=factor(Variable)),size=12/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=lab,color=factor(Variable)),size=11/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(y=pos,x=-5,label =Variable,color=factor(Variable)), vjust = 0.5,hjust=0.6,size=13/.pt,lineheight=1)+
  #scale_color_aaas()+scale_fill_aaas()+
  #geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=0,y=-0.5,xend=0,yend=7.5,linetype="longdash",col="gray60",size=1.3/lwd)+
  
  scale_x_continuous(limits = c(-7,4),breaks=seq(-4.5,3,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(-1,5),expand =expansion(add = c(0,0)))+
  xlab("LRR")+
  
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =element_text(size=11),
        axis.title = element_text(hjust=0.5,size=12),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 5, 5, -5, "pt"))

ALL
pdf("env_factor2.pdf",width = 8,height = 4)

ALL
dev.off()



ES<- read.csv('env_factor_DOC.csv', header = T)
lwd=.pt*72.27/96
colnames(ES)[1]="Variable"
ES$group=ES$Variable


ES$h=(2:1)*0.5
ES$pos=1:0

ES$lab=ES$N

vignette("ggsci")


ALL=
  ggplot(data=ES,aes(y =pos,x =b ), show.legend = FALSE)+
  scale_color_manual(values=c("#E20613","#E20613" ))+
  
  geom_errorbar(aes(xmin=ci.lb, xmax=ci.ub,y=pos,color=factor(Variable),group=factor(Variable) ),width=0,cex=3,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Variable),group=factor(Variable) ),cex=3,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=Sig.loc,color=factor(Variable)),size=12/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ci.ub+0.25,y=pos ,label=lab,color=factor(Variable)),size=11/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(y=pos,x=-3.5,label =Variable,color=factor(Variable)), vjust = 0.5,hjust=0.6,size=13/.pt,lineheight=1)+
  #scale_color_aaas()+scale_fill_aaas()+
  #geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=0,y=-0.5,xend=0,yend=7.5,linetype="longdash",col="gray60",size=1.3/lwd)+
  
  scale_x_continuous(limits = c(-4,2.5),breaks=seq(-2.5,1.5,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(-1,1.5),expand =expansion(add = c(0,0)))+
  xlab("LRR")+
  
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =element_text(size=11),
        axis.title = element_text(hjust=0.5,size=12),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 5, 5, -5, "pt"))

ALL
pdf("env_factor_DOC.pdf",width = 8,height = 1.5)

ALL
dev.off()

##latitude====


TTE.data <- read.csv('all.TTE.data.2.csv', header = T)
dat <- TTE.data[ , c(1,7,12:15,23:25,29:31,35:48,50,5,6,70)]
location.infor <- read.csv('latitude and lontitude/lat-lon translate_all.TTE.csv', header = T)

eco.record <- read.csv('ecosystem record_new.csv', header = T)
eco.1.TTE <- eco.2.TTE <- eco.3.TTE <- eco.1.FCE <- eco.2.FCE <- eco.3.FCE <- c()

newdata=read.csv('mixed model.csv')
newdata$lat=as.numeric(newdata$lat)
newdata2=newdata[newdata$Ecosystems%in% c("Natural aquatic","Natural terrestrial"),]
newdata3=newdata[newdata$TL%in%"I-II",]
newdata3=newdata[newdata$TL%in%"II-III",]
newdata3=newdata[newdata$TL%in%"III-IV",]
newdata3=newdata[newdata$TL%in%"IV-",]
newdata3=newdata[newdata$interaction_type%in%"herbivore",]
newdata3=newdata[newdata$interaction_type%in%"carnivore",]
newdata3=newdata[newdata$path%in%"Herbivory",]
newdata3=newdata[newdata$path%in%"Mixotrophy",]
newdata3=newdata[newdata$taxon%in%"invertebrate",]
newdata3=newdata[newdata$taxon%in%c("vertebrate ectotherm","invertebrate"),]
newdata4=newdata[newdata$taxon%in%c("vertebrate endotherm"),]

   geom_smooth(data = newdata3, mapping = aes(x = lat, y = eff,color="#E20613"), 
              method = "lm")+
  geom_smooth(data = newdata4, mapping = aes(x = lat, y = eff,color="#3A4696"), 
              method = "lm")
   
model1=glmer(eff~lat+(1|TL), data = newdata3,family = Gamma(link="log"))
summary(model1)
model2=glmer(eff~lat+(lat|taxon), data = newdata,family = Gamma(link="log"))
summary(model2)
newdata3=newdata[newdata$taxon%in%c("vertebrate ectotherm","invertebrate"),]
newdata4=newdata[newdata$taxon%in%c("vertebrate endotherm"),]
newdata$taxon2[newdata$taxon%in%c("vertebrate ectotherm","invertebrate")]="ectotherm"
newdata$taxon2[newdata$taxon%in%"vertebrate endotherm"]="endotherm"
newdata$taxon2=factor(newdata$taxon2)
model1=glmer(eff~lat-1+(lat|taxon2), data = newdata,family = Gamma(link="log"))
summary(model1)
model1=glm(eff~lat, data = newdata3,family = Gamma(link="log"))
summary(model1)
model2=glm(eff~lat, data = newdata4,family = Gamma(link="log"))
summary(model2)

summary(aov(model1))
summary(aov(eff~lat*taxon2, data = newdata))
model1=betareg(eff~lat, data = newdata3)
summary(model1)
model2=betareg(eff~lat, data = newdata4)
summary(model2)

plot(eff~lat,newdata3)
new_mpg1 <- seq(min(newdata3$lat,na.rm = TRUE), max(newdata3$lat,na.rm = TRUE), 0.1)
new_mpg2 <- seq(min(newdata4$lat,na.rm = TRUE), max(newdata4$lat,na.rm = TRUE), 0.1)

m1=data.frame(pre=predict.glm(model1, newdata = data.frame(lat = new_mpg1),type = "response"),new_mpg=new_mpg1)
m2=data.frame(pre=predict.glm(model2, newdata = data.frame(lat = new_mpg2),type = "response"),new_mpg=new_mpg2)



l=ggplot() +
  
  #scale_color_manual(name="Taxon",values=c("red"="#3B4992FF","blue"="#BB0021FF"))+ ## set the color
  
  geom_point(data = newdata3, mapping = aes(x = lat, y = eff,color="#3B4992FF"), alpha = 0.1,size=2) +
  geom_point(data = newdata4, mapping = aes(x = lat, y = eff,color="#BB0021FF"), alpha = 0.8,size=2) +

  geom_line(data = m1, mapping = aes(x = new_mpg, y = pre,color="#3B4992FF"),size=1.5,lty=1,alpha=1,show.legend = TRUE) +
  geom_line(data = m2, mapping = aes(x = new_mpg, y = pre,color="#BB0021FF"), size=1.5,lty=1,alpha=1)+
  annotate("text", x = 17, y = 0.35, size = 5, label = "p=0.0136",color="#BB0021FF" )+
  annotate("text", x = 17, y = 0.32, size = 5, label = "p=0.0034",color="#3B4992FF" )+
  xlab("Latitude(°)") +
  ylab("TTE") + 
  guides(fill = guide_legend(title = "Condition"))+
  scale_y_continuous(limits=c(0,0.4))+
theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(#legend.position = c(0.85, 0.9), 
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.x = element_text(size=18, hjust = 0.5, angle = 0), 
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=20, vjust=0), 
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        plot.margin=unit(c(0.5,2,0.5,0.5),"lines"))

l
pdf("latitude2.pdf",width = 9.5,height = 6)

l
dev.off()
##env====
C.TTE.data <- read.csv('C.TTE.data.all.csv', header = T)
C.TTE.data <- C.TTE.data[which(is.na(as.numeric(as.vector(C.TTE.data$trans)))==FALSE & as.numeric(C.TTE.data$trans) > 0& as.numeric(C.TTE.data$trans)<=1&C.TTE.data$Data.form=="single value"), ]
newdata <- C.TTE.data[which(!(as.vector(C.TTE.data$Nutrient) %in% c("", NA, "Y"))) , ]

for (i in 1:nrow(newdata)){
  if (newdata$Nutrient[i] %in% c('H', 'XH')) {newdata$Nutrient[i] <- 'new_H'}
  if (newdata$Nutrient[i] %in% c('L', 'XL')) {newdata$Nutrient[i] <- 'new_L'}
  if (newdata$Nutrient[i] %in% c('M')) {newdata$Nutrient[i] <- 'new_M'}
  if (newdata$Nutrient[i] %in% c('0')) {newdata$Nutrient[i] <- 'new_L'}
}

newdata$new_eff=log10(1/(1-newdata$trans))
new_model=lmer(new_eff ~Nutrient-1+(1|new.id), data = newdata)
summary(new_model)
