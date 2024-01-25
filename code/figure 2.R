library(ggplot2)
library(cowplot)
library(lmerTest)
library(grid)
library(viridis)
library(multcomp)
dat <- read.csv('distribution.csv', header = T)
dat$Definition <- factor(dat$Definition,levels = c("Ingestion","Assimilation","Production"))
dat=dat[-which(dat$new.id==46),]
length(unique(dat$new.id))
nrow(dat)
dat$new_eff=log(dat$eff/(1-dat$eff))
model=lmer(new_eff~element2-1+(1|new.id), data = dat)

ss=summary(model)
ss

summary(glht(model, linfct=cbind(contrMat(c("Energy"=1,"Nutrient"=1), type="Tukey"))))

m=1/(exp(-ss[["coefficients"]][,1])+1)
#s=log10(exp(ss[["coefficients"]][,2]))
t=(ss[["coefficients"]][,1]-log(1/9))/ss[["coefficients"]][,2]
n=table(dat$element2)
P=round(2*pt(-abs(t), df=n-1),6)
##energy====
dat_energy=dat[which(dat$element2=="Energy"), ]
n_energy=nrow(dat_energy)
length(unique(dat_energy$new.id))

length(which(dat_energy$eff<=0.05))/n_energy
distri_energy=ggplot(dat_energy, aes(eff)) +
  geom_histogram( aes(y=(..count..)/(n_energy/80),fill=Definition), position="stack",
                  alpha=0.5,  
                  center = 0,bins = 80)+ 
  scale_fill_manual(values=c("#008B45FF","#E20613","#3A4696"),name=" ")+
  geom_density(data = dat_energy,aes(x = eff,y= ..density..),alpha =1 , color="black",size=1) + 
  
  
  geom_rug(color="#56B4E9") + 
  
  annotate("text", x = 0.6, y = 2.3, size = 6, label = "(107/1269)" )+
  #annotate("text", x = 0.6, y = 2.5, size = 6, label = "SD=1.13" )+
  annotate("text", x = 0.6, y = 3, size = 6, label = "mean=5.88% *** a" )+
  
  #annotate("text", x = 0.5, y = 1.5, size = 5, label = "median=6.59% *" )+
  xlab("TTE (Energy)") +
  ylab("Density distribution") +
  ggtitle(label =expression(bold("A")) )+
  coord_cartesian(xlim = c(0, 1)) + 
  scale_x_continuous(breaks=seq(0, 1, 0.1)) + 
  geom_vline(aes(xintercept=m[1]), colour="grey", linetype="dashed",size=2)+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = c(0.3, 0.8), 
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size=18, hjust = 0.5, angle = 0), 
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=20, vjust=0), 
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))

distri_energy

dat_energy$new_eff <- log(dat_energy$eff/(1-dat_energy$eff))


log_m_energy=ss[["coefficients"]][1,1]
log_m_energy
logit_distri_energy <- ggplot(dat_energy, aes(new_eff)) +
  geom_histogram( aes(y=..density..),color="darkgrey", fill="darkgrey", 
                  alpha=1, 
                  center = 0,bins = 30)+ 
  geom_density(alpha = 0.6, color='#0072B2' ,size=1.5) + 
  geom_rug(color="#56B4E9") + 
  xlab(NULL) +
  ylab(NULL)   + 
  scale_x_continuous(breaks=seq(-6, 0, 1)) + 
  ggtitle("log(TTE/(1-TTE))")+
  geom_vline(aes(xintercept=log_m_energy), colour="#BB0000",size=1.5)+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = c(0.85, 0.80), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        #axis.text.x = element_text(size=15, hjust = 0.5, angle = 0), 
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=16, vjust=0), 
        axis.title.y = element_text(size=15, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        plot.title = element_text (size=16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),

        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))

logit_distri_energy

## Nutrient====

dat_nutrient=dat[which(dat$element2=="Nutrient"), ]
length(unique(dat_nutrient$new.id))
n_nutrient=nrow(dat_nutrient)
distri_nutrient=ggplot(dat_nutrient, aes(eff)) +
  geom_histogram( aes(y=(..count..)/(n_nutrient/80),fill=Definition), position="stack",
                  alpha=0.5,  
                  center = 0,bins = 80)+ 
  scale_fill_manual(values=c("#008B45FF","#E20613","#3A4696"),name=" ")+
  
  
  geom_density(data = dat_nutrient,aes(x = eff,y= ..density..),alpha = 1 , color="black",size=1) + 
  
  
  geom_rug(color="#56B4E9") + 
  
  annotate("text", x = 0.6, y = 2.3, size = 6, label = "(10/338)" )+
  #annotate("text", x = 0.6, y = 2.3, size = 6, label = "SD=1.19" )+
  annotate("text", x = 0.6, y = 2.8, size = 6, label = "mean=8.95% b" )+
  
  #annotate("text", x = 0.5, y = 1.5, size = 5, label = "median=6.59% *" )+
  xlab("TTE (Nutrient)") +
  ylab("Density distribution") +
  ggtitle(label =expression(bold("C")) )+
  coord_cartesian(xlim = c(0, 1)) + 
  scale_x_continuous(breaks=seq(0, 1, 0.1)) + 
  scale_y_continuous(breaks=seq(0, 9, 3)) + 
  geom_vline(aes(xintercept=m[2]), colour="grey", linetype="dashed",size=2)+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = c(0.3, 0.8), 
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        axis.title=element_text(size=14),
        axis.text.x = element_text(size=18, hjust = 0.5, angle = 0), 
        axis.text.y = element_text(size=18),
        axis.title.x = element_text(size=20, vjust=0), 
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))


distri_nutrient
dat_nutrient$new_eff <- log(dat_nutrient$eff/(1-dat_nutrient$eff))


log_m_nutrient=ss[["coefficients"]][2,1]
log_m_nutrient

logit_distri_nutrient <- ggplot(dat_nutrient, aes(new_eff)) +
  geom_histogram( aes(y=..density..),color="darkgrey", fill="darkgrey", 
                  alpha=1,  
                  center = 0,bins = 30)+
  geom_density(alpha = 0.6, color='#0072B2' ,size=1.5) + 
  geom_rug(color="#56B4E9") + 
  xlab(NULL) +
  ylab(NULL)   + 
  ggtitle(label="log(TTE/(1-TTE))")+
  geom_vline(aes(xintercept=log_m_nutrient), colour="#BB0000",size=1.5)+
  scale_x_continuous(breaks=seq(-4, 0, 1)) + 
  
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = c(0.85, 0.80), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=16, vjust=0), 
        axis.title.y = element_text(size=16, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        plot.title = element_text (size=16),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))
logit_distri_nutrient

##mixed model====
####Energy====
ES.definition.energy<- read.csv('definition_energy.csv', header = T)

lwd=.pt*72.27/96

ES.definition.energy$group=ES.definition.energy$Variable
ES.definition.energy$group[duplicated(ES.definition.energy$group)]=" "
n=nrow(ES.definition.energy)
ES.definition.energy$pos[n]=0.5
ES.definition.energy$h[n]=-1
for(i in 1:(n-1)){
  ES.definition.energy$pos[n-i]=ifelse(ES.definition.energy$Variable[n-i]==ES.definition.energy$Variable[n+1-i],ES.definition.energy$pos[n+1-i]+1,ES.definition.energy$pos[n+1-i]+1.5)
  ES.definition.energy$h[n-i]=ifelse(ES.definition.energy$Variable[n-i]==ES.definition.energy$Variable[n+1-i],-1,mean(ES.definition.energy$pos[c(n-i,n+1-i)]))
  
}

ES.definition.energy$h2[ES.definition.energy$group==" "]=-1
h2=aggregate(ES.definition.energy$pos,list(ES.definition.energy$Variable),mean)[,2]
ES.definition.energy$h2[is.na(ES.definition.energy$h2)]=h2[order(h2,decreasing =TRUE)]
ES.definition.energy$lab=ES.definition.energy$N

ES.definition.energy$h=2*ES.definition.energy$h
ES.definition.energy$h2=2*ES.definition.energy$h2
ES.definition.energy$pos=2*ES.definition.energy$pos

vignette("ggsci")

max.ub.energy=max(ES.definition.energy$ub,na.rm = TRUE)
min.lb.energy=min(ES.definition.energy$lb,na.rm = TRUE)

ALL.energy=
  ggplot(data=ES.definition.energy,aes(y =pos,x =b ), show.legend = FALSE)+
  geom_text(aes(label = Cat,x=min.lb.energy-1.5,color=factor(Cat)), hjust=0.5,vjust = 0.5,size=18/.pt) +
  geom_text(aes(y=h2,x=min.lb.energy-3.5,label =Variable),color="black", vjust = 0.5,hjust=0.6,size=18/.pt,lineheight=1, show.legend = FALSE)+    
  geom_errorbar(aes(xmin=lb, xmax=ub,y=pos,color=factor(Cat),group=factor(Cat) ),width=0,cex=6,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Cat),group=factor(Cat) ),cex=6,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ub+0.5,y=pos ,label=sig,color=factor(Cat)),size=18/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ub+0.7,y=pos ,label=lab,color=factor(Cat)),size=16/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(x=max.ub.energy+1.8,y=pos ,label=ano,color=factor(Cat)),size=16/.pt,hjust=0,vjust=0.3)+
  #scale_color_aaas()+scale_fill_aaas()+
  scale_color_manual(values=c("black","#E20613","#008B45FF","#3A4696"),name=" ")+
  geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=log(1/9),y=-0.5,xend=log(1/9),yend=max(ES.definition.energy$pos)+1.5,linetype="longdash",col="gray70",size=1.2/lwd)+
  ggtitle(label =expression(bold("B")) )+
  
  scale_x_continuous(limits = c(min.lb.energy-5,max.ub.energy+2.2),breaks=seq(-7,-2,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(-0.5,max(ES.definition.energy$pos)+1.5),expand =expansion(add = c(0,0)))+
  xlab("")+
  ylab("")+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = "none", 
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        axis.title=element_text(size=14),

        axis.title.x = element_text(size=20, vjust=0), 
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=18, hjust = 0.5, angle = 0,color="white"), 
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))

ALL.energy
####Nutrient====
ES.definition.nutrient<- read.csv('definition_element_nutrient.csv', header = T)

lwd=.pt*72.27/96

ES.definition.nutrient$group=ES.definition.nutrient$Variable
ES.definition.nutrient$group[duplicated(ES.definition.nutrient$group)]=" "
n=nrow(ES.definition.nutrient)
ES.definition.nutrient$pos[n]=1
ES.definition.nutrient$h[n]=-1
for(i in 1:(n-1)){
  ES.definition.nutrient$pos[n-i]=ifelse(ES.definition.nutrient$Variable[n-i]==ES.definition.nutrient$Variable[n+1-i],ES.definition.nutrient$pos[n+1-i]+1,ES.definition.nutrient$pos[n+1-i]+2)
  ES.definition.nutrient$h[n-i]=ifelse(ES.definition.nutrient$Variable[n-i]==ES.definition.nutrient$Variable[n+1-i],-1,mean(ES.definition.nutrient$pos[c(n-i,n+1-i)]))
  
}

ES.definition.nutrient$h2[ES.definition.nutrient$group==" "]=-1
h2=aggregate(ES.definition.nutrient$pos,list(ES.definition.nutrient$Variable),mean)[,2]
ES.definition.nutrient$h2[is.na(ES.definition.nutrient$h2)]=h2[order(h2,decreasing =TRUE)]
ES.definition.nutrient$lab=ES.definition.nutrient$N

vignette("ggsci")

max.ub.nutrient=max(ES.definition.nutrient$ub,na.rm = TRUE)
min.lb.nutrient=min(ES.definition.nutrient$lb,na.rm = TRUE)

ALL.nutrient=
  ggplot(data=ES.definition.nutrient,aes(y =pos,x =b ), show.legend = FALSE)+
  geom_text(aes(label = Cat,x=min.lb.nutrient-1,color=factor(Cat)), hjust=0.5,vjust = 0.5,size=18/.pt, show.legend = FALSE) +
  geom_text(aes(y=h2,x=min.lb.nutrient-2.7,label =Variable),color="black", vjust = 0.5,hjust=0.6,size=18/.pt,lineheight=1, show.legend = FALSE)+    
  geom_errorbar(aes(xmin=lb, xmax=ub,y=pos,color=factor(Cat),group=factor(Cat) ),width=0,cex=6,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Cat),group=factor(Cat) ),cex=6,shape=19, show.legend = FALSE)+
  geom_text(aes(x=ub+0.2,y=pos ,label=sig,color=factor(Cat)),size=18/.pt,hjust=1,vjust=0.7)+
  geom_text(aes(x=ub+0.3,y=pos ,label=lab,color=factor(Cat)),size=16/.pt,hjust=0,vjust=0.3)+
  geom_text(aes(x=max.ub.nutrient+1.3,y=pos ,label=ano,color=factor(Cat)),size=16/.pt,hjust=0,vjust=0.3)+
  #scale_color_aaas()+scale_fill_aaas()+

  scale_color_manual(values=c("black","#E20613","#008B45FF","black","black","black","#3A4696"),name=" ")+
  geom_hline(aes(yintercept =h),color="gray",size=1.1/lwd) +
  geom_segment(x=log(1/9),y=0,xend=log(1/9),yend=max(ES.definition.nutrient$pos)+1,linetype="longdash",col="gray70",size=1.2/lwd)+
  ggtitle(label =expression(bold("D")) )+
  
  scale_x_continuous(limits = c(min.lb.nutrient-3.7,max.ub.nutrient+1.6),breaks=seq(-3,0,1),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(0,max(ES.definition.nutrient$pos)+1),expand =expansion(add = c(0,0)))+
  xlab("log(TTE/(1-TTE))")+
  ylab("")+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = "none", 
        legend.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
        axis.title=element_text(size=14),
        axis.title.x = element_text(size=20, vjust=0), 
        axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(size=18, hjust = 0.5, angle = 0,color="white"), 
        
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))

ALL.nutrient
####figure=======
distri_energy_all=ggdraw() +
  draw_plot(distri_energy,0,0,0.5,1)+
  draw_plot(logit_distri_energy,0.24,0.4,0.25,0.5)+
  draw_plot(ALL.energy,0.5,0,0.5,1)

distri_nutrient_all=ggdraw() +
  draw_plot(distri_nutrient,0,0,0.5,1)+
  draw_plot(logit_distri_nutrient,0.24,0.4,0.25,0.5)+
  draw_plot(ALL.nutrient,0.5,0,0.5,1)

all.figure=plot_grid(distri_energy_all,distri_nutrient_all,nrow = 2,align = "v")

pdf("distribution_sum.pdf",width = 16,height = 12)
all.figure
dev.off()

