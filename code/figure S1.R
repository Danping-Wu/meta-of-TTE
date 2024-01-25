library(ggplot2)
library(cowplot)
library(lmerTest)
data <- read.csv("ecopath.data.csv", header = T)
dat <- data[which(data$TTE>0& data$TTE<=1&is.na(data$TTE)==FALSE), ]
length(unique(dat$FW))
dat$new_TTE=log(dat$TTE/(1-dat$TTE))
model=lmer(new_TTE~(1|FW), data = dat)

ss=summary(model)
ss
m=ss[["coefficients"]][,1]
s=ss[["coefficients"]][,2]
t=(m-log(1/9))/s
n=nrow(dat)
P=round(2*pt(-abs(t), df=n-1),6)
m=1/(exp(-ss[["coefficients"]][,1])+1)
##energy====
length(unique(dat$FW))

dis.plot=ggplot(dat, aes(TTE)) +
  geom_histogram( aes(y=..density..),color="darkgrey", fill="darkgrey", 
                  alpha=1, 
                  center = 0,bins = 50)+ 
  geom_density(alpha = 0.6, color='#0072B2' ,size=1.5) + 
  
  
  geom_rug(color="#56B4E9") + 
  
  annotate("text", x = 0.6, y = 0.7, size = 7, label = "(270/1066)" )+
  #annotate("text", x = 0.6, y = 1, size = 5, label = "SD=1.03" )+
  annotate("text", x = 0.6, y = 1.2, size = 7, label = "mean=10.49%" )+
  
  #annotate("text", x = 0.5, y = 1.5, size = 5, label = "median=6.59% *" )+
  xlab("TTE") +
  ylab("Density distribution") +
  ggtitle(label =expression(bold("A")) )+
  coord_cartesian(xlim = c(0, 1)) + 
  scale_x_continuous(breaks=seq(0, 1, 0.1)) + 
  geom_vline(aes(xintercept=m), colour="#BB0000", linetype="dashed",size=2)+
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
        plot.margin=unit(c(0.5,1,0.5,0.5),"lines"))

dis.plot



log_m=ss[["coefficients"]][,1]
log_m
log_distri <- ggplot(dat, aes(new_TTE)) +
  geom_histogram( aes(y=..density..),color="darkgrey", fill="darkgrey", 
                  alpha=1,  
                  center = 0,bins = 50)+ 
  geom_density(alpha = 0.6, color='#0072B2' ,size=1.5) + 
  geom_rug(color="#56B4E9") + 
  xlab(NULL) +
  ylab(NULL)   + 
  ggtitle("log(TTE/(1-TTE))")+
  geom_vline(aes(xintercept=log_m), colour="#BB0000",size=1.5)+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  theme(legend.position = c(0.85, 0.80), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black", size = 8), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size = 0.5),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=15),
        axis.title.x = element_text(size=16, vjust=0), 
        axis.title.y = element_text(size=15, margin = margin(t = 0, r = 5, b = 0, l = 0), vjust=2.5),
        axis.ticks = element_blank(),
        plot.title = element_text (size=16),
        plot.margin=unit(c(0,0,0,0),"lines"))

log_distri

##TTE figure=====
ES<- read.csv('ecopath_model.csv', header = T)
newdata <- read.csv("D:/work/phd/computer/data/ecopath.data.csv", header = T)
newdata <- newdata[which(newdata$TTE>0& newdata$TTE<=1&is.na(newdata$TTE)==FALSE), ]
newdata$new_TTE=log(newdata$TTE/(1-newdata$TTE))
newdata$TL=gsub("1_2", "I-II", newdata$TL) 
newdata$TL=gsub("2_3", "II-III", newdata$TL) 
newdata$TL=gsub("3_4", "III-IV", newdata$TL) 
newdata$TL=gsub("4_5", "IV-V", newdata$TL) 
newdata$Ecosystem=gsub("freshwater", "Freshwater", newdata$Ecosystem) 
newdata$Ecosystem=gsub("marine", "Marine", newdata$Ecosystem) 
newdata$Ecosystem=factor(newdata$Ecosystem,c("Marine","Freshwater"))
ES.TL=ES[ES$Variable=="TL",]
max.data=max(newdata$new_TTE)
min.data=min(newdata$new_TTE)
plot.TL=ggplot()+ 
  geom_jitter(data=newdata, aes(x = TL, y = new_TTE),color="grey",alpha=0.8, show.legend = FALSE)+
  geom_violin(data=newdata,aes(x = TL, y = new_TTE,color=factor(TL)),alpha=0.2,size=1.5, show.legend = FALSE)+
  geom_errorbar(data=ES.TL,aes(x=1:4,ymin=lb, ymax=ub,color=factor(Cat),group=factor(Cat) ),width=0.1,cex=0.8,alpha=1, show.legend = FALSE)+ 
  geom_point(data=ES.TL,aes(x=1:4,y=b,group=factor(Cat) ,color=factor(Cat)),cex=1.5,shape=19, show.legend = FALSE)+
  geom_point(aes(x=3,y=max.data+2.2 ,color="#E20613"),cex=1,shape=19, show.legend = FALSE)+
  geom_text(data=ES.TL,aes(x=c(1:4)-c(0.12,0.12,0.05,0.12),y=max.data+c(0.8,0.8,2,0.8)+1 ,label=sig,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.TL,aes(x=c(1:4)-0.05,y=max.data+1.4 ,label=ano,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.TL,aes(x=c(1:4)-0.4,y=max.data+0.4 ,label=N,color=factor(Cat)),size=6,hjust=0,vjust=0.3, show.legend = FALSE)+

  scale_y_continuous(limits=c(min.data-0.5,max.data+2.2))+
  
  scale_colour_manual(values=c(rep("#E20613",8)),name=" ")+
  geom_hline(aes(yintercept =log(1/9)),color="black",size=1,lty=2) +
  xlab("TL")+
  ylab("")+
  ggtitle(label =expression(bold("C")))+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  
  theme(
    #plot.title = element_text(colour="white"),
    legend.position = c(0.3, 0.8), 
    legend.title = element_text(color = "black", size = 15),
    legend.text = element_text(color = "black", size = 12), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    axis.title=element_text(size=14),
    axis.text.x = element_text(size=18, hjust = 0.5, angle = 0), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=20, vjust=0), 
    axis.title.y = element_text(size=20, margin = margin(t = 0, r = 5, b = 0, l = -0.5), vjust=2.5),
    plot.margin=unit(c(0.5,0.5,0.5,-0.5),"lines"))

plot.TL

ES.Eco=ES[ES$Variable=="Ecosystem type",]

plot.Eco=ggplot()+ 
  geom_jitter(data=newdata, aes(x = Ecosystem, y = new_TTE),color="grey",alpha=0.8, show.legend = FALSE)+
  geom_violin(data=newdata,aes(x = Ecosystem, y = new_TTE,color=factor(Ecosystem)),alpha=0.2,size=1.5, show.legend = FALSE)+
  geom_errorbar(data=ES.Eco,aes(x=1:2,ymin=lb, ymax=ub,color=factor(Cat),group=factor(Cat) ),width=0.1,cex=0.8,alpha=1, show.legend = FALSE)+ 
  geom_point(data=ES.Eco,aes(x=1:2,y=b,group=factor(Cat) ,color=factor(Cat)),cex=1.5,shape=19, show.legend = FALSE)+
  geom_text(data=ES.Eco,aes(x=c(1:2)-0.05,y=max.data+1.8 ,label=sig,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.Eco,aes(x=c(1:2)-0.05,y=max.data+1.4 ,label=ano,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.Eco,aes(x=c(1:2)-c(0.3,0.35),y=max.data+0.4 ,label=N,color=factor(Cat)),size=6,hjust=0,vjust=0.3, show.legend = FALSE)+
  
  scale_y_continuous(limits=c(min.data-0.5,max.data+2.1))+
  
  scale_colour_manual(values=c(rep("#3A4696",4)),name=" ")+
  geom_hline(aes(yintercept =log(1/9)),color="black",size=1,lty=2) +
  xlab("Ecosystem type")+
  ylab("log(TTE/(1-TTE)）")+
  ggtitle(label =expression(bold("B")))+
  theme_bw() + 
  theme_classic(base_size = 16) + 
  
  theme(
    legend.position = c(0.3, 0.8), 
    legend.title = element_text(color = "black", size = 15),
    legend.text = element_text(color = "black", size = 12), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    axis.title=element_text(size=14),
    axis.text.x = element_text(size=18, hjust = 0.5, angle = 0), 
    axis.text.y = element_text(size=18,colour = "white"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=20, vjust=0), 
    axis.title.y = element_text(size=20, margin = margin(t = 0, r = 0, b = 0, l = 0), vjust=2.5),
    plot.margin=unit(c(0.5,0.5,0.5,0.5),"lines"))

plot.Eco

distri_all=ggdraw() +
  draw_plot(dis.plot,0,0.5,1,0.5)+
  draw_plot(log_distri,0.42,0.71,0.48,0.23)+
  draw_plot(plot.Eco,0,0,0.4,0.5)+
  draw_plot(plot.TL,0.4,0,0.6,0.5)
distri_all

pdf("Ecopath2.pdf",width = 10,height = 10)

distri_all
dev.off()

