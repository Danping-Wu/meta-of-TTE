library(ggplot2)
library(cowplot)
ES.eco<- read.csv('Ecosystem.csv', header = T)
newdata<- read.csv('mixed model.csv', header = T)
newdata$Ecosystem.type2=factor(newdata$Ecosystem.type2,levels = c("Marine","Freshwater","Terrestrial"))
newdata$Ecosystem.origin=factor(newdata$Ecosystem.origin,levels = c("Natural","Artificial"))
newdata$new_eff=log(newdata$eff/(1-newdata$eff))
ES.eco$sig.ano=paste(ES.eco$sig,ES.eco$ano)
ES.eco.type=ES.eco[ES.eco$Variable=="Ecosystem type",]

plot.type=ggplot()+ 
  geom_jitter(data=newdata, aes(x = Ecosystem.type2, y = new_eff),color="grey",alpha=0.8, show.legend = FALSE)+
  geom_violin(data=newdata,aes(x = Ecosystem.type2, y = new_eff,color=Ecosystem.type2),alpha=0.2,size=1.5, show.legend = FALSE)+
  geom_errorbar(data=ES.eco.type,aes(x=1:3,ymin=lb, ymax=ub,color=factor(Cat),group=factor(Cat) ),width=0.1,cex=1,alpha=1, show.legend = FALSE)+ 
  geom_point(data=ES.eco.type,aes(x=1:3,y=b,group=factor(Cat) ,color=factor(Cat)),cex=2,shape=19, show.legend = FALSE)+
  geom_text(data=ES.eco.type,aes(x=c(1:3)-0.1,y=4.3 ,label=sig.ano,color=factor(Cat)),size=8,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.eco.type,aes(x=c(1:3)-0.25,y=3.2 ,label=N,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  
  scale_colour_manual(values=c("#E20613","#E20613","#3A4696","#3A4696","#008B45FF","#008B45FF"),name=" ")+
  geom_hline(aes(yintercept =log(1/9)),color="black",size=1,lty=2) +
  xlab("Ecosystem type")+
  ylab("log(TTE/(1-TTE))")+
  ggtitle(label =expression(bold("A")) )+
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_text(size=15),
    #axis.line.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text.x =element_text(size=15),
    axis.title = element_text(hjust=0.5,size=18),
    legend.position = 'top',
    legend.key = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype="solid"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "pt"))


ES.eco.origin=ES.eco[ES.eco$Variable=="Ecosystem origin",]

plot.origin=ggplot()+ 
  geom_jitter(data=newdata, aes(x = Ecosystem.origin, y = new_eff),color="grey",alpha=0.8, show.legend = FALSE)+
  geom_violin(data=newdata,aes(x = Ecosystem.origin, y = new_eff,color=Ecosystem.origin),alpha=0.2,size=1.5, show.legend = FALSE)+
  geom_errorbar(data=ES.eco.origin,aes(x=1:2,ymin=lb, ymax=ub,color=factor(Cat),group=factor(Cat) ),width=0.1,cex=1,alpha=1, show.legend = FALSE)+ 
  geom_point(data=ES.eco.origin,aes(x=1:2,y=b,group=factor(Cat) ,color=factor(Cat)),cex=2,shape=19, show.legend = FALSE)+
  geom_text(data=ES.eco.origin,aes(x=c(1:2)-0.06,y=4.3 ,label=sig.ano,color=factor(Cat)),size=8,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(data=ES.eco.origin,aes(x=c(0.84,1.75),y=3.2 ,label=N,color=factor(Cat)),size=7,hjust=0,vjust=0.3, show.legend = FALSE)+
  
  scale_colour_manual(values=c("#3A4696","#3A4696","#E20613","#E20613"),name=" ")+
  geom_hline(aes(yintercept =log(1/9)),color="black",size=1,lty=2) +
  xlab("Ecosystem origin")+
  ylab("")+
  ggtitle(label =expression(bold("B")) )+
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_text(size=15),
    #axis.line.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.text.x =element_text(size=15),
    axis.title = element_text(hjust=0.5,size=18),
    legend.position = 'top',
    legend.key = element_blank(),
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(fill=NA,color="black", size=1, linetype  ="solid"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.margin = margin(5, 5, 5, 5, "pt"))
plot.origin

pdf("Ecosystem.pdf",width = 12,height = 6)

plot_grid(plot.type,plot.origin,nrow = 1,  rel_widths = c(1.2,1),align = "h")
dev.off()
