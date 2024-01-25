library(ggplot2)
library(cowplot)
######I=========
##species====
ES.spe<- read.csv('species.csv', header = T)
lwd=.pt*72.27/96

ES.spe$group=ES.spe$Variable
ES.spe$group[duplicated(ES.spe$group)]=" "

n=nrow(ES.spe)
ES.spe$pos[n]=1.5
ES.spe$h[n]=-1
ES.spe$h3[n]=-1
for(i in 1:(n-1)){
  ES.spe$pos[n-i]=ifelse(ES.spe$Variable[n-i]==ES.spe$Variable[n+1-i],ES.spe$pos[n+1-i]+3,ES.spe$pos[n+1-i]+4)
  ES.spe$h[n-i]=ifelse(ES.spe$Variable[n-i]==ES.spe$Variable[n+1-i],-1,mean(ES.spe$pos[c(n-i,n+1-i)]))
  ES.spe$h3[n-i]=ifelse(ES.spe$Cat[n-i]==ES.spe$Cat[n+1-i],-1,mean(ES.spe$pos[c(n-i,n+1-i)]))
}

ES.spe$h2[ES.spe$group==" "]=-1
h2=aggregate(ES.spe$pos,list(ES.spe$Variable),mean)[,2]
ES.spe$h2[is.na(ES.spe$h2)]=h2[order(h2,decreasing =TRUE)]
ES.spe$lab=ES.spe$N

ES.spe$h=2*ES.spe$h
ES.spe$h2=2*ES.spe$h2
ES.spe$h3=2*ES.spe$h3
ES.spe$pos=2*ES.spe$pos

vignette("ggsci")



max.ub.spe=max(ES.spe$ub,na.rm = TRUE)
min.lb.spe=min(ES.spe$lb,na.rm = TRUE)
ES.spe$Variable=sub(" ","\n",ES.spe$Variable)
#ES.spe$Cat=sub(" ","\n",ES.spe$Cat)
ALL=
  ggplot(data=ES.spe,aes(y =pos,x =b ), show.legend = FALSE)+
  geom_text(aes(label = Cat,x=min.lb.spe-2,color=factor(Group)), hjust=0.5,vjust = 0.5,size=16/.pt, show.legend = FALSE) +
  geom_text(aes(y=h2,x=min.lb.spe-5,label =Variable,color=factor(Group)), vjust = 0.5,hjust=0.6,size=17/.pt,lineheight=1, show.legend = FALSE)+
  geom_errorbar(aes(xmin=lb, xmax=ub,y=pos,color=factor(Group),group=factor(Group) ),width=0,cex=5,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,group=factor(Group) ),cex=5,shape=19, show.legend = TRUE)+
  geom_text(aes(x=ub+0.47,y=pos ,label=sig,color=factor(Group)),size=15/.pt,hjust=1,vjust=0.7, show.legend = FALSE)+
  geom_text(aes(x=ub+0.6,y=pos ,label=lab,color=factor(Group)),size=14/.pt,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(aes(x=max.ub.spe+1.5,y=pos ,label=ano,color=factor(Group)),size=14/.pt,hjust=0,vjust=0.3, show.legend = FALSE)+
  #scale_color_aaas()+scale_fill_aaas()+
  scale_color_manual(values=c("black"),name=" ")+
  geom_segment(x=min.lb.spe,y=ES.spe$h3,xend=max.ub.spe+2,yend=ES.spe$h3,color="gray",size=1.1/lwd,lty=2) +
  geom_hline(aes(yintercept =h),color="black",size=1.1/lwd,lty=1) +
  geom_segment(x=log(1/9),y=0,xend=log(1/9),yend=max(ES.spe$pos)+3,linetype="longdash",col="gray70",size=1.2/lwd)+
  
  scale_x_continuous(limits = c(min.lb.spe-6.5,max.ub.spe+2),breaks=seq(-2.6,-0.8,0.3),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(0,max(ES.spe$pos)+3.5),expand =expansion(add = c(0,0)))+
  xlab("log(TTE/(1-TTE))")+
  
  theme(axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.x =element_text(size=12),
        axis.title = element_text(hjust=1,size=15),
        legend.position = 'top',
        legend.key = element_blank(),
        legend.text=element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 0,  5, 0, "pt"))

ALL



##type====
ES.type<- read.csv('type_species.csv', header = T)
lwd=.pt*72.27/96

ES.type$group=ES.type$Variable
ES.type$group[duplicated(ES.type$group)]=" "

n=nrow(ES.type)
ES.type$pos[c(n)]=0.5
ES.type$h[c(n)]=-1
ES.type$h3[c(n)]=-1
for(i in 1:(n-1)){
  ES.type$pos[n-i]=ifelse(ES.type$Variable[n-i]==ES.type$Variable[n+1-i],ES.type$pos[n+1-i]+1,ES.type$pos[n+1-i]+2)
  ES.type$h[n-i]=ifelse(ES.type$Variable[n-i]==ES.type$Variable[n+1-i],-1,mean(ES.type$pos[c(n-i,n+1-i)]))
  ES.type$h3[n-i]=ifelse(ES.type$Cat[n-i]==ES.type$Cat[n+1-i],-1,mean(ES.type$pos[c(n-i,n+1-i)]))
  
}

ES.type$h2[ES.type$group==" "]=-1
ES.type$h2[is.na(ES.type$h2)]=aggregate(ES.type$pos,list(ES.type$Variable),mean)[,2]
ES.type$lab=ES.type$N

ES.type$h=2*ES.type$h
ES.type$h2=2*ES.type$h2
ES.type$h3=2*ES.type$h3
ES.type$pos=2*ES.type$pos

max.ub.type=max(ES.type$ub,na.rm = TRUE)
min.lb.type=min(ES.type$lb,na.rm = TRUE)
ES.type$Type=factor(ES.type$Type,levels = c("Marine","Freshwater","Terrestrial"))
ALL.type=
  ggplot(data=ES.type,aes(y =pos,x =b ), show.legend = FALSE)+
  
  geom_errorbar(aes(xmin=lb, xmax=ub,y=pos,color=factor(Type),group=factor(Type) ),width=0,cex=3,alpha=0.3, show.legend = FALSE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Type),group=factor(Type) ),cex=3,shape=19, show.legend = TRUE)+
  geom_text(aes(x=ub+0.5,y=pos ,label=sig,color=factor(Type)),size=12/.pt,hjust=1,vjust=0.7, show.legend = FALSE)+
  geom_text(aes(x=ub+0.7,y=pos ,label=lab,color=factor(Type)),size=11/.pt,hjust=0,vjust=0.3, show.legend = FALSE)+
  geom_text(aes(x=max.ub.type+1.5,y=pos ,label=ano,color=factor(Type)),size=11/.pt,hjust=0,vjust=0.3, show.legend = FALSE)+
  #scale_color_aaas()+scale_fill_aaas()+
  scale_color_manual(values=c("#3A4696","#E20613","#008B45FF"),name=" ")+
  geom_segment(x=min.lb.type-0.1,y=ES.type$h3,xend=max.ub.type+2.2,yend=ES.type$h3,color="gray",size=1.1/lwd,lty=2) +
  geom_hline(aes(yintercept =h),color="black",size=1.1/lwd,lty=1) +
  geom_segment(x=log(1/9),y=0,xend=log(1/9),yend=max(ES.spe$pos)+3,linetype="longdash",col="gray70",size=1.2/lwd)+
  
  scale_x_continuous(limits = c(min.lb.type-0.1,max.ub.type+2.2),expand =expansion(add = c(0,0.1)))+
  scale_y_continuous(limits=c(0,max(ES.spe$pos)+3.5),expand =expansion(add = c(0,0)))+
  xlab("")+
  
  
  theme(axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.x =element_text(size=12),
        axis.title = element_text(hjust=1,size=15),
        legend.position = 'top',
        legend.key = element_blank(),
        legend.text=element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.margin = margin(5, 0,  5, 0, "pt"))

ALL.type

##figure====
pdf("species_inter.pdf",width = 15,height = 10)

plot_grid(ALL,ALL.type,nrow = 1,  rel_widths = c(1.5,1),align = "h")
dev.off()



