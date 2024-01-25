library(ggplot2)
library(cowplot)
Variable=c(rep("Nutrient",3),rep("Light",3),rep("DOC",3),rep("Primary\nproduction",3),rep("Predation",3))
Conclusion=rep(c("Positive","Uncorrelated","Negative"),5)
num=c(3,5,1,1,1,2,1,0,2,1,1,1,1,1,1)
env_factor=data.frame(Variable,Conclusion,num)
env_factor$Variable<-factor(env_factor$Variable,levels =c("Predation","Primary\nproduction","DOC","Light","Nutrient"))
env_factor$Conclusion<-factor(env_factor$Conclusion,levels =c("Positive","Uncorrelated","Negative"))
mat.num=matrix(env_factor$num,3,5)
dec.pos=t(t(mat.num)/colSums(mat.num))
mat.num[mat.num==0]=""

plot.pro=ggplot(env_factor)+
  geom_bar(mapping=aes(x = Variable, fill = Conclusion, y = num),stat = "identity",position ="fill",width=0.2,alpha=0.8, show.legend = TRUE) +
  coord_flip()+
  scale_fill_manual(values=c("#E20613","darkgrey","#008B45FF"),name=" ")+
  xlab("Experimental treatments")+
  ylab("Proportion of studies")+
  guides(fill = guide_legend(reverse=TRUE))+
  labs(fill = "Conclusions") +
  annotate("text", x = 5:1, y =dec.pos[3,]/2 , size = 5, label = mat.num[3,] )+
  annotate("text", x = 5:1, y =dec.pos[3,]+dec.pos[2,]/2 , size = 5, label = mat.num[2,] )+
  annotate("text", x = 5:1, y =1-dec.pos[1,]/2 , size = 5, label = mat.num[1,] )+
  theme(axis.text =element_text(size=18),
        axis.title = element_text(hjust=0.5,size=20,margin = margin(t = 0, r = 0, b = 0, l = 0), vjust=2.5),
        legend.position = 'top',
        legend.text=element_text(size=15),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        
        plot.margin = margin(5, 5,  5, 5, "pt"))

  
ES=read.csv("env_factor.csv")  
ES$pos=5:1#sort(seq(2,4,2/4), decreasing = TRUE) 
ES$Cat<-factor(ES$Cat,levels =c("Resource","Predator"))

ALL=
  ggplot(data=ES)+
  scale_color_manual(values=c("#E20613","#3A4696"))+
  
  geom_errorbar(aes(xmin=ci.lb, xmax=ci.ub,y=pos,color=factor(Cat),group=factor(Cat) ),width=0,cex=7,alpha=0.3, show.legend = TRUE)+ 
  geom_point(aes(x=b,y=pos,color=factor(Cat),group=factor(Cat) ),cex=7,shape=19, show.legend = TRUE)+
  annotate("text", x = max(ES$ci.ub)+0.6, y =pos , size = 6, label = ES$N )+
  geom_segment(x=0,y=0.8,xend=0,yend=5.2,linetype="longdash",col="gray60",size=1.3/lwd)+
  scale_y_continuous(limits = c(0.65,5.35))+
  scale_x_continuous(limits = c(min(ES$ci.lb)-0.1,max(ES$ci.ub)+1),breaks=seq(-4,2,1),expand =expansion(add = c(0,0.1)))+
  guides(fill = guide_legend(reverse=TRUE))+
  xlab("LRR")+
  ylab("")+
  theme_bw() + 

  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =element_text(size=18),
        axis.title = element_text(hjust=0.5,size=20,margin = margin(t = 5, r = 0, b = 5, l = 0), vjust=2.5),
        legend.position = 'top',
        legend.text=element_text(size=15),
        legend.title=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        plot.margin = margin(5, 5,  5, 5, "pt"))
ALL

all.figure=ggdraw() +
  draw_plot(plot.pro,0,0,0.55,1)+
  draw_plot(ALL,0.55,0,0.45,1)
all.figure

pdf("Environment_figure.pdf",width = 15,height = 10)

all.figure
dev.off()
