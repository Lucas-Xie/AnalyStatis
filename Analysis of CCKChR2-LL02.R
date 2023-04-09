library(ggplot2)
library(reshape2)
#######################################  fedora  ###############################

AA <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_02_05_CCKAi32-VIPAi32_LL_03/20211011_result/arcophase_CCKChR2_LL.txt",header = T)

A <- melt(AA[,2:13])


g <- gl(12,25)
al <- rep(0.2,300)
col <- c(rep("red",175),rep("black",125))
a1 <- rep(c(1:25),12)

Arco <- data.frame(x=a1,y=A[,2],color=col,alpha=al,group=g)

a2 <- rep(c(1:25),2)
g2 <- gl(2,25)
al2 <- rep(1,25)
col2 <- c(rep("black",25),rep("red",25))
conm <- apply(AA[,10:13],1,mean)
cckm <- apply(AA[,2:9],1,mean)
AA_mean <- data.frame(x=a2,y=c(conm,cckm),color2=col2,alpha2=al2,group2=g2)




############plot  multiple  line 
ggplot(Arco,aes(x,y,group=group)) + 
  geom_rect(aes(xmin=7,xmax=18,ymin=30,ymax=31),fill="lightskyblue1",alpha=0.05) +
  geom_line(alpha=Arco$alpha,color=Arco$color,size=1)+ 
  geom_line(data=AA_mean,aes(group=group2),alpha=AA_mean$alpha2,color=AA_mean$color2,size=1.2) +
  geom_point(data=AA_mean,aes(group=group2),color=AA_mean$color2,size=2.5)+
  scale_y_continuous(limits = c(10,40),breaks = c(10,20,30,40)) + 
  scale_x_continuous(limits = c(0,25),breaks = c(0,5,10,15,20,25)) +
  theme_bw() + 
  labs(x="Day",y="Relative Tb arcophase(h)")+ 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

#####################  onset
ggplot(Arco,aes(x,y,group=group)) + 
  geom_rect(aes(xmin=7,xmax=18,ymin=36,ymax=37),fill="lightskyblue1",alpha=0.05) +
  geom_line(alpha=Arco$alpha,color=Arco$color,size=1)+ 
  geom_line(data=AA_mean,aes(group=group2),alpha=AA_mean$alpha2,color=AA_mean$color2,size=1) +
  geom_point(data=AA_mean,aes(group=group2),color=AA_mean$color2,size=2.5)+
  scale_y_continuous(limits = c(14,40),breaks = c(18,24,30,36),labels = c(12,18,0,6)) + 
  scale_x_continuous(limits = c(0,25),breaks = c(0,5,10,15,20,25)) +
  theme_bw() + 
  labs(x="Day",y="Relative Tb onset(h)")+ 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())
