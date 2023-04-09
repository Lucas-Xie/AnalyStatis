library(ggplot2)
library(DescTools)

#######################################  DELL ###############################
so <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2022/2022_11_06_Result_12L-20L-LL-CCKChR2-VIPChR2-so-PS/ALL_single_PS.txt",header = F)


################              VIP-CT14
g <- c(rep("Con-12L",4),rep("VIP-12L",5),rep("Con-20L",4),rep("VIP-20L",5),rep("Con-LL",4),rep("VIP-LL",3))
v14 <- c(1:4,10:18,24:32,38:40)
pt <- data.frame(x=g,y=c(so[v14,7]))
pt$x <- factor(pt$x,levels = c("Con-12L","VIP-12L","Con-20L","VIP-20L","Con-LL","VIP-LL"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","blue","black","blue","black","blue")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(-2,-1,0)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(c(rep("Con",4),rep("VIP",5),rep("Con",4),rep("VIP",5),rep("Con",4),rep("VIP",3)))
Time <- as.factor(c(rep("12L",9),rep("20L",9),rep("LL",7)))
AA<- data.frame(Y=pt[,2],Time,Type)
AA.aov <- aov(Y~Type+Time+Time:Type,data=AA)
summary(AA.aov)

pt <- data.frame(x=g,y=c(so[v14,8]))
pt$x <- factor(pt$x,levels = c("Con-12L","VIP-12L","Con-20L","VIP-20L","Con-LL","VIP-LL"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","blue","black","blue","black","blue")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(-2,-1,0)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of LMA")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(c(rep("Con",4),rep("VIP",5),rep("Con",3),rep("VIP",5),rep("Con",4),rep("VIP",2)))
Time <- as.factor(c(rep("12L",9),rep("20L",8),rep("LL",6)))
AA<- data.frame(Y=pt[,2],Time,Type)
AA.aov <- aov(Y~Type+Time+Time:Type,data=AA)
summary(AA.aov)


####################################################################################################
################              CCK-CT14
g <- c(rep("Con-12L",4),rep("CCK-12L",5),rep("Con-20L",4),rep("CCK-20L",5),rep("Con-LL",4),rep("CCK-LL",5))
c14 <- c(1:9,15:23,29:37)
pt <- data.frame(x=g,y=c(so[c14,7]))
pt$x <- factor(pt$x,levels = c("Con-12L","CCK-12L","Con-20L","CCK-20L","Con-LL","CCK-LL"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","black","red","black","red")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(-2,-1,0)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(c(rep("Con",4),rep("CCK",5),rep("Con",4),rep("CCK",5),rep("Con",4),rep("CCK",5)))
Time <- as.factor(c(rep("12L",9),rep("20L",9),rep("LL",9)))
AA<- data.frame(Y=pt[,2],Time,Type)
AA.aov <- aov(Y~Type+Time+Time:Type,data=AA)
summary(AA.aov)
######################
g <- c(rep("Con-12L",4),rep("CCK-12L",5),rep("Con-20L",4),rep("CCK-20L",5),rep("Con-LL",4),rep("CCK-LL",5))
c14 <- c(1:9,15:23,29:37)
pt <- data.frame(x=g,y=c(so[c14,8]))
pt$x <- factor(pt$x,levels = c("Con-12L","CCK-12L","Con-20L","CCK-20L","Con-LL","CCK-LL"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","black","red","black","red")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(-2,-1,0)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of LMA")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(c(rep("Con",4),rep("CCK",5),rep("Con",4),rep("CCK",5),rep("Con",4),rep("CCK",5)))
Time <- as.factor(c(rep("12L",9),rep("20L",9),rep("LL",9)))
AA<- data.frame(Y=pt[,2],Time,Type)
AA.aov <- aov(Y~Type+Time+Time:Type,data=AA)
summary(AA.aov)
##########################


###############################################################################################
################              20L-CCK-CT22
g <- c(rep("Con-20L",6),rep("CCK-20L",5))
c22 <- c(21:31)
pt <- data.frame(x=g,y=c(so[c22,3]))
pt$x <- factor(pt$x,levels = c("Con-20L","CCK-20L"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(-0.5,2.5),breaks = c(0,1,2)) + 
  labs(x="optogenetics_stimulation_CT22",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

t.test(pt[1:6,2],pt[7:11,2])

g <- c(rep("Con-20L",6),rep("CCK-20L",5))
c22 <- c(21:31)
pt <- data.frame(x=g,y=c(so[c22,4]))
pt$x <- factor(pt$x,levels = c("Con-20L","CCK-20L"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(-0.5,2.5),breaks = c(0,1,2)) + 
  labs(x="optogenetics_stimulation_CT22",y="Phase shift of LMA")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

t.test(pt[1:6,2],pt[7:11,2])

####################################################################################################
################              20L-CCK-VIP-CT14

g <- c(rep("Con-20L",4),rep("CCK-20L",5),rep("VIP-20L",5))
c22 <- c(15:28)
pt <- data.frame(x=g,y=c(so[c22,7]))
pt$x <- factor(pt$x,levels = c("Con-20L","CCK-20L","VIP-20L"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","blue")) +
  scale_y_continuous(limits  = c(-1,0.5),breaks = c(-1,-0.5,0,0.5)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(c(rep("Con",4),rep("CCK",5),rep("VIP",5)))
AA<- data.frame(Y=pt[,2],Time)
AA.aov <- aov(Y~Time,data=AA)
summary(AA.aov)


g <- c(rep("Con-20L",4),rep("CCK-20L",5),rep("VIP-20L",5))
c22 <- c(15:28)
pt <- data.frame(x=g,y=c(so[c22,8]))
pt$x <- factor(pt$x,levels = c("Con-20L","CCK-20L","VIP-20L"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","blue")) +
  scale_y_continuous(limits  = c(-1,0.5),breaks = c(-1,-0.5,0,0.5)) + 
  labs(x="optogenetics_stimulation_CT14",y="Phase shift of LMA")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


########################################         LL       ##########################################
g <- c(rep("Control",4),rep("CCKChR2",5),rep("VIPChR2",3))

pt <- data.frame(x=g,y=c(so[32:43,3]))
pt$x <- factor(pt$x,levels = c("Control","CCKChR2","VIPChR2"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","blue")) +
  scale_y_continuous(limits  = c(-0.5,2.5),breaks = c(0,1,2)) + 
  labs(x="optogenetics_stimulation_CT22",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),
        axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),
        panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

Type <- as.factor(pt[,1])
AA<- data.frame(Y=pt[,2],Type)
AA.aov <- aov(Y~Type, data=AA)
summary(AA.aov)

PostHocTest(AA.aov,method = "lsd")


g <- c(rep("Control",4),rep("CCKChR2",5),rep("VIPChR2",3))

pt <- data.frame(x=g,y=c(so[32:43,4]))
pt$x <- factor(pt$x,levels = c("Control","CCKChR2","VIPChR2"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red","blue")) +
  scale_y_continuous(limits  = c(-0.5,2.5),breaks = c(0,1,2)) + 
  labs(x="optogenetics_stimulation_CT22",y="Phase shift of LMA")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),
        axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),
        panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

Type <- as.factor(pt[,1])
AA<- data.frame(Y=pt[,2],Type)
AA.aov <- aov(Y~Type, data=AA)
summary(AA.aov)

PostHocTest(AA.aov,method = "lsd")
