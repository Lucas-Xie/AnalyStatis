library(ggplot2)

#######################################      fedora       ############################################
cck <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_12_22_CCKChR2-CT14opto+LP-002/CCKChR2-CT14opto+LP_PS.txt",
                 header = T)
###########################################
Y <- c(cck[,2],cck[,3])
al <- rep(0.8,28)
g1 <- c(rep("Control",6),rep("CCKChR2",8),rep("Control",6),rep("CCKChR2",8))
g2 <- gl(2,14,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:14,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
pt$group <- factor(pt$group,levels = c("Control","CCKChR2"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("black","red"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of Tb (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
########################################################
A <- cck[,2]
g <- c(rep("Control",6),rep("CCKChR2",8))

ft <- data.frame(x=g,y=A)
ft$x <- factor(ft$x,levels = c("Control","CCKChR2"),ordered = T) # order of x axis
ggplot(ft,aes(x,y,color=x)) +
  geom_boxplot(fatten=2) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  labs(x=" ",y="Phase shift of Tb (h)")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))




#######################     paired-T        ####################
Y <- c(cck[7:14,2],cck[7:14,3])
al <- rep(0.6,16)
g1 <- c(rep("CCKChR2",8),rep("CCKChR2",8))
g2 <- gl(2,8,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:8,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("red"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of Tb (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
######
Y <- c(cck[1:6,2],cck[1:6,3])
al <- rep(0.6,12)
g1 <- c(rep("Control",6),rep("Control",6))
g2 <- gl(2,6,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:6,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("black"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of Tb (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
###############
A <- cck[,4]
g <- c(rep("Control",6),rep("CCKChR2",8))

ft <- data.frame(x=g,y=A)
ft$x <- factor(ft$x,levels = c("Control","CCKChR2"),ordered = T) # order of x axis
ggplot(ft,aes(x,y,color=x)) +
  geom_boxplot(fatten=2) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(23,24.2),breaks = c(23,24)) + 
  labs(x=" ",y="Period of Tb (h)")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


##################################################################
#############################################         LMA          ####################################################################

Y <- c(cck[,5],cck[,6])
al <- rep(0.8,28)
g1 <- c(rep("Control",6),rep("CCKChR2",8),rep("Control",6),rep("CCKChR2",8))
g2 <- gl(2,14,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:14,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
pt$group <- factor(pt$group,levels = c("Control","CCKChR2"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("black","red"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of LMA (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

#######################     paired-T        ####################
Y <- c(cck[7:14,5],cck[7:14,6])
al <- rep(0.6,16)
g1 <- c(rep("CCKChR2",8),rep("CCKChR2",8))
g2 <- gl(2,8,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:8,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("red"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of LMA (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
######
Y <- c(cck[1:6,5],cck[1:6,6])
al <- rep(0.6,12)
g1 <- c(rep("Control",6),rep("Control",6))
g2 <- gl(2,6,labels=c("CT14opto+LP","CT14LP"))
g3 <- factor(rep(1:6,2))

pt <- data.frame(x=g2,y=Y,group=g1,alp=al,line=g3)
pt$x <- factor(pt$x,levels = c("CT14opto+LP","CT14LP"),ordered = T) # order of x axis
ggplot(pt,aes(x,y,color=group)) + 
  geom_line(aes(group=line),alpha=pt$alp,size=0.8)+ 
  geom_point(shape=1,size=3) +
  scale_color_manual(values=c("black"))+
  scale_y_continuous(limits = c(-2.1,0.3),breaks = c(-2,-1,0)) + 
  theme_bw() + 
  labs(x=" ",y="Phase shift of LMA (h)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
###############
A <- cck[,7]
g <- c(rep("Control",6),rep("CCKChR2",8))

ft <- data.frame(x=g,y=A)
ft$x <- factor(ft$x,levels = c("Control","CCKChR2"),ordered = T) # order of x axis
ggplot(ft,aes(x,y,color=x)) +
  geom_boxplot(fatten=2) +
  geom_jitter(shape=1,size=5,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(23,24.2),breaks = c(23,24)) + 
  labs(x=" ",y="Period of Tb (h)")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

#######################################################################################################################################
###########################              20220401                 #####################################################################

im <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2021/2021_12_22_CCKChR2-CT14opto+LP-002/20220402/CCKChR2-CT14-opto+LP.txt"
                 ,header = T)

g <- c(rep("Control",6),rep("CCKChR2",8))
g1 <- c(g,g,g)
g2 <- c(rep("Steady",14),rep("immediate-onset",14),rep("immediate-offset",14))

pt <- data.frame(x=g2,y=c(im[,2],im[,3],im[,4]),group=g1)
pt$group <- factor(pt$group,levels = c("Control","CCKChR2"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=group)) +
  geom_boxplot(fatten=4) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(0,-2)) + 
  labs(x="BL+LP at CT14",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),
        axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),
        panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))

################################
g <- c(rep("Control",6),rep("CCKChR2",8))
g1 <- c(g,g,g)
g2 <- c(rep("Steady",14),rep("immediate-onset",14),rep("immediate-offset",14))

pt <- data.frame(x=g2,y=c(im[,5],im[,6],im[,7]),group=g1)
pt$group <- factor(pt$group,levels = c("Control","CCKChR2"),ordered = T) # order of x axis

ggplot(pt,aes(x,y,color=group)) +
  geom_boxplot(fatten=4) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(-2.5,0.5),breaks = c(0,-2)) + 
  labs(x="LP at CT14",y="Phase shift of Tb")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),
        axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),
        panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        axis.line=element_line(colour="black"))




