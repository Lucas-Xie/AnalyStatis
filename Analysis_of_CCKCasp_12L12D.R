library(ggplot2)


#######################################  fedora  ###############################

########################## relative activity 
A <- NA
B <- NA
RA <- function(x){
  for(i in 1:811){        ############    811    10hours
    n <- i+599
    A[i] <- sum(x[i:n])
  }
  a <- max(A)
  for(i in 1:1111){       ############ 1111   5hours
    n <- i+299
    B[i] <- sum(x[i:n])
  }
  b <- min(B)
  ra <- (a-b)/(a+b)
  ra
}

cckcasp <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_01_20_CCKCasp-20L-4D-02/20210120_CCKcasp-20L-4D.txt",header = T)

a_12L <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_01_26_CCKCasp_deletion_Result/CCKCasp_12L12D-DD_result/alpha_12L_CCKcasp.txt",header = T) 
a_20L <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_01_26_CCKCasp_deletion_Result/CCKCasp_20L4D-DD_result/alpha_20L_CCKcasp.txt",header = T) 


##############################################
u <- a_12L[,2:22]
i <- c(15:21)     ###########    control group
j <- c(1:14)      ###########    experiment group
exp <- apply(u[,j],1,mean)
con <- apply(u[,i],1,mean)
expsem <- apply(u[,j],1,sd)/(length(j)^(1/2))
consem <- apply(u[,i],1,sd)/(length(i)^(1/2))
#######################
e12 <- exp
c12 <- con
esm12 <- expsem
csm12 <- consem

##############################################
u <- a_20L[,2:17]
i <- c(1:7)     ###########    control group
j <- c(8:16)      ###########    experiment group
exp <- apply(u[,j],1,mean)
con <- apply(u[,i],1,mean)
expsem <- apply(u[,j],1,sd)/(length(j)^(1/2))
consem <- apply(u[,i],1,sd)/(length(i)^(1/2))
#######################
e20 <- exp
c20 <- con
esm20 <- expsem
csm20 <- consem

a1 <- rep(c(1:14),4)
a2 <- gl(4,14,labels=c("CCKtaCasp3_12L","CCKEYFP_12L","CCKtaCasp3_20L","CCKEYFP_20L"))
kk <- c(e12,c12,e20,c20)
ll <- c(esm12,csm12,esm20,csm20)
J <- data.frame(x=a1,y=kk,se=ll,group=a2)

ggplot(J,aes(x,y)) + 
  geom_pointrange(aes(x=x,ymin=y-se,ymax=y+se,color=group),size=1) + 
  geom_line(aes(color=group),size=1)+ 
  scale_color_manual(values = c("blue","gray","red","black")) +
  scale_x_continuous(limits=c(1,14),breaks = c(1:14),labels = c(-6:7))+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

################### 12L
a1 <- rep(c(1:14),2)
a2 <- gl(2,14,labels=c("CCKtaCasp3_12L","CCKEYFP_12L"))
kk <- c(e12,c12)
ll <- c(esm12,csm12)
K <- data.frame(x=a1,y=kk,se=ll,group=a2)

ggplot(K,aes(x,y)) + 
  geom_pointrange(aes(x=x,ymin=y-se,ymax=y+se,color=group),size=1) + 
  geom_line(aes(color=group),size=1)+ 
  scale_color_manual(values = c("red","black")) +
  scale_x_continuous(breaks = c(1:14),labels = c(-6:7))+
  scale_y_continuous(limits=c(11,13),breaks = c(11,12,13))+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

#######################################################################################################################################
##########################################          20211105                       #############################################################################################
#######################################################################################################################################

p <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_01_26_CCKCasp_deletion_Result/20211105_CCKcasp-12LDD-20LDD.txt",header = T,
                na.strings = "NA")

g <- c(rep("Control-12L",7),rep("CCKCasp-12L",18),rep("Control-20L",13),rep("CCKCasp-20L",20))
ll <- data.frame(x=g,y=c(p[25:1,4],p[33:1,2]))
ll$x <- factor(ll$x,levels = c("Control-12L","CCKCasp-12L","Control-20L","CCKCasp-20L"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","red","black","red")) +
  scale_y_continuous(limits=c(22.8,24))+
  labs(y="Period (h)")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


Data <- ll[,2]
photo <- as.factor(c(rep("12L",25),rep("20L",33)))                   
group <- as.factor(c(rep("con",7),rep("CCKCasp",18),rep("con",13),rep("CCKCasp",20)))        
TC <- data.frame(Y=Data,photo,group)
TC.aov <- aov(Y~photo+group+photo:group, data=TC)
summary(TC.aov)
R <- TukeyHSD(TC.aov)

#######################################################################################################################################
############################################              DELL             ############################################################

p <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2021/2021_01_26_CCKCasp_deletion_Result/20211105_CCKcasp-12LDD-20LDD.txt",header = T,
                na.strings = "NA")

g <- c(rep("Control-12L",7),rep("CCKCasp-12L",18),rep("Control-20L",13),rep("CCKCasp-20L",20))
ll <- data.frame(x=g,y=c(p[25:1,4],p[33:1,2]))
ll$x <- factor(ll$x,levels = c("Control-12L","CCKCasp-12L","Control-20L","CCKCasp-20L"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","red","black","red")) +
  scale_y_continuous(limits=c(22.8,24))+
  labs(y="Period (h)")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


Data <- ll[,2]
photo <- c(rep(1,25),rep(2,33))                   
Celltype <- c(rep(3,7),rep(4,18),rep(3,13),rep(4,20))        
TC <- data.frame(Y=Data,Q=photo,W=Celltype)
TC.aov <- aov(Y~as.factor(Q)+as.factor(W)+as.factor(Q):as.factor(W), data=TC)
summary(TC.aov)

TukeyHSD(TC.aov,"Q",conf.level = 0.95)
t.test(TC[1:25,1],TC[25:58,1])
mean(TC[1:25,1])
mean(TC[25:58,1])

TukeyHSD(TC.aov)

ScheffeTest(TC.aov)

