
library(ggplot2)
WT <- read.table("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/phase_change_CCKEYFP_PRC.txt",header = T)
ChR <- read.table("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/phase_change_CCKchR2_PRC.txt",header = T)

w <- dim(WT)[1]
q <- dim(ChR)[1]
a1 <- c(rep("CCKEYFP",w),rep("CCKChR2",q))
prc <- data.frame(CT=c(WT[,1],ChR[,1]),phase_change=c(WT[,2],ChR[,2]),group=a1)
prc$group <- factor(prc$group,levels = c("CCKEYFP","CCKChR2"),ordered = T)

ggplot(prc,aes(x=CT,y=phase_change)) +
  geom_point(aes(color=group)) +
  geom_smooth(aes(color=group)) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits = c(-1,1)) + 
  scale_x_continuous(limits = c(0,24),breaks = c(0,6,12,18,24)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())


opto <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/0901DD_optoTb-LMA.txt",header = T)

m <- c(9001:10440)
t1 <- opto$X52370_Tb[m]
t2 <- opto$X52899_Tb[m]
l1 <- opto$X52372_LMA[m]
l2 <- opto$X52899_LMA[m]
a1 <- c(1:1440)
#a2 <- gl(2,1440,labels=c("CCKChR2","CCKEYFP"))
T1 <- data.frame(x=a1,y=t1)
ggplot(T1,aes(x=x,y=y)) +
  geom_line(color="red",size=1.5) +
  scale_y_continuous(limits = c(35,39)) + 
  scale_x_continuous(limits = c(0,1440),breaks = c(0,780,840,1440)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

T2 <- data.frame(x=a1,y=t2)
ggplot(T2,aes(x=x,y=y)) +
  geom_line(color="black",size=1.5) +
  scale_y_continuous(limits = c(35,39)) + 
  scale_x_continuous(limits = c(0,1440),breaks = c(0,780,840,1440)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

LMA1 <- data.frame(x=a1,y=l1)
ggplot(LMA1,aes(x=x,y=y)) +
  geom_line(color="red",size=1.2) +
  scale_y_continuous(limits = c(0,0.5)) + 
  scale_x_continuous(limits = c(0,1440),breaks = c(0,780,840,1440)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

LMA2 <- data.frame(x=a1,y=l2)
ggplot(LMA2,aes(x=x,y=y)) +
  geom_line(color="black",size=1.2) +
  scale_y_continuous(limits = c(0,0.5)) + 
  scale_x_continuous(limits = c(0,1440),breaks = c(0,780,840,1440)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

#########  Change in temperature difference
CTb <- function(x){mean(x[45:60]-mean(x[1:15]))} 
CTB <- matrix(nrow =14,ncol=8)

for (i in 1:14){
  a <- (i-1)*1440
  m <- 1141+a
  n <- 1200+a
  A <- opto[m:n,3:10]
  CTB[i,1:8] <- apply(A,2,CTb)
}

write.csv(CTB,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/CCKChR2_Change_in_Tb.csv")

MCT <- apply(CTB,2,mean)
m1 <- c(rep("CCKChR2",5),rep("CCKEYFP",3))
CT <- data.frame(x=m1,y=MCT)
ggplot(CT,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_point(size=6,alpha=I(0.4)) +
  scale_color_manual(values = c("red","black")) +
  scale_y_continuous(limits  = c(-2,1)) + 
  theme_bw() + 
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.major=element_blank(),panel.grid.minor=element_blank())


###########################

SLMA <- matrix(nrow =14,ncol=8)

for (i in 1:14){
  a <- (i-1)*1440
  m <- 1141+a
  n <- 1200+a
  A <- opto[m:n,11:18]
  SLMA[i,1:8] <- apply(A,2,sum)
}

write.csv(SLMA,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/CCKChR2_SUM_LMA.csv")

MSL <- apply(SLMA[1:13,],2,mean)
m1 <- c(rep("CCKChR2",5),rep("CCKEYFP",3))
M <- data.frame(x=m1,y=MSL)
ggplot(M,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_point(size=6,alpha=I(0.4)) +
  scale_color_manual(values = c("red","black")) +
  scale_y_continuous(limits  = c(1,15)) + 
  theme_bw() + 
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.major=element_blank(),panel.grid.minor=element_blank())
##########################################################################################################
########################################      2021_10_11  


Tb <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/CCKchR2_Tb.txt",header = T)

n1 <- 17281
n2 <- 25920

oTb <- Tb[n1:n2,c(3:5,8:10)]

a <- rep(c(1:1440),6)
E_mean <- function(x){tapply(x,a,mean)}
mTb <- apply(oTb,2,E_mean)

exp <- mTb[1081:1260,1:3]
con <- mTb[1081:1260,4:6]
e <- apply(exp,1,mean)
c <- apply(con,1,mean)
expsem <- apply(exp,1,sd)
consem <- apply(con,1,sd)
a1 <- rep(c(1:180),2)
a2 <- gl(2,180)
kk <- c(e,c)
ll <- c(expsem,consem)
TB <- data.frame(x=a1,y=kk,se=ll,group=a2)
######  save  6:4

ggplot(TB,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.5)+ theme_bw()+ 
  scale_fill_manual(values=c("lightpink1", "gray"))+
  scale_color_manual(values=c("red","black"))+ 
  scale_y_continuous(limits=c(35,39))+ 
  scale_x_continuous(breaks = c(1,60,120,180))+ 
  labs(x="Time (h)",y="Body temperature")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
############################################  boxplt
OTb <- Tb[n1:n2,c(3:10)]
a <- rep(c(1:1440),3)
E_mean <- function(x){tapply(x,a,mean)}
M1Tb <- apply(OTb[1:4320,],2,E_mean)
M2Tb <- apply(OTb[4321:8640,],2,E_mean)

om1tb <- apply(M1Tb[1081:1260,],2,mean) 
om2tb <- apply(M2Tb[1081:1260,],2,mean) 

mm <- c(om1tb[1:5],om2tb[1:8],om1tb[7:8])
nn <- c(rep("CCKChR2",10),rep("Control",5))
qq <- data.frame(x=nn,y=mm)
qq$nn <- factor(qq$nn,levels = c("Control","CCKChR2"),ordered = T) # order of x axis

ggplot(qq,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=3.5,width=0.1) +
  scale_color_manual(values = c("red","black")) +
  scale_y_continuous(limits  = c(36.5,38.6),breaks = c(37,38)) + 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))



#############
LMA <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/CCKchR2_LMA.txt",header = T)
n1 <- 17281
n2 <- 25920

olma <- LMA[n1:n2,c(3:5,8:10)]

a <- rep(c(1:1440),6)
E_mean <- function(x){tapply(x,a,mean)}
mlma <- apply(olma,2,E_mean)

exp <- mlma[1081:1260,1:3]
con <- mlma[1081:1260,4:6]
e <- apply(exp,1,mean)
c <- apply(con,1,mean)
expsem <- apply(exp,1,sd)
consem <- apply(con,1,sd)
a1 <- rep(c(1:180),2)
a2 <- gl(2,180)
kk <- c(e,c)
ll <- c(expsem,consem)
lma <- data.frame(x=a1,y=kk,se=ll,group=a2)

ggplot(lma,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.5)+ theme_bw()+ 
  scale_fill_manual(values=c("lightpink1", "gray"))+
  scale_color_manual(values=c("red","black"))+ 
  scale_y_continuous(limits=c(-0.04,0.5))+ 
  scale_x_continuous(breaks = c(1,60,120,180))+ 
  labs(x="Time (h)",y="Locomotor activity")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
###################
OLMA <- LMA[n1:n2,c(3:10)]
a <- rep(c(1:1440),3)
E_mean <- function(x){tapply(x,a,mean)}
M1L <- apply(OLMA[1:4320,],2,E_mean)
M2L <- apply(OLMA[4321:8640,],2,E_mean)

om1l <- apply(M1L[1081:1260,],2,mean) 
om2l <- apply(M2L[1081:1260,],2,mean) 





#################################################################################
#################################################################################
############################### dell          ###################################
opto <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/0901DD_optoTb-LMA.txt",header = T)

########################################  wavelet analysis
library(WaveletComp)
a<- gl(14100,2)
A <- opto[1:28200,3:18]
M <- function(x){tapply(x,a,mean)}
P <- apply(A,2,M)

a1 <- data.frame(x=P[,7])

Tb1 <- analyze.wavelet(a1,"x",loess.span = 0,dt = 1/720,lowerPeriod = 0.5,upperPeriod = 1)
####   dt*bin_value === 1 days  default=1
wt.image(Tb1)

##############################################

ChR <- read.table("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2020/2020_09_20_CCkChR2_entrain02_3week/phase_change_CCKchR2_PRC.txt",header = T)

Tb <- opto[361:1800,3]
a <- c(1:1440)
A <- smooth.Pspline(a,Tb,spar = 500,method = 1)
B <- A$ysmth-mean(A$ysmth)

plot(a,Tb)
lines(a,A$ysmth)

B <- pmax(B,0)  ###########  
plot(a,B)
sum(B[600:700]>0)

D <- NA
for(i in 1:600){
  e <- i+99
  D[i] <- sum(B[i:e]>0)
}

S <- which(D==100)
s <-length(S)
onset <- S[1]/60
offset <- S[s]/60
alpha <- offset-onset

