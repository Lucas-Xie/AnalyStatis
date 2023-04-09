library(ggplot2)
library(reshape2)
#######################################  fedora  ############################################

##########################################################################################################################################
#################################################         20211109              #########################################################################################
##########################################################################################################################################
#
cckcasp <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_04_08_CCKCasp-20L4D-DD_04/20210408_CCKCasp-20L-DD.txt",header = T)
a <- 1440*26+1
b <- 33*1440   #################  a:b   4th week 
cck4th <- cckcasp[a:b,3:21]
e <- rep(1:1440,7)
E_mean <- function(x){tapply(x,e,mean)}
Cck <- apply(cck4th,2,E_mean)
LD <- apply(Cck,2,sum)

CPD <- function(x){
  p <- rep(c(1:1440),9)
  sum(tapply(x,p,mean))
}
m <- 1440*35+1
n <- 1440*44
DD <- apply(cckcasp[m:n,3:21],2,CPD)

AA <- cbind(LD,DD)
write.csv(AA,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_04_08_CCKCasp-20L4D-DD_04/20210408_CCKCasp-20L-DD-counts.csv")

##############################################################
u <- Cck
i <- c(14:19)     ###########    control group
j <- c(1:13)      ###########    experiment  group
exp1 <- apply(u[,j],1,mean)
con <- apply(u[,i],1,mean)

exp1sem <- apply(u[,j],1,sd)/(length(j)^(1/2))
consem <- apply(u[,i],1,sd)/(length(i)^(1/2))

a1 <- rep(c(1:1440),2)
a2 <- gl(2,1440,labels=c("Control","CCK-Casp3"))
kk <- c(con,exp1)
ll <- c(consem,exp1sem)
CCK <- data.frame(x=a1,y=kk,se=ll,group=a2)
CCK$group <- factor(CCK$group,levels = c("Control","CCK-Casp3"),ordered = T) # order of x axis

ggplot(CCK,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  scale_fill_manual(values=c("gray","lightpink1"))+
  scale_color_manual(values=c("black","red"))+ 
  scale_y_continuous(limits=c(-6,120))+
  scale_x_continuous(breaks = c(0,360,720,1080,1440),labels = c("ZT20","ZT2","ZT8","ZT14","ZT20"))+ 
  labs(x="Circadian time (h)",y="locomotor activity (counts)")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


################# two way ANOVA  #########################
t <- c(j,i)  
y <- gl(4,30)
Y_mean <- function(x){tapply(x,y,mean)}
AA <- apply(u[841:960,t],2,Y_mean)##########################  ZT10--ZT12
TWA <- melt(AA)
Time <- as.factor(TWA[,1])
Type  <- as.factor(c(rep("CCK-Casp3",4*11),rep("Control",4*6)))
CCKcasp20L <- data.frame(Y=TWA[,3],Time,Type)
C.aov <- aov(Y~Time+Type+Time:Type, data=CCKcasp20L)
summary(C.aov)








