library(ggplot2)

#######################################      fedora       ############################################
tent1 <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_06_30_CCKtent_statistics_Result/20210408_ccktent.txt",header = T)

tent2 <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_06_30_CCKtent_statistics_Result/20210627_ccktent.txt",header = T)
#####################################
m <- rep(c(1:1440),7)
Emean <- function(x){tapply(x,m,mean)}
w = 1440*30+1
e = 1440*37
T2 <- apply(tent2[w:e,2:13],2,Emean)
LD <- apply(T2,2,sum)

n <- rep(c(1:1440),5)
Dmean <- function(x){tapply(x,n,mean)}
o <- 1440*38+721
p <- 1440*43+720
dd <- apply(tent2[o:p,2:13],2,Dmean)
DD <- apply(dd,2,sum)

AA <- cbind(LD,DD)
write.csv(AA,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_06_30_CCKtent_statistics_Result/20210627_ccktent-counts.csv")

#######################################   20L4D    7 days 
a = 1440*25+1
b = 1440*32

m <- rep(c(1:1440),7)
Emean <- function(x){tapply(x,m,mean)}
T1 <- apply(tent1[a:b,3:11],2,Emean)

w = 1440*30+1
e = 1440*37
T2 <- apply(tent2[w:e,2:13],2,Emean)

Cck <- cbind(T1,T2)

ccktent <- apply(Cck[,c(8,9,18:21)],1,mean)
control <- apply(Cck[,c(1,2,4,5,10,14,17)],1,mean)

ccktents <- apply(Cck[,c(18:21)],1,sd)/3
controls <- apply(Cck[,c(1:6,10:17)],1,sd)/3

a1 <- rep(c(1:1440),2)
a2 <- gl(2,1440,labels=c("Control","CCKtent"))
kk <- c(control,ccktent)
ll <- c(controls,ccktents)
lma <- data.frame(x=a1,y=kk,se=ll,group=a2)
lma$group <- factor(lma$group,levels = c("Control","CCKtent"),ordered = T) 
##############################  save  6:4
ggplot(lma,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.5)+ theme_bw()+ 
  scale_fill_manual(values=c("gray","lightpink1"))+
  scale_color_manual(values=c("black","red"))+ 
  scale_y_continuous(limits=c(-10,120))+ 
  scale_x_continuous(breaks = c(0,360,720,1080,1440),labels=c("ZT0","ZT6","ZT12","ZT18","ZT24")) +
  labs(x="Circadian time",y="Locomotor activity(counts)") + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

########################################################################################################################

per <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_06_30_CCKtent_statistics_Result/20210701_CCKtent_period.txt",header = T)

g <- c(rep("Control",7),rep("CCKtent",7))
pe <- data.frame(x=g,y=c(per[17:23,2],per[10:16,2]))
pe$x <- factor(pe$x,levels = c("Control","CCKtent"),ordered = T) # order of x axis
ggplot(pe,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=4,width = 0.1) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits  = c(22.9,24.3),breaks = c(23,23.5,24)) + 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=15,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

############################################# #################################################
########################################### two way ANOVA  ####################################
u <- Cck[,c(1,2,4,5,10,14,17,8,9,18:21)] 
y <- gl(4,30)
Y_mean <- function(x){tapply(x,y,mean)}
AA <- apply(u[841:960,],2,Y_mean)##########################  ZT10--ZT12
TWA <- melt(AA)
Time <- as.factor(TWA[,1])
Type  <- as.factor(c(rep("CCKTeNT",4*6),rep("Control",4*7)))
CCKcasp20L <- data.frame(Y=TWA[,3],Time,Type)
C.aov <- aov(Y~Time+Type+Time:Type, data=CCKcasp20L)
summary(C.aov)

#   Df Sum Sq Mean Sq F value Pr(>F)  
#  Time         3   2714   904.6   3.505 0.0230 *
#  Type         1   1537  1537.2   5.955 0.0188 *
#  Time:Type    3    616   205.5   0.796 0.5028  
#  Residuals   44  11357   258.1                 

y <- gl(8,30)
Y_mean <- function(x){tapply(x,y,mean)}
BB <- apply(u[1:240,],2,Y_mean)##########################  ZT20--ZT2
TWA <- melt(BB)
Time <- as.factor(TWA[,1])
Type  <- as.factor(c(rep("CCKTeNT",8*6),rep("Control",8*7)))
CCKcasp20L <- data.frame(Y=TWA[,3],Time,Type)
B.aov <- aov(Y~Time+Type+Time:Type, data=CCKcasp20L)
summary(B.aov)

#Df Sum Sq Mean Sq F value   Pr(>F)    
#Time         7  163.4    23.3   0.851 0.548473    
#Type         1  381.2   381.2158  13.898 0.000341 ***
#  Time:Type    7   62.3     8.9   0.325 0.940910    
#Residuals   88 2413.4    27.4


##########################################################
y <- gl(8,30)
Y_mean <- function(x){tapply(x,y,mean)}
QQ <- apply(u[961:1200,],2,Y_mean)##########################  ZT12--ZT16
TWA <- melt(QQ)
Time <- as.factor(TWA[,1])
Type  <- as.factor(c(rep("CCKTeNT",8*6),rep("Control",8*7)))
CCKcasp20L <- data.frame(Y=TWA[,3],Time,Type)
Q.aov <- aov(Y~Time+Type+Time:Type, data=CCKcasp20L)
summary(Q.aov)
#  Df Sum Sq Mean Sq F value   Pr(>F)    
# Time         7   2507     358   0.445    0.871    
# Type         1  36090   36090  44.879 1.91e-09 ***
# Time:Type    7    961     137   0.171    0.990


