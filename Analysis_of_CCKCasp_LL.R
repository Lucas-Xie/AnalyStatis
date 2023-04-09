library(ggplot2)

#######################################  fedora  ############################################

Pe <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/periodgram_CCKCasp_LL.txt",header = T)

u <- Pe[,3:22]
i <- c(14:20)     
j <- c(1:4)      
m <- c(5:8)         
n <- c(9:13)       
exp1 <- apply(u[,j],1,mean)
exp2 <- apply(u[,m],1,mean)
exp3 <- apply(u[,n],1,mean)
con <- apply(u[,i],1,mean)

exp1sem <- apply(u[,j],1,sd)/(length(j)^(1/2))
exp2sem <- apply(u[,m],1,sd)/(length(m)^(1/2))
exp3sem <- apply(u[,n],1,sd)/(length(1)^(1/2))
consem <- apply(u[,i],1,sd)/(length(i)^(1/2))

Y <- Pe$Y1
y <- rep(0,960)

##
a1 <- rep(c(1:960),5)
a2 <- gl(5,960,labels=c("Control","RS_CCKCasp","RNS_CCKCasp","NR_CCKCasp",""))
kk <- c(con,exp1,exp2,exp3,Y)
ll <- c(consem,exp1sem,exp2sem,exp3sem,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)

######  save  17:8
ggplot(period,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  scale_fill_manual(values=c("gray","lightpink1","plum2","wheat","black"))+
  scale_color_manual(values=c("black","red","purple","orange","black"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

##########################################################################################

cckcasp <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/CCKCasp_LL.txt",header = T)
LL <- cckcasp[14401:28800,2:23]
total_ll <- apply(LL,2,sum)/10

alpha <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/alpha_CCKCasp_LL.txt",header = T)

g <- c(rep("Control",13),rep("RNS_CCKCasp",7),rep("RS_CCKCasp",5))
ll <- data.frame(x=g,y=alpha[,2])
ll$x <- factor(ll$x,levels = c("Control","RNS_CCKCasp","RS_CCKCasp"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","purple","red")) +
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

########################################################################################################

cckcasp <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210505CCKCasp_LL.txt",header = T)

p <- rep(c(1:1440),6)
Emean <- function(x){tapply(x,p,mean)}
ld <- apply(cckcasp[1:8640,2:23],2,Emean)
LD <- apply(ld,2,sum)

ll <- apply(cckcasp[12241:20880,2:23],2,Emean)
LL <- apply(ll,2,sum)

AA <- cbind(LD,LL)
write.csv(AA,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210505CCKCasp_LL-counts.csv")




#################################################################################################################
period <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/FR_period_CCKCasp_LL.txt",header = T)

g <- c(rep("Control",13),rep("RNS_CCKCasp",7),rep("RS_CCKCasp",5))
ll <- data.frame(x=g,y=period[,2])
ll$x <- factor(ll$x,levels = c("Control","RNS_CCKCasp","RS_CCKCasp"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","purple","red")) +
  scale_y_continuous(limits=c(23.9,25.01))+
  labs(y="Free running period(h)")+ 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


#######################################################################################################################
###############################################    20211227                   ########################################################################

c5 <- read.csv("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/actogram/20211228/CCKCasp5_1-7.csv",
               header = T)
u <- c5[,2]
Y <- c5[,3]
y <- rep(0,960)

##
a1 <- rep(c(1:960),2)
a2 <- gl(2,960,labels=c("CCKCasp5",""))
kk <- c(u,Y)
ll <- c(y,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)

######  save  17:8
ggplot(period,aes(x,y))+
  #geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  #scale_fill_manual(values=c("gray","lightpink1","plum2","wheat","black"))+
  scale_color_manual(values=c("black","blue"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

c51 <- read.csv("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/actogram/20211228/CCKCasp5_7-13.csv",
               header = T)
u <- c51[,2]
Y <- c51[,3]
y <- rep(0,960)
##
a1 <- rep(c(1:960),2)
a2 <- gl(2,960,labels=c("CCKCasp5",""))
kk <- c(u,Y)
ll <- c(y,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)
######  save  17:8
ggplot(period,aes(x,y))+
  #geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  #scale_fill_manual(values=c("gray","lightpink1","plum2","wheat","black"))+
  scale_color_manual(values=c("red","blue"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


c2 <- read.csv("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/actogram/20211228/CCKCasp2_1-7.csv",
               header = T)
u <- c2[,2]
Y <- c2[,3]
y <- rep(0,960)

##
a1 <- rep(c(1:960),2)
a2 <- gl(2,960,labels=c("CCKCasp2",""))
kk <- c(u,Y)
ll <- c(y,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)

######  save  17:8
ggplot(period,aes(x,y))+
  #geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  #scale_fill_manual(values=c("gray","lightpink1","plum2","wheat","black"))+
  scale_color_manual(values=c("black","blue"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

c21 <- read.csv("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/actogram/20211228/CCKCasp2_7-13.csv",
               header = T)

u <- c21[,2]
Y <- c21[,3]
y <- rep(0,960)
##
a1 <- rep(c(1:960),2)
a2 <- gl(2,960,labels=c("CCKCasp2",""))
kk <- c(u,Y)
ll <- c(y,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)
######  save  17:8
ggplot(period,aes(x,y))+
  #geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.4))+ 
  geom_line(aes(color=group),size=0.6)+ 
  theme_bw()+ 
  #scale_fill_manual(values=c("gray","lightpink1","plum2","wheat","black"))+
  scale_color_manual(values=c("red","blue"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

###########
peak <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/split-peak-of CCKCasp.txt",header = T)

g <- c(rep("peakA",5),rep("peakB",5))
ll <- data.frame(x=g,y=c(peak[,2],peak[,3]))
ll$x <- factor(ll$x,levels = c("peakA","peakB"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits=c(22,25.6))+
  labs(y="Period(h)")+ 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))
###################
pe2 <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/split-peak-of CCKCasp.txt",header = T)
pe <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/FR_period_CCKCasp_LL.txt",header = T)
period <- c(pe[1:20,2],pe2[,4],pe2[,2],pe2[,3])

g <- c(rep("Control",13),rep("R_CCKCasp",7),rep("Unsplit",5),rep("peakA",5),rep("peakB",5))
ll <- data.frame(x=g,y=period)
ll$x <- factor(ll$x,levels = c("Control","R_CCKCasp","Unsplit","peakA","peakB"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","purple","lightpink","orange","green3")) +
  scale_y_continuous(limits=c(22,25.4))+
  labs(y="Period(h)")+ 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(ll[,1])
AA<- data.frame(Y=ll[,2],Type)
AA.aov <- aov(Y~Type, data=AA)
summary(AA.aov)




##############################################          DELL       ####################################################
pe2 <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/split-peak-of CCKCasp.txt",header = T)
pe <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2021/2021_05_05_CCKCasp_LL01/20210510_result/FR_period_CCKCasp_LL.txt",header = T)
period <- c(pe[1:20,2],pe2[,4],pe2[,2],pe2[,3])

g <- c(rep("Control",13),rep("R_CCKCasp",7),rep("Unsplit",5),rep("peakA",5),rep("peakB",5))
ll <- data.frame(x=g,y=period)
ll$x <- factor(ll$x,levels = c("Control","R_CCKCasp","Unsplit","peakA","peakB"),ordered = T) # order of x axis
ggplot(ll,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=5,width = 0.3) +
  scale_color_manual(values = c("black","purple","pink","orange","green3")) +
  scale_y_continuous(limits=c(22,25.3))+
  labs(y="Period(h)")+ 
  theme_bw() + 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

Type <- as.factor(ll[,1])
AA<- data.frame(Y=ll[,2],Type)
AA.aov <- aov(Y~Type, data=AA)
summary(AA.aov)




