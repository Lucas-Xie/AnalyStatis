
library(ggplot2)
########################################################  fedora
jetlag <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_23_CCKhm4D_Jetlag/CCKhm4D_Activity_onset_Jetlag.txt",header = T,row.names = 1)

hC_mean <- apply(jetlag[,1:12],1,mean)
hS_mean <- apply(jetlag[,13:17],1,mean)
eC_mean <- apply(jetlag[,18:23],1,mean)
hC_sem <- apply(jetlag[,1:12],1,sd)/(12^(1/2))
hS_sem <- apply(jetlag[,13:17],1,sd)/(5^(1/2))
eC_sem <- apply(jetlag[,18:23],1,sd)/(6^(1/2))

a <- dim(jetlag)[1]
a1 <- rep(c(1:a),3)
a2 <- gl(3,a,labels=c("CCKhm4D_CNO","CCKhm4D_saline","CCKEYFP_CNO"))
kk <- c(hC_mean,hS_mean,eC_mean)
ll <- c(hC_sem,hS_sem,eC_sem)
Jet <- data.frame(x=a1,y=kk,se=ll,group=a2)

ggplot(Jet,aes(x,y)) + 
  geom_pointrange(aes(x=x,ymin=y-se,ymax=y+se,color=group),size=1) + 
  geom_line(aes(color=group),size=1.5)+ theme_bw() + 
  scale_color_manual(values = c("red","blue","black")) +
  scale_y_continuous(breaks = c(-2:14)) + 
  scale_x_continuous(breaks = c(1:19)) +
  theme(axis.title.x=element_text(size=25,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

##PS50
ps50 <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_09_23_CCKhm4D_Jetlag/PS50.txt",header = T)
##
ps50 <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2020/2020_09_23_CCKhm4D_Jetlag/PS50.txt",header = T)

g <- c(rep("CCKhm4D_CNO",12),rep("CCKhm4D_saline",5),rep("CCKEYFP_CNO",6))
PS50 <- data.frame(x=ps50$FJ_PS50,y=ps50$SJ_PS50,group=g)

ggplot(PS50,aes(x,y)) +
  geom_point(aes(color=group),size=2.5) +
  geom_smooth(method = "lm",color="green",fill="lightgreen") +
  scale_color_manual(values = c("black","red","blue")) +
  scale_y_continuous(limits  = c(1,5)) + 
  scale_x_continuous(limits  = c(1,5)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.major=element_blank(),panel.grid.minor=element_blank())

cor.test(PS50$x,PS50$y)


fj <- data.frame(x=g,y=ps50$FJ_PS50)
fj$x <- factor(fj$x,levels = c("CCKhm4D_CNO","CCKhm4D_saline","CCKEYFP_CNO"),ordered = T) # order of x axis
ggplot(fj,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=6,width = 0.3) +
  scale_color_manual(values = c("red","blue","black")) +
  scale_y_continuous(limits  = c(1,5)) + 
  theme_bw() + 
  theme(axis.title.x=element_text(size=25,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

sj <- data.frame(x=g,y=ps50$SJ_PS50)
sj$x <- factor(sj$x,levels = c("CCKhm4D_CNO","CCKhm4D_saline","CCKEYFP_CNO"),ordered = T)
ggplot(sj,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=6,width = 0.3) +
  scale_color_manual(values = c("red","blue","black")) +
  scale_y_continuous(limits  = c(1,5)) + 
  theme_bw() + 
  theme(axis.title.x=element_text(size=25,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))

#######################################     DELL      #############################################

jetlag <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2020/2020_09_23_CCKhm4D_Jetlag/CCKhm4D_Activity_onset_Jetlag.txt",header = T,row.names = 1)

hC_mean <- apply(jetlag[,1:12],1,mean)
hS_mean <- apply(jetlag[,13:17],1,mean)
eC_mean <- apply(jetlag[,18:23],1,mean)
hC_sem <- apply(jetlag[,1:12],1,sd)/(12^(1/2))
hS_sem <- apply(jetlag[,13:17],1,sd)/(5^(1/2))
eC_sem <- apply(jetlag[,18:23],1,sd)/(6^(1/2))

a <- dim(jetlag)[1]
a1 <- rep(c(1:a),3)
a2 <- gl(3,a,labels=c("CCKhm4D_CNO","CCKhm4D_saline","CCKEYFP_CNO"))
kk <- c(hC_mean,hS_mean,eC_mean)
ll <- c(hC_sem,hS_sem,eC_sem)
Jet <- data.frame(x=a1,y=kk,se=ll,group=a2)

ggplot(Jet,aes(x,y)) + 
  geom_pointrange(aes(x=x,ymin=y-se,ymax=y+se,color=group),size=0.2) + 
  geom_line(aes(color=group),size=0.8)+ theme_bw() + 
  scale_color_manual(values = c("red","blue","black")) +
  scale_y_continuous(breaks = c(-2:14)) + 
  scale_x_continuous(breaks = c(1:19)) +
  theme(axis.title.x=element_text(size=25,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())




