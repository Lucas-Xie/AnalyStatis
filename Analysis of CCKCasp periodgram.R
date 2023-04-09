library(ggplot2)

#######################################  fedora  ############################################


########################################################################

Pe <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2021/2021_05_31_Statistics-Result_CCKCasp-20L4D/20L4D-DD_chisquare-period/chi-square-period.txt",header = T)

u <- Pe[,3:29]
i <- c(17:27)     ###########    control group
j <- c(1:13)      ###########    experiment 1 group
m <- c(14:16)      ###########    experiment 2 group
exp1 <- apply(u[,j],1,mean)
exp2 <- apply(u[,m],1,mean)
con <- apply(u[,i],1,mean)
exp1sem <- apply(u[,j],1,sd)/(length(j)^(1/2))
exp2sem <- apply(u[,m],1,sd)#/(length(m)^(1/2))
consem <- apply(u[,i],1,sd)/(length(i)^(1/2))

Y <- Pe$Y1
y <- rep(0,960)

##
a1 <- rep(c(1:960),4)
a2 <- gl(4,960,labels=c("Control","1CCKCasp","2CCKCasp",""))
kk <- c(con,exp1,exp2,Y)
ll <- c(consem,exp1sem,exp2sem,y)
period <- data.frame(x=a1,y=kk,se=ll,group=a2)

######  save  17:8
ggplot(period,aes(x,y,fill=group))+
  geom_ribbon(aes(x=x,ymin=y-se,ymax=y+se,fill=group,alpha=0.6))+ 
  geom_line(aes(color=group),size=0.8)+ 
  theme_bw()+ 
  scale_fill_manual(values=c("gray","lightpink1","wheat","black"))+
  scale_color_manual(values=c("black","red","orange","black"))+ 
  scale_x_continuous(breaks = c(0,240,480,720,960),labels = c(16,20,24,28,32))+ 
  labs(x="Period (h)",y="Qp")+ 
  theme(axis.title.x=element_text(size=15,hjust=0.5),axis.title.y=element_text(size=25,hjust=0.5),axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),axis.line=element_line(colour="black"))


#########################################################################################





#########################################################################################
