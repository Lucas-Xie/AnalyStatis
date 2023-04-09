library(ggplot2)

#######################################  fedora  ###############################
cck <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_12_21_CCKxAi32ChR2_DD_entrain05/20201119DD_CCKxAi32_entrain05.txt",header = T)

########################  OO  calculate onset &&&  offset
OO <- function(Y){
  x <- smooth.spline(Y)$y                        ####### smooth data
  R <- NA
  onset <- NA
  offset <- NA
  for(i in 10:30){                                #######  "32" means total calculate days
    m <- 1440*(i-1)
    for(j in 1:1320){                            ###### everyday  1440min  1min/bins  
      n <- j+m
      a <- n+119                                  ######  slide windows  120mins
      R[j] <- sum(x[n:a]>median(x))              ######  threshold   median or mean
    }
    S <- which(R==120)                           ######  over threshold time point
    onset[i] <- S[1]/60
    offset[i] <- (tail(S,1)/60)+2
  }
  c(onset,offset)
}

ooresult <- apply(cck[,3:18],2,OO)
write.csv(ooresult,"/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_12_21_CCKxAi32ChR2_DD_entrain05/CCKxAi32_onsetoffset.csv")


Tbprc <- read.delim("/home/xielucheng/Documents/DELL-G5_syncthing/Work_progress/2020/2020_12_21_CCKxAi32ChR2_DD_entrain05/20201221_result/Tb_PRC_exp(1-210)-con(211-301).txt",header = T)

a1 <- c(rep("CCKEYFP",91),rep("CCKChR2",210))
prc <- data.frame(CT=c(Tbprc[211:301,1],Tbprc[1:210,1]),phase_change=c(Tbprc[211:301,2],Tbprc[1:210,2]),group=a1)
prc$group <- factor(prc$group,levels = c("CCKEYFP","CCKChR2"),ordered = T)

ggplot(prc,aes(x=CT,y=phase_change)) +
  geom_point(aes(color=group)) +
  geom_smooth(aes(color=group)) +
  scale_color_manual(values = c("black","red")) +
  scale_y_continuous(limits = c(-1.2,1.5)) + 
  scale_x_continuous(limits = c(0,24),breaks = c(0,6,12,18,24)) +
  theme_bw() + 
  theme(axis.text.y=element_text(size=20),axis.text.x=element_text(size=20),legend.text=element_text(size=15),panel.grid.minor=element_blank(),panel.grid.major=element_blank())

#################################################################

con <- Tbprc[211:301,]
exp <- Tbprc[1:210,]

d1 <- Tbprc[211:301,1]
d2 <- Tbprc[211:301,2]
d <- data.frame(d1,d2)
CON <- NA
CON <- list()
for(i in 1:24){
  a <- i-1
  b <- i+1
  CON[[i]] <- d[d1>=a&d1<b,][,2]
}


e1 <- Tbprc[1:210,1]
e2 <- Tbprc[1:210,2]
e <- data.frame(e1,e2)
EXP <- NA
EXP <- list()
for(i in 1:24){
  a <- i-1
  b <- i
  EXP[[i]] <- e[e1>=a&e1<b,][,2]
}


cct22 <- c(CON[[23]],CON[[24]],CON[[1]],CON[[2]])
ect22 <- c(EXP[[23]],EXP[[24]],EXP[[1]],EXP[[2]])
cct2 <- c(CON[[3]],CON[[4]],CON[[5]],CON[[6]])
ect2 <- c(EXP[[3]],EXP[[4]],EXP[[5]],EXP[[6]])
cct6 <- c(CON[[7]],CON[[8]],CON[[9]],CON[[10]])
ect6 <- c(EXP[[7]],EXP[[8]],EXP[[9]],EXP[[10]])
cct10 <- c(CON[[11]],CON[[12]],CON[[13]],CON[[14]])
ect10 <- c(EXP[[11]],EXP[[12]],EXP[[13]],EXP[[14]])
cct14 <- c(CON[[15]],CON[[16]],CON[[17]],CON[[18]])
ect14 <- c(EXP[[15]],EXP[[16]],EXP[[17]],EXP[[18]])
cct18 <- c(CON[[19]],CON[[20]],CON[[21]],CON[[22]])
ect18 <- c(EXP[[19]],EXP[[20]],EXP[[21]],EXP[[22]])


t.test(cct2,ect2)
# p-value = 0.04595 *

t.test(cct18,ect18)
# p-value = 0.004797 **

t.test(cct22,ect22)
# p-value = 0.003945 **


##################################   CCK PRC   boxplot
######################################################## 
mm <- c(rep("a",25),rep("b",28),rep("c",20),rep("d",27),rep("e",28),rep("f",46),rep("g",61),rep("h",44),rep("i",19),rep("j",28),rep("k",25),rep("l",37))
nn <- c(cct2,ect2,cct6,ect6,cct10,ect10,cct14,ect14,cct18,ect18,cct22,ect22)
qq <- data.frame(x=mm,y=nn)
ggplot(qq,aes(x,y,color=x)) +
  geom_boxplot(fatten=4) +
  geom_jitter(shape=1,size=3,width=0.3) +
  scale_color_manual(values = c("black","red","black","red","black","red","black","red","black","red","black","red")) +
  scale_y_continuous(limits  = c(-1.1,1.5),breaks = c(-1,0,1)) + 
  theme_bw() + 
  theme(axis.text.y=element_text(size=15),axis.text.x=element_text(size=15),legend.text=element_text(size=15),panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#############################################################

#######################################################  DELL  #########################################

Tbprc <- read.delim("D:/CAS_Yanlab/Fedora_syncthing/Work_progress/2020/2020_12_21_CCKxAi32ChR2_DD_entrain05/20201221_result/Tb_PRC_exp(1-210)-con(211-301).txt",header = T)

con <- Tbprc[211:301,]
exp <- Tbprc[1:210,]

d1 <- Tbprc[211:301,1]
d2 <- Tbprc[211:301,2]
d <- data.frame(d1,d2)
CON <- NA
CON <- list()
for(i in 1:24){
  a <- i-1
  b <- i+1
  CON[[i]] <- d[d1>=a&d1<b,][,2]
}


e1 <- Tbprc[1:210,1]
e2 <- Tbprc[1:210,2]
e <- data.frame(e1,e2)
EXP <- NA
EXP <- list()
for(i in 1:24){
  a <- i-1
  b <- i
  EXP[[i]] <- e[e1>=a&e1<b,][,2]
}


