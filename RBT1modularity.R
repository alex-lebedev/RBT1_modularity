# PJ: REBOOT-I
# LTD data analysis:
# By Alexander V. Lebedev (2017-10-06)

##################
# I. Preparation #
##################

# Clear workspace:
rm(list=ls())

# Load necessary libraries:
library(ggplot2)
library(nlme)
library(RColorBrewer)

# Scaling function:
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


# Generate nice palette:
color <- brewer.pal(n = 8, "Dark2")

# Load the data
load('/Users/alebedev/Dropbox/REBOOT1/RBTI-modularity-data.rda')

# Generate performance clasees based on median splits (for plotting)
ddd$NBclass <- rep(ddd$TOTperf [ddd$task=='nb' & ddd$visit==1 & ddd$L==1]>median(ddd$TOTperf [ddd$task=='nb' & ddd$visit==1 & ddd$L==1]),12)
ddd$Rclass <- rep(ddd$TOTperf [ddd$task=='r' & ddd$visit==1 & ddd$L==1]>median(ddd$TOTperf [ddd$task=='r' & ddd$visit==1 & ddd$L==1]),12)

# Select subsets for specific analyses and plotting
dddSnb <- subset(ddd, ddd$task=='nb' & ddd$visit ==1)
dddSr <- subset(ddd, ddd$task=='r' & ddd$visit ==1)
dddV1c <- subset(ddd, ddd$visit ==1)
dddV2c <- subset(ddd, ddd$visit ==2)
dddSnbV12 <- subset(ddd, ddd$task=='nb')
dddSrV12 <- subset(ddd, ddd$task=='r')

# Difference with fixation blocks:
t.test(Qr_fix_v1,
       apply(cbind(subset(dddSr$Qcor, dddSr$L==1),
                   subset(dddSr$Qcor, dddSr$L==2),
                   subset(dddSr$Qcor, dddSr$L==3)),1,mean), paired=T)

t.test(Qnb_fix_v1,
       apply(cbind(subset(dddSnb$Qcor, dddSnb$L==1),
                   subset(dddSnb$Qcor, dddSnb$L==2),
                   subset(dddSnb$Qcor, dddSnb$L==3)),1,mean), paired=T)
################################
# II. Estimate final contrasts #
################################

# BL: L, Load x Task, Task x Load x TOTperformance
modME <- lme(Qcor~L*task*TOTperf,data=dddV1c, random=~1|subj)
summary(modME)

# Group x visit:
modME <- lme(Qcor~gr*visit,data=ddd, random=~1|subj)
summary(modME)

# Task x group x visit:
modME <- lme(Qcor~L*task*gr*visit,data=ddd, random=~1|subj)
summary(modME)

#  Group x visit: > in reasoning:
modME <- lme(Qcor~L*gr*visit,data=dddSrV12, random=~1|subj)
summary(modME)

# in separate groups:
ddd_act<- subset(ddd, ddd$gr=='1.00')
ddd_con<- subset(ddd, ddd$gr=='0.00')

modME <- lme(Qcor~L*task*visit,data=ddd_act, random=~1|subj)
summary(modME)

#############
# III. PLOT #
#############

# III.1 Performance x Moduarity:
par(mfrow=c(2,2),mar=c(2, 4, 4, 6))
rlab <- c('A', 'B', 'C')

for (i in 1:3) {
  l=i
  totperf <- range01(dddSnb$TOTperf[dddSnb$L==l])
  mod <- dddSnb$Qcor[dddSnb$L==l]
  plot(totperf, mod, col=1, pch=16,cex=2,ylim=c(0.01,0.29),cex.axis=2,
       xlab='Acc',ylab='Modularity', main=paste(i,'-Back / Reasoning-', rlab[i], sep=''))
  abline(glm(mod~totperf), col='black', lty=1, lwd=4)
  cor(totperf, mod)
  totperf <- range01(dddSr$TOTperf[dddSr$L==l])
  mod <- dddSr$Qcor[dddSr$L==l]
  points(totperf, mod, col='black', pch=2,cex=2,ylim=c(0.01,0.29))
  abline(glm(mod~totperf), col='gray31', lty=3, lwd=4)
  cor(totperf, mod)}

totperf <- range01(dddSnb$TOTperf[dddSnb$L==l])
mod <- dddSnb$Qcor[dddSnb$L==l]
plot(totperf, mod, col=1, pch=16,type='n', cex=4,ylim=c(0.01,0.29),cex.axis=2,
     xlab='',ylab='', yaxt='n',xaxt='n')
legend(-0.01, 0.2, c('N-back', 'Reasoning'), col = c('Black', 'gray31'),
       text.col = "black", lty = c(1, 2), pch = c(16,2), cex=1.5,
       merge = TRUE, bg = "white", border = 'white', bty = 'n')



# III.2 Change in performance :

  # III.2a [performance-change] N-back:


l=3 # do for each l {1, 2, 3}
dat <- as.data.frame(subset(ddd[,c('subj', 'perf', 'visit', 'gr')], 
                            ddd$task=='nb' & ddd$L==l))
dat$subj <- as.factor(dat$subj)
dat$gr <- as.factor(dat$gr)
dat$gr1[dat$gr=='0.00'] <- 'con'
dat$gr1[dat$gr=='1.00'] <- 'act'
dat$gr1 <- as.factor(dat$gr1)
dat$visit <- as.factor(dat$visit)


df <- aggregate(perf~visit+gr1, data=dat, mean)
df$se <- aggregate(perf~visit+gr1, data=dat, sd)[,3]/as.numeric(c(sqrt(27),sqrt(27),sqrt(25),sqrt(25)))
gp <- ggplot(data=df, aes(x=visit, y=perf, group=gr1))

gp + geom_line(aes(linetype=gr1), size=3)+theme_bw() +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=0.1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  geom_point(aes(shape=gr1), size=6) +
  geom_errorbar(aes(ymax=perf+se, ymin=perf-se),width=.1) +
  scale_y_continuous(limits = c(0.5, 1))


  # III.2b [performance-change] Reasoning:
l=3 # do for each l {1, 2, 3}
dat <- as.data.frame(subset(ddd[,c('subj', 'perf', 'visit', 'gr')], 
                            ddd$task=='r' & ddd$L==l))
dat$subj <- as.factor(dat$subj)
dat$gr <- as.factor(dat$gr)
dat$gr1[dat$gr=='0.00'] <- 'con'
dat$gr1[dat$gr=='1.00'] <- 'act'
dat$gr1 <- as.factor(dat$gr1)
dat$visit <- as.factor(dat$visit)

df <- aggregate(perf~visit+gr1, data=dat, mean)
df$se <- aggregate(perf~visit+gr1, data=dat, sd)[,3]/as.numeric(c(sqrt(27),sqrt(27),sqrt(25),sqrt(25)))
gp <- ggplot(data=df, aes(x=visit, y=perf, group=gr1))

gp + geom_line(aes(linetype=gr1), size=3)+theme_bw() +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=0.1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  geom_point(aes(shape=gr1), size=6) +
  geom_errorbar(aes(ymax=perf+se, ymin=perf-se),width=.1) +
  scale_y_continuous(limits = c(0.2, 1))



# III.3 MODULARITY

  # III.3a [modularity] N-back:
l=3
dat <- as.data.frame(subset(ddd[,c('subj', 'Qcor', 'visit', 'gr')], 
                            ddd$task=='nb' & ddd$L==l))
dat$subj <- as.factor(dat$subj)
dat$gr <- as.factor(dat$gr)
dat$visit <- as.factor(dat$visit)
dat$gr1[dat$gr=='0.00'] <- 'con'
dat$gr1[dat$gr=='1.00'] <- 'act'
dat$gr1 <- as.factor(dat$gr1)

df <- aggregate(Qcor~visit+gr1, data=dat, mean)
df$se <- aggregate(Qcor~visit+gr1, data=dat, sd)[,3]/as.numeric(c(sqrt(27),sqrt(27),sqrt(25),sqrt(25)))
gp <- ggplot(data=df, aes(x=visit, y=Qcor, group=gr1))

gp + geom_line(aes(linetype=gr1), size=3) + geom_point(aes(shape=gr1), size=6) + theme_bw() +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=0.1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  geom_errorbar(aes(ymax=Qcor+se, ymin=Qcor-se), width=.1) +
  scale_y_continuous(limits = c(0.12, 0.24))


# III.3b [modularity] Reasoning:
l=3
dat <- as.data.frame(subset(ddd[,c('subj', 'Qcor', 'visit', 'gr')], 
                            ddd$task=='r' & ddd$L==l))
dat$subj <- as.factor(dat$subj)
dat$gr <- as.factor(dat$gr)
dat$gr1[dat$gr=='0.00'] <- 'con'
dat$gr1[dat$gr=='1.00'] <- 'act'
dat$gr1 <- as.factor(dat$gr1)
dat$visit <- as.factor(dat$visit)

df <- aggregate(Qcor~visit+gr1, data=dat, mean)
df$se <- aggregate(Qcor~visit+gr1, data=dat, sd)[,3]/as.numeric(c(sqrt(27),sqrt(27),sqrt(25),sqrt(25)))
gp <- ggplot(data=df, aes(x=visit, y=Qcor, group=gr1))

gp + geom_line(aes(linetype=gr1), size=3) + geom_point(aes(shape=gr1), size=6) + theme_bw() +
  theme(axis.text=element_text(size=30), axis.title=element_text(size=0.1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  geom_errorbar(aes(ymax=Qcor+se, ymin=Qcor-se), width=.1) +
  scale_y_continuous(limits = c(0.12, 0.24))


# III.4 Final plot (shifts in performance + median splits)

# Prepare the data

# III.4a "Good" performers:
# Uncomment to plot intermediate results
#plot(c(1:4), c(0,0.05,0.1,0.15), type='n')
#points(c(1:4), c(mean(Qnb_fix_v1[ddd$NBclass[1:53]==T]),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==T)),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==T)),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==T))),
#       pch=1, cex=5)

dnb <- as.data.frame(cbind(c(1:4),c(mean(Qnb_fix_v1[ddd$NBclass[1:53]==T]),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==T)),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==T)),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==T)))))

dr <- as.data.frame(cbind(c(1:4),c(mean(Qr_fix_v1[ddd$Rclass[1:53]==T]),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==1 & dddV1c$Rclass==T)),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==2 & dddV1c$Rclass==T)),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==3 & dddV1c$Rclass==T)))))

dm <- rbind(dnb, dr)
dm$task <- c(rep('NB', 4), rep('R', 4))
colnames(dm)[1:2] <- c('L', 'Modularity')


mod1 <- c(Qnb_fix_v1[ddd$NBclass[1:53]==T],
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==T),
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==T),
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==T))
mod2 <- c(Qr_fix_v1[ddd$Rclass[1:53]==T],
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==1 & dddV1c$Rclass==T),
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==2 & dddV1c$Rclass==T),
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==3 & dddV1c$Rclass==T))

L1 <- c(rep(1,length(mod1)/4),rep(2,length(mod1)/4),rep(3,length(mod1)/4),rep(4,length(mod1)/4))
m1 <- cbind(mod1, L1)                                            

L2 <- c(rep(1,length(mod2)/4),rep(2,length(mod2)/4),rep(3,length(mod2)/4),rep(4,length(mod2)/4))
m2 <- cbind(mod2, L1)                                            

d <- as.data.frame(rbind(m1,m2))
d$task <- c(rep('nb', dim(d)[1]/2),rep('r', dim(d)[1]/2))
colnames(d)[1:2] <- c('Modularity', 'L') 

d$ModM[d$task=='nb' & d$L==1] <- dm$Modularity[dm$task=='NB' & dm$L==1]
d$ModM[d$task=='nb' & d$L==2] <- dm$Modularity[dm$task=='NB' & dm$L==2]
d$ModM[d$task=='nb' & d$L==3] <- dm$Modularity[dm$task=='NB' & dm$L==3]
d$ModM[d$task=='nb' & d$L==4] <- dm$Modularity[dm$task=='NB' & dm$L==4]
d$ModM[d$task=='r' & d$L==1] <- dm$Modularity[dm$task=='R' & dm$L==1]
d$ModM[d$task=='r' & d$L==2] <- dm$Modularity[dm$task=='R' & dm$L==2]
d$ModM[d$task=='r' & d$L==3] <- dm$Modularity[dm$task=='R' & dm$L==3]
d$ModM[d$task=='r' & d$L==4] <- dm$Modularity[dm$task=='R' & dm$L==4]


#ggplot(d) + 
#  geom_point(aes(x = L, y = ModM, shape=task), size = 7) +
#  stat_smooth(aes(x = L, y = Modularity, linetype=task, fill=task), method = "loess", se = T, size=3, color=1, span=0.95) +
#  coord_cartesian(ylim = c(0, 0.3)) + 
#  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # white background

dT <- d
dT$class <- T


# III.4b "Bad" performers:
#plot(c(1:4), c(0,0.05,0.1,0.15), type='n')
#points(c(1:4), c(mean(Qnb_fix_v1[ddd$NBclass[1:53]==F]),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==F)),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==F)),
#                 mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==F))),
#       pch=1, cex=5)

dnb <- as.data.frame(cbind(c(1:4),c(mean(Qnb_fix_v1[ddd$NBclass[1:53]==F]),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==F)),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==F)),
                                    mean(subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==F)))))

dr <- as.data.frame(cbind(c(1:4),c(mean(Qr_fix_v1[ddd$Rclass[1:53]==F]),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==1 & dddV1c$Rclass==F)),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==2 & dddV1c$Rclass==F)),
                                   mean(subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==3 & dddV1c$Rclass==F)))))

dm <- rbind(dnb, dr)
dm$task <- c(rep('NB', 4), rep('R', 4))
colnames(dm)[1:2] <- c('L', 'Modularity')
# Final plot:
mod1 <- c(Qnb_fix_v1[ddd$NBclass[1:53]==F],
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==1 & dddV1c$NBclass==F),
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==2 & dddV1c$NBclass==F),
          subset(dddV1c$Qcor, dddV1c$task=='nb' & dddV1c$L==3 & dddV1c$NBclass==F))
mod2 <- c(Qr_fix_v1[ddd$Rclass[1:53]==F],
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==1 & dddV1c$Rclass==F),
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==2 & dddV1c$Rclass==F),
          subset(dddV1c$Qcor, dddV1c$task=='r' & dddV1c$L==3 & dddV1c$Rclass==F))

L1 <- c(rep(1,length(mod1)/4),rep(2,length(mod1)/4),rep(3,length(mod1)/4),rep(4,length(mod1)/4))
m1 <- cbind(mod1, L1)                                            

L2 <- c(rep(1,length(mod2)/4),rep(2,length(mod2)/4),rep(3,length(mod2)/4),rep(4,length(mod2)/4))
m2 <- cbind(mod2, L1)                                            

d <- as.data.frame(rbind(m1,m2))
d$task <- c(rep('nb', dim(d)[1]/2),rep('r', dim(d)[1]/2))
colnames(d)[1:2] <- c('Modularity', 'L') 

d$ModM[d$task=='nb' & d$L==1] <- dm$Modularity[dm$task=='NB' & dm$L==1]
d$ModM[d$task=='nb' & d$L==2] <- dm$Modularity[dm$task=='NB' & dm$L==2]
d$ModM[d$task=='nb' & d$L==3] <- dm$Modularity[dm$task=='NB' & dm$L==3]
d$ModM[d$task=='nb' & d$L==4] <- dm$Modularity[dm$task=='NB' & dm$L==4]
d$ModM[d$task=='r' & d$L==1] <- dm$Modularity[dm$task=='R' & dm$L==1]
d$ModM[d$task=='r' & d$L==2] <- dm$Modularity[dm$task=='R' & dm$L==2]
d$ModM[d$task=='r' & d$L==3] <- dm$Modularity[dm$task=='R' & dm$L==3]
d$ModM[d$task=='r' & d$L==4] <- dm$Modularity[dm$task=='R' & dm$L==4]


#ggplot(d) + 
#  geom_point(aes(x = L, y = ModM, shape=task), size = 7) +
#  stat_smooth(aes(x = L, y = Modularity, linetype=task, fill=task), method = "loess", se = T, size=3, color=1, span=0.95) +
#  coord_cartesian(ylim = c(0, 0.3)) + 
#  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # white background


dF <- d
dF$class <- F

# III.4c Both "good" and "bad" performers:
d <- rbind(dT,dF)
ggplot(d) + 
  geom_point(aes(x = L, y = ModM, shape=task, color=task), size = 10, data=dT) +
  geom_point(aes(x = L, y = ModM, shape=task,color=task), size = 4, data=dF) +
  stat_smooth(aes(x = L, y = Modularity, color=task, method = "loess"), size=5, se=F, span=0.95, data=dT) +
  stat_smooth(aes(x = L, y = Modularity, color=task), linetype=2,method = "loess", se = F, size=2, span=0.95, data=dF) +
  coord_cartesian(ylim = c(0.1, 0.23)) + 
  theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) # white background
