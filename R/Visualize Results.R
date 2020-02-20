
################################################

rm(list=ls())
library(tidyverse)
library(ggplot2)
source("Functions.R")

################################################

res<-read.table("Data/res.csv",header=T,sep=",")
res$n_sample = as.factor(res$n_sample)
res$method = factor(res$method,levels=c("ML","Bayes_default","Bayes_infI","Bayes_infII"))

################################################

# METACOGN~NEUROT

trueValueMetaNeur = .205

ggplot(res[res$parameter=="METACOGN~NEUROT",],aes(x=n_sample,y=est,fill=method))+
  geom_hline(yintercept=c(0,trueValueMetaNeur),size=1.5,linetype=2,color=c("black","red"))+geom_boxplot(size=1)+scale_y_continuous(breaks=seq(-1,1,.2))+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Standardized coefficient")

mn<-data.frame(res%>%
              filter(parameter=="METACOGN~NEUROT")%>%
              group_by(n_sample,method)%>%
              summarise(relative_mean_bias = relative_bias(estimates_vector = est, true_value = trueValueMetaNeur, FUN = mean),
                        relative_median_bias = relative_bias(estimates_vector = est, true_value = trueValueMetaNeur, FUN = median),
                        mean_squared_error = mean_squared_error(estimates_vector = est, true_value = trueValueMetaNeur),
                        coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = trueValueMetaNeur),
                        power = power(lb_vector = ci.lower, ub_vector = ci.upper)))
mn

ggplot(mn,aes(x=as.numeric(as.character(n_sample)),y=power,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Power")+scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1))+
  scale_x_continuous(breaks=c(30,50,100,500))

ggplot(mn,aes(x=as.numeric(as.character(n_sample)),y=coverage,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Coverage")+scale_y_continuous(breaks=seq(.9,1,.02),limits=c(.9,1))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(mn,aes(x=as.numeric(as.character(n_sample)),y=relative_median_bias*100,linetype=method,group=method,color=method))+
  geom_hline(yintercept=c(-10,10),size=1)+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Relative median bias %")+scale_y_continuous(breaks=seq(-100,100,5))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(mn,aes(x=as.numeric(as.character(n_sample)),y=mean_squared_error,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Mean Squared Error")+scale_y_continuous()+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

################################################

# SLEEP~METACOGN

trueValueSleepMeta = -.363

ggplot(res[res$parameter=="SLEEP~METACOGN",],aes(x=n_sample,y=est,fill=method))+
  geom_hline(yintercept=c(0,trueValueSleepMeta),size=1.5,linetype=2,color=c("black","red"))+geom_boxplot(size=1)+scale_y_continuous(breaks=seq(-1,1,.2))+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Standardized coefficient")

sm<-data.frame(res%>%
             filter(parameter=="SLEEP~METACOGN")%>%
             group_by(n_sample,method)%>%
             summarise(relative_mean_bias = relative_bias(estimates_vector = est, true_value = trueValueSleepMeta, FUN = mean),
                       relative_median_bias = relative_bias(estimates_vector = est, true_value = trueValueSleepMeta, FUN = median),
                       mean_squared_error = mean_squared_error(estimates_vector = est, true_value = trueValueSleepMeta),
                       coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = trueValueSleepMeta),
                       power = power(lb_vector = ci.lower, ub_vector = ci.upper)))
sm

ggplot(sm,aes(x=as.numeric(as.character(n_sample)),y=power,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Power")+scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1))+
  scale_x_continuous(breaks=c(30,50,100,500))

ggplot(sm,aes(x=as.numeric(as.character(n_sample)),y=coverage,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Coverage")+scale_y_continuous(breaks=seq(.9,1,.02),limits=c(.9,1))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(sm,aes(x=as.numeric(as.character(n_sample)),y=relative_median_bias*100,linetype=method,group=method,color=method))+
  geom_hline(yintercept=c(-10,10),size=1)+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Relative median bias %")+scale_y_continuous(breaks=seq(-100,100,5))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(sm,aes(x=as.numeric(as.character(n_sample)),y=mean_squared_error,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Mean Squared Error")+scale_y_continuous()+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

################################################

# SLEEP~NEUROT

trueValueSleepNeur= -.129

ggplot(res[res$parameter=="SLEEP~NEUROT",],aes(x=n_sample,y=est,fill=method))+
  geom_hline(yintercept=c(0,trueValueSleepNeur),size=1.5,linetype=2,color=c("black","red"))+geom_boxplot(size=1)+scale_y_continuous(breaks=seq(-1,1,.2))+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Standardized coefficient")

sn<-data.frame(res%>%
             filter(parameter=="SLEEP~NEUROT")%>%
             group_by(n_sample,method)%>%
             summarise(relative_mean_bias = relative_bias(estimates_vector = est, true_value = trueValueSleepNeur, FUN = mean),
                       relative_median_bias = relative_bias(estimates_vector = est, true_value = trueValueSleepNeur, FUN = median),
                       mean_squared_error = mean_squared_error(estimates_vector = est, true_value = trueValueSleepNeur),
                       coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = trueValueSleepNeur),
                       power = power(lb_vector = ci.lower, ub_vector = ci.upper)))
sn

ggplot(sn,aes(x=as.numeric(as.character(n_sample)),y=power,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Power")+scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1))+
  scale_x_continuous(breaks=c(30,50,100,500))

ggplot(sn,aes(x=as.numeric(as.character(n_sample)),y=coverage,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Coverage")+scale_y_continuous(breaks=seq(.9,1,.02),limits=c(.9,1))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(sn,aes(x=as.numeric(as.character(n_sample)),y=relative_median_bias*100,linetype=method,group=method,color=method))+
  geom_hline(yintercept=c(-10,10),size=1)+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Relative median bias %")+scale_y_continuous(breaks=seq(-100,100,5))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(sn,aes(x=as.numeric(as.character(n_sample)),y=mean_squared_error,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Mean Squared Error")+scale_y_continuous()+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

################################################

# indirect

trueValueIndirect = -.075

ggplot(res[res$parameter=="indirect",],aes(x=n_sample,y=est,fill=method))+
  geom_hline(yintercept=c(0,trueValueIndirect),size=1.5,linetype=2,color=c("black","red"))+geom_boxplot(size=1)+scale_y_continuous(breaks=seq(-1,1,.2))+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Standardized coefficient")

ind<-data.frame(res%>%
             filter(parameter=="indirect")%>%
             group_by(n_sample,method)%>%
             summarise(relative_mean_bias = relative_bias(estimates_vector = est, true_value = trueValueIndirect, FUN = mean),
                       relative_median_bias = relative_bias(estimates_vector = est, true_value = trueValueIndirect, FUN = median),
                       mean_squared_error = mean_squared_error(estimates_vector = est, true_value = trueValueIndirect),
                       coverage = coverage(lb_vector = ci.lower, ub_vector = ci.upper, true_value = trueValueIndirect),
                       power = power(lb_vector = ci.lower, ub_vector = ci.upper)))
ind

ggplot(ind,aes(x=as.numeric(as.character(n_sample)),y=power,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Power")+scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1))+
  scale_x_continuous(breaks=c(30,50,100,500))

ggplot(ind,aes(x=as.numeric(as.character(n_sample)),y=coverage,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Coverage")+scale_y_continuous(breaks=seq(.9,1,.02),limits=c(.9,1))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(ind,aes(x=as.numeric(as.character(n_sample)),y=relative_median_bias*100,linetype=method,group=method,color=method))+
  geom_hline(yintercept=c(-10,10),size=1)+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Relative median bias %")+scale_y_continuous(breaks=seq(-100,100,5))+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

ggplot(ind,aes(x=as.numeric(as.character(n_sample)),y=mean_squared_error,linetype=method,group=method,color=method))+geom_line(size=2)+
  theme(axis.title=element_text(size=25),axis.text=element_text(size=25),legend.title=element_text(size=16),legend.text=element_text(size=14))+
  xlab("Sample size (N)")+ylab("Mean Squared Error")+scale_y_continuous()+
  scale_x_continuous(breaks=c(30,50,100,500),trans="log")

 ################################################

