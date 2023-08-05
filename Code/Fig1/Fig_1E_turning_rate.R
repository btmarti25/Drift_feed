library(signal)
library(segmented)
library(cowplot)
library(viridis)
library(ggplot2)

setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")
source("Code/R_functions/lag_timeseries.R")
df <-  read.csv("Data/Combined_track_data.csv")

fps=100
lag_steps=4

#get lagged LOS rate
splitData<-split(df,df$ID)
for (i in 1:length(splitData)){
  splitData[[i]]$lag_dBA  <-  lag_timeseries(splitData[[i]]$dBA,lag_steps)
  splitData[[i]]$d_vel_heading_sm <-   sgolayfilt(splitData[[i]]$vel_heading ,p=1,n=5,m=1,ts=(1/fps))
  
}

df<-unsplit(splitData, df$ID) 
sdf<- subset(df,  frame > change_frame  & frame < (max_frame-2)  )

sdf$pred_min = pmin(3*sdf$lag_dBA,5*sdf$lag_dBA)
sdf$pred_max = pmax(3*sdf$lag_dBA,5*sdf$lag_dBA)


#fit linear model
mod<-lm(-d_vel_heading_sm~0+lag_dBA,sdf)
summary(mod)
confint(mod)


ggplot()+geom_bin2d(data=sdf,aes(x=lag_dBA,y=-d_vel_heading_sm),bins = 70)+
  scale_fill_viridis(option = "C")+theme_cowplot()+
  geom_abline(slope=coef(mod),lwd=1)+xlab("")+ylab("")+
  geom_abline(slope=3,lwd=.5)+xlab("")+ylab("")+
  geom_abline(slope=5,lwd=.5)+xlab("")+ylab("")
 

