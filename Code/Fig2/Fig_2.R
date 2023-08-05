library(signal)
setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")

source("Code/R_functions/lag_timeseries.R")

df <- read.csv("Data/Combined_track_data.csv")
fps=100

splitData<-split(df,df$ID)

dBAout<-get_dBA_exp(df$prey_x_f,df$prey_y,df$prey_dx_f,df$prey_dy,df$fish_x_f,df$fish_y,df$fish_dx_f,df$fish_dy)
df$dBA<-dBAout$DBA


lag_steps=4
splitData<-split(df,df$ID)
for (i in 1:length(splitData)){
  splitData[[i]]$vel_heading <- -atan2(splitData[[i]]$fish_dy,-splitData[[i]]$fish_dx_f)
  splitData[[i]]$lag_dBA  <-  lag_timeseries(splitData[[i]]$dBA,lag_steps)
  splitData[[i]]$d_vel_heading <- sgolayfilt(splitData[[i]]$vel_heading ,p=2,n=5,m=1,ts=(1/fps))
}

df<-unsplit(splitData, df$ID) 

ggplot(subset(df,frame>change_frame),aes(x=lag_dBA,y=-d_vel_heading))+geom_point(alpha=0.2)



df$veer_frame=(df$frame-(df$change_frame+5))

ldf<-subset(df,frame < endframe -1 & veer_frame >= 0 & veer_frame < 11 & type !="DoubleVeer")

p1<-ggplot(ldf,aes(x=lag_dBA,y=-d_vel_heading))+geom_point(alpha=.5)+
  facet_wrap(~veer_frame,ncol=11)+
  stat_smooth(method='lm',color="#E08957")+
  theme_cowplot()+
  theme(axis.line=element_blank(),
        axis.text=element_blank())+
  scale_x_continuous(breaks=c(-25,0,25),limits=c(-30,30))+
  scale_y_continuous(breaks=c(-50,0,50),limits=c(-60,60))+
  geom_segment(x = -25.5, y = -66, xend = 25.5, yend = -66)+
  geom_segment(x = -33, y = -51, xend = -33, yend = 51)+ylab("")+xlab("")
#ggplot(ldf,aes(x=lag_dBAs,y=-d_heading))+geom_point()+facet_wrap(~veer_frame)+stat_smooth(method='lm',color='red')+theme_cowplot()

gain={}
se = {}
for (i in 1:11){
  s_df<-subset(ldf,veer_frame == (i-1) & type !="DoubleVeer")
  mod<-lm(d_vel_heading~ lag_dBA,s_df)
  #  mod<-lm(d_smooth_heading~ lag_dBAs,s_df)
  gain[i] =  -coef(mod)[2]
  se[i]=summary(mod)$coefficients[2,2]
}

time=(0:10)*10
g_df<-data.frame(time,gain,se)
g_df$upr=g_df$gain+2*g_df$se
g_df$lwr=g_df$gain-2*g_df$se
g_df$col<- "N"
p2<-ggplot(g_df,aes(x=time,y=gain))+geom_line(lwd=1)+geom_ribbon(aes(x=time,ymax=upr,ymin=lwr),fill="#E08957",alpha=.4)+theme_cowplot()+
  xlab("")+ylab("")

p2<-ggplot(g_df,aes(x=time,y=gain))+geom_point(aes(fill=col),shape=21,size=3)+geom_errorbar(aes(x=time,ymax=upr,ymin=lwr),width=.1)+theme_cowplot()+
  xlab("")+geom_point(aes(fill=col),shape=21,size=3)+scale_fill_viridis_d(begin=.7,end=.7,option="C")+ 
  theme(legend.position = "none",
        axis.title.y=element_blank())


grid.arrange(p1,p2,heights=c(.4,.7))
