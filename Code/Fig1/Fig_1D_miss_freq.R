library(ggplot2)
library(signal) 
library(cowplot)
library(viridis)
library(gridExtra)

setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")

source("Code/R_functions/interpolate_timeseries.R")
source("Code/R_functions/runPN_observed_speed_2D_ff.R")
source("Code/R_functions/setup_track_flow.R")
source("Code/R_functions/get_dBA_exp.R")
source("Code/R_functions/lag_timeseries.R")
source("Code/R_functions/setup_model_runs_on_tracks.R")
source("Code/R_functions/get_pred_DBA.R")
source("Code/R_functions/get_pred_BA.R")
source("Code/R_functions/summarySE.R")

df <- read.csv("Data/Combined_track_data.csv")

dfout<-data.frame(ID = df$ID,type = df$type, frame=df$frame,prey_x = df$prey_x, prey_y = df$prey_y ,prey_x_f = df$prey_x_f, fish_x = df$fish_x, fish_y = df$fish_y ,fish_x_f = df$fish_x_f)
## compare miss distance of models
fps=100
integration_factor = 64
initial_depth=-0.00
tau=0.042
flow_ref = 1
f_time=tau
heading_er={}
parm_grid<-data.frame(model="PN",gain=seq(.1,3,.1))
er_out<-{}
miss<-{}
miss_d = {}
miss_s = {}
for (i in 1:nrow(parm_grid)){
  out <- setup_model_runs_on_tracks(df,N=parm_grid$gain[i],tau,model=parm_grid$model[i],initial_depth,integration_factor,f_time=f_time)
  x = out$rx_m 
  y = out$ry_m  
  df$x<- x
  df$y<- y
  df$miss <-  sqrt((df$x-df$prey_x_f)^2+(df$y-df$prey_y)^2)
  sdf=subset(df,frame==max_frame )
  miss_dist<- aggregate(cbind(miss_dist=sdf$miss),by=list(ID=sdf$ID,type=sdf$type),FUN=min)
  temp<-data.frame(miss_dist)
  temp$mod<-parm_grid$model[i]
  er_out<-rbind(er_out,temp)
  
  
  miss[i] = sum(temp$miss_dist>1.5)/nrow(temp)
  
  temp_d<-subset(temp,type=="DoubleVeer")
  miss_d[i] = sum(temp_d$miss_dist>1.5)/nrow(temp_d)
  
  temp_s<-subset(temp,type!="DoubleVeer")
  miss_s[i] = sum(temp_s$miss_dist>1.5)/nrow(temp_s)
}

miss_df <- data.frame(gain = parm_grid$gain, miss,miss_d,miss_s)



## calculate observed miss distances for each track
obs_miss_dist={}
obs_type={}
obs_ID={}
splitData<-split(df, df$ID)
for (j in 1:length(splitData)){
  obs_miss= (sqrt((splitData[[j]]$prey_x_f[nrow(splitData[[j]])] -splitData[[j]]$fish_x_f[nrow(splitData[[j]])])^2+(splitData[[j]]$prey_y[nrow(splitData[[j]])]-splitData[[j]]$fish_y[nrow(splitData[[j]])])^2))
  obs_miss_dist[j] = (sqrt((splitData[[j]]$prey_y[nrow(splitData[[j]])]-splitData[[j]]$fish_y[nrow(splitData[[j]])])^2))
  # obs_miss_dist[j] = min(obs_miss)
  obs_type[j]<-splitData[[j]]$type[1]
  obs_ID[j]<-splitData[[j]]$ID[1]
}
df_obs<-data.frame(miss_dist=obs_miss_dist,type=obs_type,ID=obs_ID)



obs_miss = sum(df_obs$miss_dist>1.5)/nrow(df_obs)

ggplot(miss_df,aes(x=gain,y=miss))+geom_line(lwd=2,color="#E08957")+
  geom_hline(yintercept=obs_miss,lwd=1,linetype="dashed")+
  theme_cowplot()+ylim(0,1)+xlab("")+ylab("")

