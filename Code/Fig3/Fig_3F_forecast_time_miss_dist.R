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

## compare miss distance of models
integration_factor = 16
initial_depth=-0.0
tau=0.042
flow_ref = 1
heading_er={}
f_time=seq(0,.8,.025)
gain = seq(0.2,2,.05)
heading_er={}
parm_grid=expand.grid(model= "pPP",gain=gain,f_time=f_time)
bestout={}

er_out<-{}
miss_dist<-{}
for (i in 1:nrow(parm_grid)){
  print(i/nrow(parm_grid))
  out <- setup_model_runs_on_tracks(df,N=parm_grid$gain[i],tau,model=parm_grid$model[i],initial_depth,integration_factor,f_time=parm_grid$f_time[i])
  x = out$rx_m 
  y = out$ry_m  
  df$x<- x
  df$y<- y
  df$miss <-  sqrt((df$x-df$prey_x_f)^2+(df$y-df$prey_y)^2)
  sdf=subset(df,frame>change_frame )
  miss_dist<- aggregate(cbind(miss_dist=sdf$miss),by=list(ID=sdf$ID),FUN=min)
  temp<-data.frame(miss_dist)
  temp$gain<-parm_grid$gain[i]
  temp$f_time<-parm_grid$f_time[i]
  er_out<-rbind(er_out,temp)
}

er_out_mean<-aggregate(miss_dist ~ f_time + gain, data= er_out, mean)

er_out_mean<-subset(er_out_mean,gain>.1 & gain < 2)
ggplot(er_out_mean,aes(x=50*gain,y=1000*f_time))+geom_tile(aes(fill=miss_dist))+
  xlab(expression(paste("gain s"^"-1"))) +
  ylab("Forecast time (ms)")+scale_fill_viridis_b(direction=-1,n.breaks=12,end=.95,option = "inferno",alpha=.9)+theme_cowplot()+
  geom_hline(aes(yintercept=42),linetype="dashed",lwd=1)

setwd("Final_figs")

# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Fig3C_", formatted_date, "_", formatted_time, ".png")
ggsave(filename,
       width = 4,
       height =3
       )