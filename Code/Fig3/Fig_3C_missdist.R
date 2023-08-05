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
integration_factor = 8 *4
initial_depth=-0.0
tau=0.042
flow_ref = 1
f_time=tau#seq(0,.5,.025)
heading_er={}
parm_grid<-data.frame(model=c("PN","pPN","pPP"),gain=c(1.2,0.8,1.1))
er_out<-{}
miss_dist<-{}
for (i in 1:nrow(parm_grid)){
  out <- setup_model_runs_on_tracks(df,N=parm_grid$gain[i],tau,model=parm_grid$model[i],initial_depth,integration_factor,f_time=f_time)
  x = out$rx_m 
  y = out$ry_m  
  df$x<- x
  df$y<- y
  df$miss <-  sqrt((df$x-df$prey_x_f)^2+(df$y-df$prey_y)^2)
  sdf=subset(df,frame==max_frame )
  miss_dist<- aggregate(cbind(miss_dist=sdf$miss),by=list(ID=sdf$ID),FUN=min)
  temp<-data.frame(miss_dist)
  temp$mod<-parm_grid$model[i]
  er_out<-rbind(er_out,temp)
}

## calculate observed miss distances for each track
obs_miss_dist={}
splitData<-split(df, df$ID)
for (j in 1:length(splitData)){
  obs_miss= (sqrt((splitData[[j]]$prey_x_f[nrow(splitData[[j]])] -splitData[[j]]$fish_x_f[nrow(splitData[[j]])])^2+(splitData[[j]]$prey_y[nrow(splitData[[j]])]-splitData[[j]]$fish_y[nrow(splitData[[j]])])^2))
  obs_miss_dist[j] = (sqrt((splitData[[j]]$prey_y[nrow(splitData[[j]])]-splitData[[j]]$fish_y[nrow(splitData[[j]])])^2))
  # obs_miss_dist[j] = min(obs_miss)
}
df_obs<-data.frame(miss_dist=obs_miss_dist)
df_obs$model<-"Obs"

miss_out<-data.frame(miss_dist=er_out$miss_dist, model=er_out$mod)
miss_out<-rbind(df_obs,miss_out)

summary(lm(miss_dist~model,miss_out))
df_fit_h <- summarySE(miss_out, measurevar="miss_dist", groupvars=c("model"))

df_fit_h$model[df_fit_h$model=="Obs"]<-NA

ggplot()+
  geom_errorbar(data=df_fit_h,aes(x=factor(model),ymin=miss_dist-ci,ymax=miss_dist+ci),width=.1)+
  geom_point(data=df_fit_h,aes(x=factor(model),y=miss_dist,fill=factor(model)),shape=21,size=4)+
  theme_cowplot()+ylab("miss distance (cm)") + 
  scale_fill_viridis_d(option="C",end=.7,direction=-1,begin=.2,na.value="grey")




# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Final_figs/Fig3F_", formatted_date, "_", formatted_time, ".png")
ggsave(filename,
       width = 4,
       height =3
)                    