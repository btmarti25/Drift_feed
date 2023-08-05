
setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")
library(ggplot2)
library(cowplot)
library(viridis)
library(gridExtra)
library(signal)
# source("R_functions/interpolate_timeseries.R")
# source("R_functions/runPN_observed_speed_2D_ff.R")
# source("R_functions/setup_track.R")
# source("R_functions/setup_track_flow.R")
# source("R_functions/get_dBA.R")
# source("R_functions/get_dBA_exp.R")
# source("R_functions/lag_timeseries.R")
# source("R_functions/get_forecast_dh.R")
# source("R_functions/setup_model_runs_on_tracks.R")
# source("R_functions/get_pred_DBA.R")
# source("R_functions/get_pred_BA.R")
# source("R_functions/summarySE.R")


source("Code/R_functions/interpolate_timeseries.R")
source("Code/R_functions/runPN_observed_speed_2D_ff.R")
source("Code/R_functions/setup_track_flow.R")
source("Code/R_functions/get_dBA_exp.R")
source("Code/R_functions/lag_timeseries.R")
source("Code/R_functions/setup_model_runs_on_tracks.R")
source("Code/R_functions/get_pred_DBA.R")
source("Code/R_functions/get_pred_BA.R")
source("Code/R_functions/summarySE.R")
source("Code/R_functions/setup_model_runs_on_tracks_old_start_times.R")



## Model comparison with moving reference frame
df <- read.csv("Data/Combined_track_data.csv")


## run pred PP for different lags and gains
# use flow reference frame
flow_ref=1
fps<-100
integration_factor = 64
tau= 0.042 # delay in ms
model = c("PN","pPN","pPP")
initial_depth=-0.0
f_time=tau #seq(0,.5,.025)
gain = seq(.7,1.3,0.05)
heading_er={}

parm_grid=expand.grid(model= model,gain=gain,f_time=f_time)
mean_er={}
# mean_er_Veer={}
# mean_er_singleVeer={}
# mean_er_doubleVeer={}
# heading_er_singleVeer={}
# heading_er_doubleVeer={}

dataout={}

for (i in 1:nrow(parm_grid)){
  print(i/nrow(parm_grid))  
  #### run PN model
  temp <- setup_model_runs_on_tracks_old_start_times(df,N=parm_grid$gain[i],tau,model=parm_grid$model[i],initial_depth,integration_factor,f_time=parm_grid$f_time[i],fps=100)
  temp$gain<-parm_grid$gain[i]
  temp$model<-parm_grid$model[i]
  temp$f_time<-parm_grid$f_time[i]
  dataout=rbind(dataout,temp)
}

# estimate parameters that maximize 100 ms model error
ag_dat<-subset(dataout,h==10)

#ag_dat<-aggregate(pos_error ~ model +  gain + ID + h, data= ag_dat, mean)
ag_dat<-aggregate(pos_error ~ model +  gain + ID , data= ag_dat, mean)

ag_dat<-aggregate(pos_error ~ model +  gain , data= ag_dat, mean)


library(dplyr)    
best_parm <- ag_dat %>% 
  group_by(model) %>% 
  slice(which.min(pos_error))
best_parm

ggplot(ag_dat,aes(x=gain,y=pos_error,group=model))+geom_line(aes(color=factor(model)),size=1)+theme_cowplot()


################## Run on models with best gains

bestout={}
for (i in 1:nrow(best_parm)){
  
  print(i/nrow(best_parm))
  temp <- setup_model_runs_on_tracks_old_start_times(df,N=best_parm$gain[i],tau,model=best_parm$model[i],initial_depth,integration_factor,f_time=tau,fps=100)
  temp$gain<-best_parm$gain[i]
  temp$model<-best_parm$model[i]
  bestout=rbind(bestout,temp)
}

best_dat<-aggregate(pos_error ~ model+gain+h+ ID, data= bestout, mean)

df_fit_h <- summarySE(data=best_dat, measurevar="pos_error", groupvars=c("model","h"))

ggplot(subset(df_fit_h,model != "CN" ),aes(x=h*10,y=pos_error,group=model))+geom_line(aes(color=factor(model)),size=1)+
  theme_cowplot()+xlim(0,200)+ylim(0,1.1)+
  ylab("Forecast error (cm)")+xlab("Forecast time (ms)")+
  geom_ribbon(aes(x=h*10,ymax=pos_error+2*se,ymin=pos_error-2*se,fill=factor(model)),alpha=.3)+ 
  scale_color_viridis_d(option = "C", end = 0.7, direction = -1) +  scale_fill_viridis_d(option = "C", end = 0.7, direction = -1) 


# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Final_figs/Fig3B_", formatted_date, "_", formatted_time, ".png")
#ggsave(filename,
#       width = 4,
#       height =3
#)


