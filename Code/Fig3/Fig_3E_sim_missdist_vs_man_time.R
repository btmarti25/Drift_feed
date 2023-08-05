## Load libraries
library(ggplot2)
library(cowplot)
library(Rmisc)
library(reshape2)
library(scales)

## Load custom functions
source("Code/R_functions/interpolate_timeseries.R")
source("Code/R_functions/setup_track.R")
source("Code/R_functions/get_dBA.R")
source("Code/R_functions/get_dBA_exp.R")
source("Code/R_functions/lag_timeseries.R")
source("Code/R_functions/get_pred_DBA.R")
source("Code/R_functions/run_sim.R")
source("Code/R_functions/get_pred_BA.R")
source("Code/R_functions/anglediff.R")



dt = 0.0005
lag = 0.042
lag_steps =  lag/dt


min_PN={}
min_pPN={}
min_pPP={}

N_sim=1000
maneuver_dist=runif(N_sim,min = 0*lag, max =3*lag)
maneuver_angle = runif(N_sim,min=pi/5,max=pi/3)
k = runif(N_sim,min=1/1.2,max=1*1.2)
s_p = runif(N_sim,min=60,max=120)
s_t = s_p * runif(N_sim,min=.6,max=.8)
tMax=1.5

for (i in 1:N_sim){
  
  out<-run_sim(model="PN",lag,dt,s_p[i],s_t[i],maneuver_dist[i],maneuver_angle[i],1.2*k[i],tMax,f_time=0.05)
  min_PN[i] = out[[5]]
  
  out<-run_sim(model="pPN",lag,dt,s_p[i],s_t[i],maneuver_dist[i],maneuver_angle[i],.8*k[i],tMax,f_time=0.05)
  min_pPN[i] = out[[5]]
  
  out<-run_sim(model="pPP",lag,dt,s_p[i],s_t[i],maneuver_dist[i],maneuver_angle[i],55*k[i],tMax,f_time=0.05)
  min_pPP[i] = out[[5]]
  
  
}

df<-data.frame(k,maneuver_dist,s_p,s_t,PN=min_PN,pPN=min_pPN,pPP=min_pPP)

df_out<-melt(df,id=c("k","maneuver_dist","s_p","s_t"))
names(df_out)[5]<-"model"
df_small<-df_out[runif(nrow(df_out))<.5,]
ggplot()+geom_point(data=df_small,aes(x=maneuver_dist*1000,y=value,group=model,color=model),alpha=.2)+
  geom_smooth(data=df_out,aes(x=maneuver_dist*1000,y=value,group=model,color=model),method='gam',size=2,se=F)+
  theme_cowplot()+
  scale_color_viridis_d(option="C",end=.7,direction=-1)+
  xlab("maneuver time (ms)")+
  ylab("miss distance (cm)")



# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Final_figs/Fig3E_", formatted_date, "_", formatted_time, ".png")
ggsave(filename,
       width = 4,
       height =3
)                    