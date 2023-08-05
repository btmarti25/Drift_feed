library(ggplot2)
library(signal) 
library(cowplot)
library(viridis)
library(gridExtra)

setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")

source("Code/R_functions/runPN_observed_speed_2D_ff.R")
source("Code/R_functions/interpolate_timeseries.R")
source("Code/R_functions/setup_track_flow.R")
source("Code/R_functions/get_dBA_exp.R")
source("Code/R_functions/lag_timeseries.R")
source("Code/R_functions/setup_model_runs_on_tracks.R")
source("Code/R_functions/get_pred_DBA.R")
source("Code/R_functions/get_pred_BA.R")
source("Code/R_functions/summarySE.R")

df <- read.csv("Data/Combined_track_data.csv")

parm_grid<-data.frame(model="PN",gain=seq(1,3,.5))
initial_depth <-0
integration_factor = 64
initial_depth=-0.0
tau=0.042
flow_ref = 1
fps=100
f_time=tau#seq(0,.5,.025)
## run pred PP for different lags and gains

# 





plotsingle_track <- function(df) {
  library("scales")
  cols = viridis(nrow(parm_grid),option="B",begin = .2,end=.8)
  xmid = mean(df$fish_x_f[df$frame > df$change_frame])
  ymid = mean(df$fish_y[df$frame > df$change_frame])
  
  plot(df$fish_x_f - xmid, df$fish_y - ymid, xlim = c(-7, 11), ylim = c(-6, 8),
       cex = 1.2, pch = 19, frame.plot = FALSE, xaxt = 'n', yaxt = 'n', ann = FALSE,asp=1)
  
  points(df$prey_x_f - xmid, df$prey_y - ymid, col = 'grey', cex = 1.2, pch = 19)
  
  start_frame = df$change_frame[1] - df$frame[1]
  
  for (t in start_frame:nrow(df)) {
    lines(c(df$fish_x_f[t], df$prey_x_f[t]) - xmid, c(df$fish_y[t], df$prey_y[t]) - ymid,
          col = alpha('grey', .9), lwd = .8)
  }
  
  for (i in 1:nrow(parm_grid)){
  out_PN <- setup_model_runs_on_tracks(df,N=parm_grid$gain[i],tau,model=parm_grid$model[i],initial_depth,integration_factor,f_time=f_time)
  PNx = out_PN$rx_m 
  PNx[df$frame<df$change_frame]<-NA
  PNy = out_PN$ry_m  
  PNy[df$frame<df$change_frame]<-NA
  lines(PNx - xmid, PNy - ymid, col = alpha(cols[i], .7), lwd = 6)
  }
  
  # Add scale bars and labels
  segments(5, -6.3, 5 + 5, -6.3, col = 'black', lwd = 4)
  # text(-7, -7, "0", pos = 2, cex = 0.8)
  text(4.8 , -6+1, "5 cm", pos = 4, cex = 2.5)
}




# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Final_figs/Fig3A_", formatted_date, "_", formatted_time, ".png")

#png(filename,width=1800,height=1800,res=300)

par(mfrow = c(2, 1), mar = c(1, 0.1, 0.1, 0.1), oma = c(0.1, 0.1, 0.1, 0.1))
#plotsingle_track(subset(df,(ID == 70)))
plotsingle_track(subset(df,(ID == 133)))
#plotsingle_track(subset(df,(ID == 18)))
plotsingle_track(subset(df,(ID == 67)))

#dev.off()


