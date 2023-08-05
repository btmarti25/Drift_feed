setup_track <- function(df,fps, integrationFactor,start_frame,end_frame,initial_depth,tau) {
  #initial_depth = -0.3
  zpos=seq(to= 0, by = df$z_speed[1]/(100*100),length.out=nrow(df))
  predPos=cbind(df$fish_x/100,df$fish_y/100,zpos)
  predVel = cbind(df$flowspeed/100-df$fish_dx/100,df$fish_dy/100,df$z_speed[1]/100)
  preyPos = cbind(df$prey_x/100,df$prey_y/100,0)
  preyVel = rbind(c(NA,NA,NA),diff(preyPos)*100)
  start_step <- start_frame + (start_frame-1)* (integrationFactor-1)
  end_step <- end_frame + (end_frame-1)* (integrationFactor-1)
  predPos <- interpolate_timeseries(predPos, integrationFactor )
  preyPos <- interpolate_timeseries(preyPos, integrationFactor )
  preyVel <- interpolate_timeseries(preyVel, integrationFactor )
  predVel <-interpolate_timeseries(predVel, integrationFactor )
  predSpeed <- interpolate_timeseries(cbind(df$fish_speed/100), integrationFactor)
  heading <-  interpolate_timeseries(cbind(df$heading), integrationFactor)
  d_t = 1/(fps * integrationFactor)
  tau_frames = (tau*fps)
  tau_steps= 1 + floor(tau_frames * integrationFactor)
  return(list(predPos,preyPos,preyVel,d_t,start_step,end_step,tau_steps,predSpeed,heading,predVel))
}


