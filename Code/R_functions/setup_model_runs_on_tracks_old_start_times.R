setup_model_runs_on_tracks_old_start_times <- function(df,N,tau,model,initial_depth,integration_factor,f_time,fps){

dataout ={}

df$z_speed = 0
df$fish_speed = sqrt((df$fish_dx_f)^2+df$fish_dy^2)

splitdf<-split(df,df$ID)
r_m_out = c()
v_m_out = c()
a_m_out = c()
ID_out = c()
lam_out=c()
frame_out =c()
for (i in 1:length(splitdf)){
  # for (i in 41){
  sdf= splitdf[[i]]

  if (fps == 100){
  start_frame = max(splitdf[[i]]$change_frame[1]- splitdf[[i]]$frame[1],9)
  interval = 1
  }else{
    start_frame = max(nrow(sdf)-45,10)
    interval = 2
  }
  #print(start_frame)
  for (j in rep(seq(start_frame,(nrow(sdf)),interval),1)){
  start_frame =  j
  end_frame = min(nrow(sdf),j+20)
  flowVel = c(0,0,0)

  setup <- setup_track_flow(df=sdf, fps=fps,integration_factor,start_frame,end_frame,initial_depth,tau)
  r_p = setup[[1]]; r_t = setup[[2]]; v_t = setup[[3]];
  d_t = setup[[4]]; start_step = setup[[5]]; end_step = setup[[6]]; tau_steps = setup[[7]]
  fish_speed = setup[[8]]
  heading = -setup[[9]]
  v_m=setup[[10]]

  simOut<-runPN_observed_speed_2D_ff(N, flowVel, r_p, r_t, v_t , d_t,tau_steps, start_step, end_step, integration_factor,fish_speed,heading,v_m,flow_ref,model,ID=sdf$ID[1],f_time)

  ID_out = c(ID_out ,splitdf[[i]]$ID)
  frame_out = c(frame_out ,splitdf[[i]]$frame)
  r_m_out = rbind(r_m_out, simOut[[1]])
  v_m_out = rbind(v_m_out, simOut[[2]])
  a_m_out = rbind(a_m_out, simOut[[3]])
  #lam_out = c(lam_out, simOut[[4]])
  vx = simOut[[2]][,1]
  vy = simOut[[2]][,2]
  rx = simOut[[1]][start_frame:end_frame,1] 
  ry = simOut[[1]][start_frame:end_frame,2] 
 # pred_heading = -atan2(vy,-vx)[start_frame:end_frame]
 # obs_heading = splitdf[[i]]$heading[start_frame:end_frame]
  obs_x= splitdf[[i]]$fish_x_f[start_frame:end_frame]
  obs_y= splitdf[[i]]$fish_y[start_frame:end_frame]
 # heading_error = obs_heading - pred_heading

  pos_error = sqrt((rx-obs_x)^2+(ry-obs_y)^2)
  temp = data.frame(h = 1:length(pos_error),pos_error)
  temp$ID = sdf$ID[1]
  temp$start_frame = j
  dataout = rbind(dataout,temp)
  }
}

return(dataout)
}
