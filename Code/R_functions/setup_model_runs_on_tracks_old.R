setup_model_runs_on_tracks_old <- function(df,N,tau,model,initial_depth,integration_factor){



#df$z_speed =  -(initial_depth * 100)/((df$max_frame - df$change_frame)/100)
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
  start_frame =  15
  end_frame = nrow(sdf)
  flowVel = c(0,0,0)

  setup <-setup_track_flow(df=sdf, fps=150,integration_factor,start_frame,end_frame,initial_depth)
  r_p = setup[[1]]; r_t = setup[[2]]; v_t = setup[[3]];
  d_t = setup[[4]]; start_step = setup[[5]]; end_step = setup[[6]]; tau_steps = setup[[7]]
  fish_speed = setup[[8]]
  heading = -setup[[9]]
  v_m=setup[[10]]
 #if (model == 1){
  #runPN_observed_speed runs PN that requires measuring closing velocity and works in 3D
 # simOut<-runPN_observed_speed_2D(N, flowVel, r_p, r_t, v_t , d_t,tau_steps, start_step, end_step, integration_factor,fish_speed,heading,v_m,flow_ref,model,ID=sdf$ID[1])
#} else {
  # runs PN that just requires measuring bearing angle
  simOut<-runPN_observed_speed_2D(N, flowVel, r_p, r_t, v_t , d_t,tau_steps, start_step, end_step, integration_factor,fish_speed,heading,v_m,flow_ref,model,ID=sdf$ID[1])
#}
  ID_out = c(ID_out ,splitdf[[i]]$ID)
  frame_out = c(frame_out ,splitdf[[i]]$frame)
  r_m_out = rbind(r_m_out, simOut[[1]])
  v_m_out = rbind(v_m_out, simOut[[2]])
  a_m_out = rbind(a_m_out, simOut[[3]])
  #lam_out = c(lam_out, simOut[[4]])
}
mod_out<-data.frame(ID=ID_out, frame= frame_out,rx_m = r_m_out[,1], ry_m = r_m_out[,2],rz_m = r_m_out[,3],vx=v_m_out[,1],vy=v_m_out[,2])



df_out<-merge(df,mod_out,by=c("ID","frame"))

df_out$pred_heading = -atan2(df_out$vy,-df_out$vx)
df_out <- df_out[order(df_out $frame),]
df_out <- df_out[order(df_out $ID),]


splitData<-split(df_out, df$ID)

for (i in 1:length(splitData)){
  splitData[[i]]$max_pred_heading <- max(abs(splitData[[i]]$pred_heading),na.rm=T)
}
df_out<-unsplit(splitData, df_out$ID)
}
