get_pred_DBA  <- function(r_t,v_t,r_p,v_p,t,lag_steps,dt,f_time,update_rate_frames) {

  LOS <-  get_dBA_exp(prey_x=r_t[t-lag_steps,1],
                  prey_y=r_t[t-lag_steps,2],
                  prey_dx=v_t[t-lag_steps,1],
                  prey_dy=v_t[t-lag_steps,2],
                  fish_x=r_p[t-lag_steps,1],
                  fish_y=r_p[t-lag_steps,2],
                  fish_dx=v_p[t-lag_steps,1],
                  fish_dy=v_p[t-lag_steps,2])
  LOS_min1 <-  get_dBA_exp(prey_x=r_t[t-(lag_steps+1),1],
                  prey_y=r_t[t-(lag_steps+1),2],
                  prey_dx=v_t[t-(lag_steps+1),1],
                  prey_dy=v_t[t-(lag_steps+1),2],
                  fish_x=r_p[t-(lag_steps+1),1],
                  fish_y=r_p[t-(lag_steps+1),2],
                  fish_dx=v_p[t-(lag_steps+1),1],
                  fish_dy=v_p[t-(lag_steps+1),2])

  dBA2 <-  (LOS$DBA - LOS_min1$DBA) /(dt)

  #print(LOS$DBA - LOS_min1$DBA)
 # if (abs(LOS$DBA - LOS_min1$DBA) > 3){
 #   print(c(LOS$DBA - LOS_min1$DBA,t))
 #   dBA2=0
  #  }
 # if (LOS_min1$DBA == 0){
  #  dBA2=0
  #  }
  pred_dBA =  LOS$DBA + dBA2 * f_time

}
