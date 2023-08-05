get_pred_error_angle  <- function(r_t,v_t,r_p,v_p,t,lag_steps,d_t,h) {

  
  #Calculate delayed error angle
    prey_x = r_t[t-lag_steps,1]# + v_t[t-1,1] *.0
    prey_y = r_t[t-lag_steps,2]# + v_t[t-1,2] *.0
    fish_x = r_p[t-lag_steps,1]
    fish_y  = r_p[t-lag_steps,2]
    x_diff =  fish_x - prey_x
    y_diff = fish_y - prey_y
    pred_BA =-atan2(y_diff,x_diff)
    error_new =  (pred_BA -  h[t-lag_steps])
  # Calculate error angle 1 step before  
    prey_x = r_t[t-lag_steps-1,1]# + v_t[t-1,1] *.0
    prey_y = r_t[t-lag_steps-1,2]# + v_t[t-1,2] *.0
    fish_x = r_p[t-lag_steps-1,1]
    fish_y  = r_p[t-lag_steps-1,2]
    x_diff =  fish_x - prey_x
    y_diff = fish_y - prey_y
    pred_BA =-atan2(y_diff,x_diff)
    error_old =  (pred_BA -  h[t-lag_steps-1])
  # Estimate derivative of error angle  
    d_error = (error_new - error_old) / d_t
    
  # Forecast error angle at time t
    pred_error = error_new + d_error * lag_steps * d_t
    
    return(pred_error)
}

