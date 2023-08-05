run_sim <- function(model, lag, dt, s_p, s_t, maneuver_time, maneuver_angle, k, tMax, f_time) {
  
  lag_steps <- lag / dt
  
  # Initialize target and predator positions and velocity
  r_t <- cbind(rep(NA, tMax/dt), rep(NA, tMax/dt))
  r_p <- cbind(rep(NA, tMax/dt), rep(NA, tMax/dt))
  v_t <- cbind(rep(NA, tMax/dt), rep(NA, tMax/dt))
  v_p <- cbind(rep(NA, tMax/dt), rep(NA, tMax/dt))
  
  r_t[1:(lag_steps+2), ] <- 0
  r_p[1:(lag_steps+2), 1] <- 30
  r_p[1:(lag_steps+2), 2] <- 0
  v_t[1:(lag_steps+2), ] <- 0
  v_p[1:(lag_steps+2), 1] <- -s_p
  v_p[1:(lag_steps+2), 2] <- 0
  
  h <- rep(0, lag_steps+2)
  man_frame<-NA
  t <- lag_steps+2
  r <- sqrt(sum((r_t[t, ] - r_p[t, ])^2))
  min_r <- r
  dh <- 0
  sensory_update_rate <- 60 #Hz
  sense_frame = floor(runif(1,max=floor(1/sensory_update_rate*(1/dt))))
  update_rate_frames = round(1/sensory_update_rate*(1/dt))
  
  repeat {
    
    if (t %% update_rate_frames == sense_frame){
      
    if (model == "PN") {
      LOS <- get_dBA_exp(prey_x = r_t[t-lag_steps, 1],
                         prey_y = r_t[t-lag_steps, 2],
                         prey_dx = v_t[t-lag_steps, 1],
                         prey_dy = v_t[t-lag_steps, 2],
                         fish_x = r_p[t-lag_steps, 1],
                         fish_y = r_p[t-lag_steps, 2],
                         fish_dx = v_p[t-lag_steps, 1],
                         fish_dy = v_p[t-lag_steps, 2])
      
      dh <- k * LOS$DBA
    } else if (model == "pPN") {
      pDBA <- get_pred_DBA(r_t, v_t, r_p, v_p, t, lag_steps, dt, f_time)
      dh <- k * pDBA
    } else if (model == "pPP") {
      # Predictive pursuit
      predBA <- get_pred_BA(r_t, v_t, r_p, v_p, t, lag_steps, dt, f_time)
      dh <- 1 * k * anglediff(predBA, h[t])
    }
    }
    h[t+1] <- h[t] + dh * dt
    
    v_p[t+1, ] <- s_p * c(-cos(h[t+1]), sin(h[t+1]))
    r_p[t+1, ] <- r_p[t, ] + v_p[t+1, ] * dt
    v_t[t+1, ] <- c(0, 0)
    
    if ((r / s_p) < maneuver_time) {
      if (is.na(man_frame) == 1) {
        man_frame <- t
      }
      v_t[t+1, ] <- s_t * c(cos(maneuver_angle), sin(maneuver_angle))
    }
    
    r_t[t+1, ] <- r_t[t, ] + v_t[t+1, ] * dt
              
    r = sqrt(sum((r_t[t+1,] - r_p[t+1,])^2))
    min_r = min(r,min_r)
    t = t + 1
    if (t > (tMax/dt) - 1) {
      break
    }
    
    if (r > 2 * min_r) {
      break
    }
  }
out<-list(r_t,v_t,r_p,v_p,min_r,h,t)
}



