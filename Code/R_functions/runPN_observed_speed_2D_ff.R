runPN_observed_speed_2D_ff <-
  function(N,
           v_f,
           r_p,
           r_t,
           v_t ,
           d_t,
           tau,
           startRow,
           endRow,
           integrationFactor,
           speed,
           h,
           v_p,
           flow_ref,
           pred_DBA,
           ID,
           f_time) {
    #Initializes matrices for pred's simulated position and velocity
    sensory_update_rate = 60 #Hz
    if (flow_ref == 1) {
      v_f = 0
    }

    r_m <- r_p * NA
    r_m[1:startRow,] = r_p[1:startRow,]
    v_m = NA * r_m
    v_m[1:startRow,] =  v_p[1:startRow, ]
    v_m[1:startRow, 1] =   -1 * v_m[1:startRow, 1]
    a_m = v_m * NA
    #dh = c(NA, diff(h) * 100)
    predBA = NA 
    dBAout = NA * h
    dh = -atan2(v_m[startRow,2],-v_m[startRow,1])
    
    sense_frame = floor(runif(1,max=floor(1/sensory_update_rate*fps*integration_factor)))
    update_rate_frames = round(1/sensory_update_rate*fps*integration_factor)
    # integrate interception model
    for (t in startRow:endRow) {
     # startRow %% round(1/30*fps*integration_factor
      if (t %% update_rate_frames == sense_frame){
      # Prop Nav
      if (pred_DBA == "PN") {
       
        dBA <-
          get_dBA_exp( prey_x = r_t[t - tau, 1],
                       prey_y = r_t[t - tau, 2],
                       prey_dx = v_t[t - tau, 1],
                       prey_dy = v_t[t - tau, 2],
                       fish_x = r_m[t - tau, 1],
                       fish_y = r_m[t - tau, 2],
                       fish_dx = v_m[t - tau, 1],
                      fish_dy = v_m[t - tau, 2])

        dh = N * dBA$DBA
        dBAout[t] = dBA$DBA
        
      } else if (pred_DBA == "pPN") {
        # predictive prop nav
        pDBA <- get_pred_DBA(r_t, v_t, r_p, v_m, t, tau, d_t,  f_time,update_rate_frames)
        dh = N * pDBA
        #dBAout[t] = pDBA
      }  else if (pred_DBA == "PP") {
        #proportional pursuit
        predBA[t]   <-
          get_pred_BA(r_t, v_t, r_m, v_p, t, tau, d_t, f_time)
        dh = 20 * N * (predBA[t] -  h[t - tau])
      } else if (pred_DBA == "pPP") {
        #predictive pursuit using effecence copy of self motion
        predBA[t]   <-
          get_pred_BA(r_t, v_t, r_m, v_p, t, tau, d_t, f_time)
        dh = 50 * N * (predBA[t] -  h[t - 1])
      } else if (pred_DBA == "pPP2") {
        #predictive pursuit based on taylor expansion of error angle
        pred_error_angle   <-
          get_pred_error_angle(r_t, v_t, r_m, v_p, t, tau, d_t, h)
        dh = 50 * N * pred_error_angle
      }
      }
      h[t] = h[t - 1] + dh * d_t
      v_m[t, ]  = speed[t] * c(-cos(h[t]), sin(h[t]), 0)
      r_m[t, ] = r_m[t - 1, ] + (v_m[t, ]  + v_f) * d_t

    }
    r_m <- r_m[seq(1, nrow(r_m), integrationFactor), ]
    v_m <- v_m[seq(1, nrow(v_m), integrationFactor), ]
    a_m <- a_m[seq(1, nrow(a_m), integrationFactor), ]
    predBA <- predBA[seq(1, length(predBA), integrationFactor)]
    h <- h[seq(1, length(h), integrationFactor)]
    dBAout <- dBAout[seq(1, nrow(dBAout), integrationFactor), ]

    output = list(r_m, v_m, a_m, h, predBA, dBAout)
    return(output)
  }
