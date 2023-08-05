get_pred_BA <- function(r_t, v_t, r_p, v_p, t, lag_steps, dt, f_time) {

    pred_prey_x = r_t[t - lag_steps, 1] + v_t[t - lag_steps, 1] * f_time
    pred_prey_y = r_t[t - lag_steps, 2] + v_t[t - lag_steps, 2] * f_time

    pred_fish_x = r_p[t - 1, 1]
    pred_fish_y = r_p[t - 1, 2]
 

  # Calculate the differences in x and y coordinates between the predator and prey (or predicted prey)
  x_diff = pred_fish_x - pred_prey_x
  y_diff = pred_fish_y - pred_prey_y

  # Compute the predicted Bearing Angle using the atan2 function
  pred_BA = -atan2(y_diff, x_diff)

  # Return the calculated predicted Bearing Angle value
  return(pred_BA)
}
