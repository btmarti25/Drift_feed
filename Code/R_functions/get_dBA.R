get_dBA <- function(prey_x,prey_y,prey_dx,prey_dy,fish_x,fish_y,fish_dx,fish_dy) {
  ## get drag coef, wetted area, and mass from fish length
  x_diff =  fish_x - prey_x
  y_diff =  prey_y -fish_y
  dx_diff = prey_dx - fish_dx
  dy_diff = prey_dy - fish_dy
  dBA = (x_diff * dy_diff - y_diff * dx_diff)/(x_diff^2+y_diff^2)
  BA =atan2(y_diff,x_diff)
  return(list("BA"=-BA,"DBA"=-dBA))
}
