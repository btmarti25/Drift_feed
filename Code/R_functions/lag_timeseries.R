lag_timeseries <- function(timeseries,lag_steps) {
  ## get drag coef, wetted area, and mass from fish length
lag_timeseries<-c(rep(NA,lag_steps),timeseries[1:(length(timeseries)-lag_steps)])
return(lag_timeseries)
}
