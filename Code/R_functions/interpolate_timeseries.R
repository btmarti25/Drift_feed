interpolate_timeseries <- function(in_data,  factor) {
  in_data <-data.frame(in_data)

  out_data <- matrix(, 1+factor*(nrow(in_data)-1),ncol(in_data))
  for (i in 1:ncol(in_data)) {
    smooth <- approx(1:nrow(in_data),  in_data[, i], seq(1, nrow(in_data),  length.out = 1+factor * (nrow(in_data)-1)))
    out_data[, i] <- smooth$y
  }
  return(out_data)
}

# # # # df<-data.frame()
# # x<-data.frame(c(1,2,3,1))
# # names(x)<-'x'
# # interpolate_timeseries(x,2)
# xout=interpolate_timeseries(x,4)
  # xout <xout[seq(1,nrow(xout), 4)]
 # in_data=x
 # factor=2

# interpolate_timeseries <- function(in_data,  factor) {
  # out_data <- matrix(, 1+factor*(nrow(in_data)-1),ncol(in_data))
  # for (i in 1:ncol(in_data)) {
    # smooth <- approx(1:nrow(in_data),  in_data[, i], seq(1, nrow(in_data),  length.out = 1+factor * (nrow(in_data)-1)))
    # out_data[, i] <- smooth$y
  # }
  # return(out_data)
# }
