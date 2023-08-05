## Load libraries
library(ggplot2)
library(cowplot)
library(Rmisc)
library(reshape2)
library(scales)

setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")

source("Code/R_functions/interpolate_timeseries.R")
source("Code/R_functions/setup_track.R")
source("Code/R_functions/get_dBA.R")
source("Code/R_functions/get_dBA_exp.R")
source("Code/R_functions/lag_timeseries.R")
source("Code/R_functions/get_pred_DBA.R")
source("Code/R_functions/run_sim.R")
source("Code/R_functions/get_pred_BA.R")
source("Code/R_functions/anglediff.R")

## Set parameters
dt <- 0.0005 # timestep
lag <- 0.042  # sensory-motor delay (s)
lag_steps <- lag / dt # sensory-motor delay (steps)

N_sim <- 1000 # Number of simulations per gain
maneuver_time <- runif(N_sim, min = lag, max = 3 * lag) # Vary time of maneuver between 1 and 3 SM delays until collision
maneuver_angle <- runif(N_sim, min = pi / 5, max = pi / 3) # Escape angle after maneuver

kvect <- exp(seq(-2, 2, 0.1)) #range of gains (factor change from fitted gain for each model)
s_p <- runif(N_sim, min = 90, max = 120) # predator speed
s_t <- s_p * runif(N_sim, min = 0.6, max = 0.8) # prey speed as fraction of predator speed

tMax <- 2 # max simulation time (seconds)

## initialize vectors to store miss distance data for each model
missPN <- list()
misspPN <- list()
misspPP <- list()
meanPN <- numeric(length(kvect))
meanpPN <- numeric(length(kvect))
meanpPP <- numeric(length(kvect))
cap_PN <- numeric(length(kvect))
cap_pPN <- numeric(length(kvect))
cap_pPP <- numeric(length(kvect))

invlogit <- function(x) {
  exp(x) / (1 + exp(x))
}

## Perform simulations
for (j in 1:length(kvect)) {
  print(j / length(kvect))
  for (i in 1:N_sim) {
    out <- run_sim(model = "PN", lag, dt, s_p[i], s_t[i], maneuver_time[i], maneuver_angle[i], kvect[j] * 0.95, tMax, f_time = lag)
    missPN[[i]] <- out[[5]]
    
    out <- run_sim(model = "pPN", lag, dt, s_p[i], s_t[i], maneuver_time[i], maneuver_angle[i], kvect[j] * 0.75, tMax, f_time = lag)
    misspPN[[i]] <- out[[5]]
    
    out <- run_sim(model = "pPP", lag, dt, s_p[i], s_t[i], maneuver_time[i], maneuver_angle[i], kvect[j] * 57.5, tMax, f_time = lag)
    misspPP[[i]] <- out[[5]]
  }
  
  meanPN[j] <- mean(unlist(missPN))
  meanpPN[j] <- mean(unlist(misspPN))
  meanpPP[j] <- mean(unlist(misspPP))
  
  cap_PN[j] <- sum(missPN <1.5)/N_sim #mean(invlogit(5.67 - 4.24 * abs(unlist(missPN))))
  cap_pPN[j] <- sum(misspPN <1.5)/N_sim  #mean(invlogit(5.67 - 4.24 * abs(unlist(misspPN))))
  cap_pPP[j] <- sum(misspPP <1.5)/N_sim #mean(invlogit(5.67 - 4.24 * abs(unlist(misspPP))))
}

## Create data frames for plotting
fitdf <- data.frame(kvect,  PN = meanPN,
                    pPN = meanpPN,
                    pPP = meanpPP
)

sddf <- data.frame(
  kvect,
  cap_PN,
  cap_pPN,
  cap_pPP
)

df_out <- melt(fitdf, id.vars = "kvect")
sd_out <- melt(sddf, id.vars = "kvect")
df_out$sd <- sd_out$value

## Plot Figure 1
plot1 <- ggplot(subset(df_out,variable != "PP"), aes(x = kvect, y = sd, group = variable)) +
  geom_line(aes(color = variable), alpha = 0.8, size = 1.5) +
  ylab("Capture success") +
  xlab("gain") +
  scale_x_continuous(
    trans = log2_trans(),
    breaks = c(1/8, 1/4, 0.5, 1, 2, 4, 8),
    labels = c(1/8, 1/4, 0.5, 1, 2, 4, 8)
  ) +
  scale_color_viridis_d(option = "C", end = 0.7, direction = -1) +
  theme_cowplot()

## Plot Figure 2
plot2 <- ggplot(subset(df_out,variable != "pP"), aes(x = kvect, y = value, group = variable)) +
  geom_line(aes(color = variable), alpha = 0.8, size = 2) +
  ylab("Mean miss distance (cm)") +
  xlab("gain") +
  geom_vline(aes(xintercept = 1)) +
  scale_x_continuous(
    trans = log2_trans(),
    breaks = c(1/8, 1/4, 0.5, 1, 2, 4, 8),
    labels = c(1/8, 1/4, 0.5, 1, 2, 4, 8)
  ) +
  scale_color_viridis_d(option = "C", end = 0.7, direction = -1) +
  theme_cowplot()

## Arrange and display the plots
plot_grid(plot1, plot2, ncol = 1)


#setwd("Final_figs")

# Get the current date and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Format the date and time
formatted_date <- format(current_date, "%Y-%m-%d")
formatted_time <- format(current_time, "%H-%M-%S")

# Create the filename
filename <- paste0("Final_figs/Fig3D_", formatted_date, "_", formatted_time, ".png")
ggsave(filename,
       width = 4.5,
       height =6
)                    