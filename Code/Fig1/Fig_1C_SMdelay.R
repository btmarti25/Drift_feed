
setwd("/Users/benmartin/Library/CloudStorage/GoogleDrive-btmarti25@gmail.com/My Drive/Projects/David_Driftfeed/Analysis/CB_code")
  library(signal)
  library(segmented)
  library(cowplot)
  library(viridis)
  library(ggplot2)
  source("Code/R_functions/summarySE.R")
  
  df <- read.csv("Data/Combined_track_data.csv")

  
  
  # Function to fit a piecewise linear model with a fixed breakpoint
  piecewise_lm <- function(df, x, y, b) {
    # df: data frame containing the variables
    # x: predictor variable (as a string)
    # y: dependent variable (as a string)
    # b: breakpoint value
    
    # Define a new variable that is equal to x - b for x > b and 0 otherwise
    df$delta <- ifelse(df[[x]] > b, df[[x]] - b, 0)
    
    # Fit a linear model with an interaction between x and the new variable
    # This gives a model with a different slope before and after the breakpoint
    model <- lm(formula(paste(y, "~", x, "+ delta")), data = df)
    
    # Return the model
    return(model)
  }
  
  
  df$veer_frame = df$frame-df$change_frame
  
  sdf<- subset(df,  frame > (change_frame-5) & frame < (change_frame+10) & type !="DoubleVeer")
  
  
  ssdf<-  subset(sdf, is.na(fish_dy) !=1)
  splitData<-split(ssdf,ssdf$ID)

  # Usage
  out_er={}
  ID={}
  b_vect = seq(0,8,.2)
  dataout<-{}
  
  for (j in 1:length(b_vect)){
  b<- b_vect[j] 
  sq_err <- {}
  for (i in 1:length(splitData)){
    
  ddf <- data.frame(x =splitData[[i]]$veer_frame,y= splitData[[i]]$vel_heading)   
  model <- piecewise_lm(ddf, "x", "y", b)
  sq_err[i] = mean(summary(model)$residuals^2)
  ID[i] = splitData[[i]]$ID[1]
  }
  temp<-data.frame(sq_err,ID)
  temp$b<-b
  dataout <- rbind(dataout,temp)
}


df_fit_h <- summarySE(dataout, measurevar="sq_err", groupvars=c("b"))

ggplot(df_fit_h,aes(x=b*10,y=sq_err))+geom_line(lwd=1.5)+theme_cowplot()+
  ylab("mean squared error")+
  xlab("Assumed sensory-motor delay (ms)")+
  geom_vline(xintercept = 42,linetype="dashed",lwd=1)


### Method 2
df$s_frame=df$frame-df$change_frame
df$abs_dy_fish= df$fish_dy * -df$veer_direction

sdf<- subset(df,  frame > (change_frame-1) & frame < (change_frame+10) & type =="Veer")


ssdf<-  subset(sdf, is.na(abs_dy_fish) !=1)
tgc <- summarySE(ssdf, measurevar="abs_dy_fish", groupvars="s_frame")

ggplot(tgc,aes(x=s_frame*10,y=-abs_dy_fish))+  
  geom_errorbar(aes(ymin=abs_dy_fish-2*se, ymax=abs_dy_fish+2*se), colour="black", width=.1) + 
  geom_point(aes(x=10*s_frame,y=abs_dy_fish))+theme_cowplot()+
  xlab("time relative to stimulus maneuver (ms)")+ylab(" turning rate (rad/s)")+
  geom_hline(aes(yintercept=0))  