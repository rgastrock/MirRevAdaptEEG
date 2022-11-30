source('ana/shared.R')

getTrialList <- function(){
  
  
  #Aligned Session
  #Create data frame containing values
  trial <- c(1:186)
  
  #1 means aligned cursor
  #2 means random
  #3 means perturb
  #4 means washout
  
  align <- matrix(rep(1,48), nrow = 48, ncol = 1)
  random <- matrix(rep(2,48), nrow = 48, ncol = 1)
  perturb <- matrix(rep(3,90), nrow = 90, ncol = 1)
  
  
  task <- rbind(align, random, perturb)
  sched_df<- data.frame(trial, task)
  return(sched_df)
  
}

plotExpSched<- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig0_expsched.svg', width=8.5, height=4, pointsize=14, system_fonts=list(sans="Arial"))#width is 8.5, height is 4, pointsize is 14
  }
  
  #might need to specify different X and Y for each trial
  sched_df <- getTrialList()
  
  X1 <- c(1,48)
  X2 <- c(49,96)
  X3 <- c(97,186)
  
  
  Y <- c(0, 1)
  
  plot(c(1:length(sched_df$trial)), seq (0,30, length.out = length(sched_df$trial)), type = 'n', axes = FALSE,
       xlab = 'Trial', ylab = '',
       xlim = c(0,187), ylim = c(-0.2,2.5))#, cex.main=.65, cex.lab=.65)
  
  #set variables for colours
  alignedcol <- '#005de4ff'
  random    <- '#c400c4ff'
  perturbation    <- '#e51636ff'
  
  #specify rects
  rect(X1[1], Y[1], X1[2], Y[2], border = alignedcol, col = alignedcol)
  
  rect(X2[1], Y[1], X2[2], Y[2], border = random, col = random)
  rect(X3[1], Y[1], X3[2], Y[2], border = perturbation, col = perturbation)
  
  
  axis(side=1, at=c(1,48), labels=c('1','48'))
  axis(side=1, at=c(49,96), labels=c('1','48'))
  axis(side=1, at=c(97,186), labels=c('1','90'))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}