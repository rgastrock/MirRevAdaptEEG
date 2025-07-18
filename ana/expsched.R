source('ana/shared.R')

getTrialList <- function(){
  
  
  #Aligned Session
  #Create data frame containing values
  trial <- c(1:420)
  #trial <- c(1:324)
  
  #1 means aligned cursor
  #2 means random
  #3 means perturb
  #4 means washout
  
  align <- matrix(rep(1,48), nrow = 48, ncol = 1)
  random <- matrix(rep(2,48), nrow = 48, ncol = 1)
  perturb <- matrix(rep(3,90), nrow = 90, ncol = 1)
  washout <- matrix(rep(4,48), nrow = 48, ncol = 1)
  
  
  task <- rbind(align, random, perturb, washout,
              random, perturb, washout)
  # task <- rbind(align, perturb, washout,
  #               perturb, washout)
  sched_df<- data.frame(trial, task)
  return(sched_df)
  
}

plotExpSched <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig0_expsched.svg', width=8.5, height=4, pointsize=14, system_fonts=list(sans="Arial"))#width is 8.5, height is 4, pointsize is 14
  }
  
  #might need to specify different X and Y for each trial
  sched_df <- getTrialList()
  
  X1 <- c(1,48)
  X2 <- c(49,96)
  X3 <- c(97,186)
  X4 <- c(187,234)
  X5 <- c(235,282)
  X6 <- c(283,372)
  X7 <- c(373,420)
  
  Y <- c(0, 1)
  Y1<- c(0, .5)
  Y2<- c(.5, 1)
  
  plot(c(1:length(sched_df$trial)), seq (0,30, length.out = length(sched_df$trial)), type = 'n', axes = FALSE,
       xlab = 'Trial', ylab = '',
       xlim = c(0,421), ylim = c(-0.2,2.5))#, cex.main=.65, cex.lab=.65)
  
  #set variables for colours
  alignedcol <- '#A9A9A9ff'
  randcol1    <- '#ff8200ff'
  randcol2    <- '#c400c4ff'
  rotcol     <- '#e51636ff'
  mircol     <- '#005de4ff'
  
  #specify rects
  rect(X1[1], Y[1], X1[2], Y[2], border = alignedcol, col = alignedcol)
  
  rect(X2[1], Y[1], X2[2], Y[2], border = randcol1, col = randcol1)
  rect(X3[1], Y1[1], X3[2], Y1[2], border = mircol, col = mircol)
  rect(X3[1], Y2[1], X3[2], Y2[2], border = rotcol, col = rotcol)
  rect(X4[1], Y[1], X4[2], Y[2], border = alignedcol, col = alignedcol)
  
  
  rect(X5[1], Y[1], X5[2], Y[2], border = randcol2, col = randcol2)
  rect(X6, Y1[1], X6[2], Y1[2], border = rotcol, col = rotcol)
  rect(X6[1], Y2[1], X6[2], Y2[2], border = mircol, col = mircol)
  rect(X7, Y[1], X7[2], Y[2], border = alignedcol, col = alignedcol)
  
  axis(side=1, at=c(1,49,97,187,235,283,373,420))
  # axis(side=1, at=c(1,48), labels=c('1',''))
  # axis(side=1, at=c(49,138), labels=c('49',''))
  # axis(side=1, at=c(139,186), labels=c('139',''))
  # axis(side=1, at=c(187,276), labels=c('187',''))
  # axis(side=1, at=c(277,324), labels=c('276','324'))
  
  legend(55,2.3,legend=c('Aligned cursor/ washout', 'Random rotation: Early', 'Random rotation: Late', 'Fixed rotation', 'Mirror reversal'),
         col=c(alignedcol,randcol1, randcol2, rotcol,mircol),
         #text.col=c("#000000","#76BA1B","#4C9A2A","#A4DE02",drkbl,lgtbl),
         lty=2,bty='n',lwd=5, cex=.8, ncol=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}


#create tablet experiment axes
plotTabletConditions <- function(conds = c(1,2,3,4), target='inline'){
  
  if (target == 'svg') {
    svglite(file='doc/fig/Fig0A_TabletConditions.svg', width=12, height=4, pointsize=18, system_fonts=list(sans='Arial'))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  layout(matrix(c(1,2,3,4), 1, 4), widths=c(1,1,1,1), heights=c(4,4,4,4))
  
  #Quadrant 1
  targ1 <- as.circular(7.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ2 <- as.circular(15, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ3 <- as.circular(22.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ4 <- as.circular(67.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ5 <- as.circular(75, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ6 <- as.circular(82.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  #Quadrant 2
  targ7 <- as.circular(97.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ8 <- as.circular(105, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ9 <- as.circular(112.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ10 <- as.circular(157.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ11 <- as.circular(165, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ12 <- as.circular(172.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  #Quadrant 3
  targ13 <- as.circular(187.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ14 <- as.circular(195, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ15 <- as.circular(202.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ16 <- as.circular(247.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ17 <- as.circular(255, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ18 <- as.circular(262.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  
  #Quadrant 4
  targ19 <- as.circular(277.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ20 <- as.circular(285, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ21 <- as.circular(292.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ22 <- as.circular(337.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ23 <- as.circular(345, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  targ24 <- as.circular(352.5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
  for(cond in conds){
    if(cond == 1){
      x <- c(-NA, 0, NA)
      plot(x, sin(x), axes=FALSE, xlim = c(-1,1), ylim = c(-1,1), xlab = '', ylab = '', asp=1, main = 'Participant ID: 0 and 1')
      abline(h = 0, col = '#e51636ff', lwd = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= 0, col = '#005de4ff', lwd = 2)
      axis(1, at = 0, labels = 'Mirror', tick = FALSE, col.axis = '#005de4ff') #tick marks for x axis
      axis(2, at = 0, labels = 'Rotation', tick = FALSE, col.axis = '#e51636ff', las=2) #tick marks for y axis 
      
      #Quadrant 1
      points.circular(targ1, pch = 16, col = '#A9A9A9ff')
      points.circular(targ2, pch = 16, col = '#00CED1')
      points.circular(targ3, pch = 16, col = '#c400c4ff')
      points.circular(targ4, pch = 16, col = '#c400c4ff')
      points.circular(targ5, pch = 16, col = '#00CED1')
      points.circular(targ6, pch = 16, col = '#A9A9A9ff')
      #Quadrant 3
      points.circular(targ13, pch = 16, col = '#A9A9A9ff')
      points.circular(targ14, pch = 16, col = '#00CED1')
      points.circular(targ15, pch = 16, col = '#c400c4ff')
      points.circular(targ16, pch = 16, col = '#c400c4ff')
      points.circular(targ17, pch = 16, col = '#00CED1')
      points.circular(targ18, pch = 16, col = '#A9A9A9ff')
      
      arrows.circular(targ7, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ8, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ9, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ10, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ11, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ12, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      
      arrows.circular(targ19, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ20, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ21, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ22, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ23, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ24, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
    } else if (cond == 2){
      x <- c(-NA, 0, NA)
      plot(x, sin(x), axes=FALSE, xlim = c(-1,1), ylim = c(-1,1), xlab = '', ylab = '', asp=1, main = 'Participant ID: 2 and 3')
      abline(h = 0, col = '#e51636ff', lwd = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v= 0, col = '#005de4ff', lwd = 2)
      axis(1, at = 0, labels = 'Mirror', tick = FALSE, col.axis = '#005de4ff') #tick marks for x axis
      axis(2, at = 0, labels = 'Rotation', tick = FALSE, col.axis = '#e51636ff', las=2) #tick marks for y axis
      
      #Quadrant 2
      points.circular(targ7, pch = 16, col = '#A9A9A9ff')
      points.circular(targ8, pch = 16, col = '#00CED1')
      points.circular(targ9, pch = 16, col = '#c400c4ff')
      points.circular(targ10, pch = 16, col = '#c400c4ff')
      points.circular(targ11, pch = 16, col = '#00CED1')
      points.circular(targ12, pch = 16, col = '#A9A9A9ff')
      #Quadrant 4
      points.circular(targ19, pch = 16, col = '#A9A9A9ff')
      points.circular(targ20, pch = 16, col = '#00CED1')
      points.circular(targ21, pch = 16, col = '#c400c4ff')
      points.circular(targ22, pch = 16, col = '#c400c4ff')
      points.circular(targ23, pch = 16, col = '#00CED1')
      points.circular(targ24, pch = 16, col = '#A9A9A9ff')
      
      arrows.circular(targ3, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ2, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ1, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ6, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ5, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ4, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      
      arrows.circular(targ15, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ14, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ13, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ18, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ17, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ16, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
    } else if (cond == 3){
      x <- c(-NA, 0, NA)
      plot(x, sin(x), axes=FALSE, xlim = c(-1,1), ylim = c(-1,1), xlab = '', ylab = '', asp=1, main = 'Participant ID: 4 and 5')
      abline(v = 0, col = '#e51636ff', lwd = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(h= 0, col = '#005de4ff', lwd = 2)
      axis(2, at = 0, labels = 'Mirror', tick = FALSE, col.axis = '#005de4ff', las=2) #tick marks for x axis
      axis(1, at = 0, labels = 'Rotation', tick = FALSE, col.axis = '#e51636ff') #tick marks for y axis
      
      #Quadrant 2
      points.circular(targ7, pch = 16, col = '#A9A9A9ff')
      points.circular(targ8, pch = 16, col = '#00CED1')
      points.circular(targ9, pch = 16, col = '#c400c4ff')
      points.circular(targ10, pch = 16, col = '#c400c4ff')
      points.circular(targ11, pch = 16, col = '#00CED1')
      points.circular(targ12, pch = 16, col = '#A9A9A9ff')
      #Quadrant 4
      points.circular(targ19, pch = 16, col = '#A9A9A9ff')
      points.circular(targ20, pch = 16, col = '#00CED1')
      points.circular(targ21, pch = 16, col = '#c400c4ff')
      points.circular(targ22, pch = 16, col = '#c400c4ff')
      points.circular(targ23, pch = 16, col = '#00CED1')
      points.circular(targ24, pch = 16, col = '#A9A9A9ff')
      
      arrows.circular(targ1, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ2, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ3, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ4, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ5, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ6, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      
      arrows.circular(targ13, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ14, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ15, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ16, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ17, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ18, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
    } else if (cond == 4){
      x <- c(-NA, 0, NA)
      plot(x, sin(x), axes=FALSE, xlim = c(-1,1), ylim = c(-1,1), xlab = '', ylab = '', asp=1, main = 'Participant ID: 6 and 7')
      abline(v = 0, col = '#e51636ff', lwd = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(h= 0, col = '#005de4ff', lwd = 2)
      axis(2, at = 0, labels = 'Mirror', tick = FALSE, col.axis = '#005de4ff', las=2) #tick marks for x axis
      axis(1, at = 0, labels = 'Rotation', tick = FALSE, col.axis = '#e51636ff') #tick marks for y axis
      
      #Quadrant 1
      points.circular(targ1, pch = 16, col = '#A9A9A9ff')
      points.circular(targ2, pch = 16, col = '#00CED1')
      points.circular(targ3, pch = 16, col = '#c400c4ff')
      points.circular(targ4, pch = 16, col = '#c400c4ff')
      points.circular(targ5, pch = 16, col = '#00CED1')
      points.circular(targ6, pch = 16, col = '#A9A9A9ff')
      #Quadrant 3
      points.circular(targ13, pch = 16, col = '#A9A9A9ff')
      points.circular(targ14, pch = 16, col = '#00CED1')
      points.circular(targ15, pch = 16, col = '#c400c4ff')
      points.circular(targ16, pch = 16, col = '#c400c4ff')
      points.circular(targ17, pch = 16, col = '#00CED1')
      points.circular(targ18, pch = 16, col = '#A9A9A9ff')
      
      arrows.circular(targ9, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ8, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ7, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ12, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ11, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ10, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      
      arrows.circular(targ21, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ20, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ19, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
      arrows.circular(targ24, length = 0, angle = 0, lwd=2, col = '#A9A9A9ff')
      arrows.circular(targ23, length = 0, angle = 0, lwd=2, col = '#00CED1')
      arrows.circular(targ22, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
    }
  }
  
  if (target == 'svg') {
    dev.off()
  }
}

# #create online experiment axes
# plotOnlineConditions <- function(conds = c(1,2,3,4), target='inline'){
#   
#   if (target == 'svg') {
#     svglite(file='doc/fig/controlmirgenonline-master/Fig0A_OnlineTargets.svg', width=6, height=6, pointsize=18, system_fonts=list(sans='Arial'))
#   }
#   
#   #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
#   #layout(matrix(c(1,2,3,4), 1, 4), widths=c(1,1,1,1), heights=c(4,4,4,4))
#   
#   #Quadrant 1
#   targ1 <- as.circular(5, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ2 <- as.circular(45, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ3 <- as.circular(85, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   #Quadrant 2
#   targ4 <- as.circular(95, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ5 <- as.circular(135, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ6 <- as.circular(175, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   #Quadrant 4
#   targ7 <- as.circular(275, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ8 <- as.circular(315, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   targ9 <- as.circular(355, type='angles', units='degrees', template = 'none', modulo = 'asis', zero = 0, rotation = 'counter')
#   
#   
#   x <- c(-NA, 0, NA)
#   plot(x, sin(x), axes=FALSE, xlim = c(-1,1), ylim = c(-1,1), xlab = '', ylab = '', asp=1)
#   abline(v= 0, col = '#dadadaff', lwd = 2, lty=2)
#   axis(1, at = 0, labels = 'Mirror', tick = FALSE, col.axis = '#dadadaff') #tick marks for x axis
#   
#   points.circular(targ3, pch = 16, col = '#ff8200ff')
#   points.circular(targ2, pch = 16, col = '#e51636ff')
#   points.circular(targ1, pch = 16, col = '#c400c4ff')
#   points.circular(targ6, pch = 1, col = '#c400c4ff')
#   points.circular(targ5, pch = 1, col = '#e51636ff')
#   points.circular(targ4, pch = 1, col = '#ff8200ff')
#   points.circular(targ7, pch = 1, col = '#ff8200ff')
#   points.circular(targ8, pch = 1, col = '#e51636ff')
#   points.circular(targ9, pch = 1, col = '#c400c4ff')
#   
#   
#   arrows.circular(targ4, length = 0, angle = 0, lwd=2, col = '#ff8200ff')
#   arrows.circular(targ5, length = 0, angle = 0, lwd=2, col = '#e51636ff')
#   arrows.circular(targ6, length = 0, angle = 0, lwd=2, col = '#c400c4ff')
#   
#   
#   
#   if (target == 'svg') {
#     dev.off()
#   }
# }