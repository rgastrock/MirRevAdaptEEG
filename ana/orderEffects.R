source('ana/shared.R')
source('ana/learningRates.R')
source('ana/reachAftereffects.R')

#Perturbation Order effects (LEARNING): ROT----

getROTOrderEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompROTGroupLearningCurves(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_ROTfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_ROTsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is neede
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTOrderEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTOrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_ordereffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTOrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_ordereffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotROTOrderEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig47_ROT_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_CI_ordereffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTOrderEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }

  return(ndat_long)
}

plotROTBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig47B_ROT_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('ROT_First','ROT_second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

# Perturbation Order effects (WASHOUT): ROT----

getROTRAEOrderEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_ROTfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_ROTsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_ROTfirst <- cbind(trial, pp_ROTfirst)
  pp_ROTsecond <- cbind(trial, pp_ROTsecond)
  
  #return whichever data is neede
  if (condition == 1){
    return(pp_ROTfirst)
  } else if (condition == 2){
    return(pp_ROTsecond)
  }
  
}

getROTRAEOrderEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTRAEOrderEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_ordereffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTRAEOrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_ordereffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotROTRAEOrderEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig47A_ROT_RAE_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Rotation Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_RAE_CI_ordereffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('ROT_First','ROT_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAEBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTRAEOrderEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotROTRAEBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig47C_ROT_RAE_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('ROT_First','ROT_second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAEBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Rotation first compared to Rotation second (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation first compared to Rotation second (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

#Perturbation Order effects (LEARNING): MIR----

getMIROrderEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompMIRGroupLCALL(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_MIRfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_MIRsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRfirst <- cbind(trial, pp_MIRfirst)
  pp_MIRsecond <- cbind(trial, pp_MIRsecond)
  
  #return whichever data is neede
  if (condition == 1){
    return(pp_MIRfirst)
  } else if (condition == 2){
    return(pp_MIRsecond)
  }
  
}

getMIROrderEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIROrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_ordereffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIROrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_ordereffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIROrderEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig48_MIR_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Mirror", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_CI_ordereffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIROrderEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig48B_MIR_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('MIR_First','MIR_second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}

# Perturbation Order effects (WASHOUT): MIR----

getMIRRAEOrderEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #even ID gets ROT first, then MIR; odd ID gets MIR first, then ROT
  #but participant count starts at 0, whereas dat already starts at one
  #so at this point, odd numbers should have ROT first, while even have ROT second
  pp_MIRfirst <- dat[,c(seq(1,ncol(dat),2))]
  pp_MIRsecond <- dat[,c(seq(2,ncol(dat),2))]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_MIRfirst <- cbind(trial, pp_MIRfirst)
  pp_MIRsecond <- cbind(trial, pp_MIRsecond)
  
  #return whichever data is neede
  if (condition == 1){
    return(pp_MIRfirst)
  } else if (condition == 2){
    return(pp_MIRsecond)
  }
  
}

getMIRRAEOrderEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRRAEOrderEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_ordereffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIRRAEOrderEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_ordereffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIRRAEOrderEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig48A_MIR_RAE_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Perturbation order: Mirror Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_RAE_CI_ordereffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('MIR_First','MIR_Second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAEBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIRRAEOrderEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRRAEBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig48C_MIR_RAE_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('MIR_First','MIR_second'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAEBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRRAEBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Mirror first compared to Mirror second (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror first compared to Mirror second (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}

#Target location effects (LEARNING): ROT----

getROTTargetEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompROTGroupLearningCurves(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get rot after axis, 2,3 get rot before axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling after axis, condition 2 will be before
  pp_ROTaft<- dat[,c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30)] #targets fall after axis
  pp_ROTbef <- dat[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32)] #targets fall before axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROTaft <- cbind(trial, pp_ROTaft)
  pp_ROTbef <- cbind(trial, pp_ROTbef)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTaft)
  } else if (condition == 2){
    return(pp_ROTbef)
  }
  
}

getROTTargetEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTTargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_targeteffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTTargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_targeteffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotROTTargetEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig49_ROT_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target location: Rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_CI_targeteffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_after','ROT_before'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTTargetBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTTargetEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotROTTargetBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig49B_ROT_Target_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('ROT_after','ROT_before'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTTargetBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis (second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis(last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

# Target location effects (WASHOUT): ROT----

getROTRAETargetEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get rot after axis, 2,3 get rot before axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling after axis, condition 2 will be before
  pp_ROTaft<- dat[,c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30)] #targets fall after axis
  pp_ROTbef <- dat[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32)] #targets fall before axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_ROTaft <- cbind(trial, pp_ROTaft)
  pp_ROTbef <- cbind(trial, pp_ROTbef)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROTaft)
  } else if (condition == 2){
    return(pp_ROTbef)
  }
  
}

getROTRAETargetEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTRAETargetEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_targeteffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTRAETargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_targeteffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotROTRAETargetEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig49A_ROT_RAE_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target location: Rotation Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_RAE_CI_targeteffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('ROT_after','ROT_before'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAETargetBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTRAETargetEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotROTRAETargetBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig49C_ROT_RAE_Target_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('ROT_after','ROT_before'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAETargetBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis (second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Rotation before axis compared to after axis (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation before axis compared to after axis (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

#Target location effects (LEARNING): MIR----

getMIRTargetEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompMIRGroupLCALL(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get mir before axis, 2,3 get mir after axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling before axis, condition 2 will be after
  pp_MIRbef<- dat[,c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30)] #targets fall before axis
  pp_MIRaft <- dat[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32)] #targets fall after axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRbef <- cbind(trial, pp_MIRbef)
  pp_MIRaft <- cbind(trial, pp_MIRaft)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRbef)
  } else if (condition == 2){
    return(pp_MIRaft)
  }
  
}

getMIRTargetEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRTargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_targeteffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIRTargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_targeteffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIRTargetEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig50_MIR_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target location: Mirror", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_CI_targeteffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_before','MIR_after'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRTargetBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIRTargetEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRTargetBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig50B_MIR_Target_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('MIR_before','MIR_after'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRTargetBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRTargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis (first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}

# Target location effects (WASHOUT): MIR----

getMIRRAETargetEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1 get mir before axis, 2,3 get mir after axis and so on
  #but participant count starts at 0, whereas dat already starts at one
  #so condition 1 will be those falling before axis, condition 2 will be after
  pp_MIRbef<- dat[,c(1,2,5,6,9,10,13,14,17,18,21,22,25,26,29,30)] #targets fall before axis
  pp_MIRaft <- dat[,c(3,4,7,8,11,12,15,16,19,20,23,24,27,28,31,32)] #targets fall after axis
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_MIRbef <- cbind(trial, pp_MIRbef)
  pp_MIRaft <- cbind(trial, pp_MIRaft)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRbef)
  } else if (condition == 2){
    return(pp_MIRaft)
  }
  
}

getMIRRAETargetEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRRAETargetEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_targeteffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIRRAETargetEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_targeteffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIRRAETargetEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig50A_MIR_RAE_targeteffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Target location: Mirror Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_RAE_CI_targeteffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('MIR_before','MIR_after'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAETargetBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIRRAETargetEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRRAETargetBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig50C_MIR_RAE_Target_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('MIR_before','MIR_after'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAETargetBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRRAETargetBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis(first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Mirror before axis compared to after axis (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror before axis compared to after axis (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}

#Axis orientation effects (LEARNING): ROT----

getROTAxisEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompROTGroupLearningCurves(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets ROT on HOR axis, 4,5,6,7 gets ROT on VER axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be horizontal, second would be vertical
  pp_ROThor <- dat[,c(1,2,3,4,9,10,11,12,17,18,19,20,25,26,27,28)]
  pp_ROTver <- dat[,c(5,6,7,8,13,14,15,16,21,22,23,24,29,30,31,32)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_ROThor <- cbind(trial, pp_ROThor)
  pp_ROTver <- cbind(trial, pp_ROTver)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROThor)
  } else if (condition == 2){
    return(pp_ROTver)
  }
  
}

getROTAxisEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_axiseffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_CI_axiseffects_2.csv', row.names = F) 
      
      
    }
  }
}

plotROTAxisEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig51_ROT_ordereffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis orientation: Rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_CI_axiseffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('ROT_horizontal','ROT_vertical'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTAxisBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTAxisEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotROTAxisBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig51B_ROT_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('ROT_horizontal','ROT_vertical'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTAxisBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis (second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

# Axis orientation effects (WASHOUT): ROT----

getROTRAEAxisEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getROTGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets ROT on HOR axis, 4,5,6,7 gets ROT on VER axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be horizontal, second would be vertical
  pp_ROThor <- dat[,c(1,2,3,4,9,10,11,12,17,18,19,20,25,26,27,28)]
  pp_ROTver <- dat[,c(5,6,7,8,13,14,15,16,21,22,23,24,29,30,31,32)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_ROThor <- cbind(trial, pp_ROThor)
  pp_ROTver <- cbind(trial, pp_ROTver)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_ROThor)
  } else if (condition == 2){
    return(pp_ROTver)
  }
  
}

getROTRAEAxisEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getROTRAEAxisEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_axiseffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getROTRAEAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/ROT_RAE_CI_axiseffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotROTRAEAxisEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig51A_ROT_RAE_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis orientation: Rotation Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ROT_RAE_CI_axiseffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('ROT_horizontal','ROT_vertical'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAEAxisBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getROTRAEAxisEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotROTRAEAxisBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig51C_ROT_RAE_Axis_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getROTRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('ROT_horizontal','ROT_vertical'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getROTRAEAxisBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat1 <- getROTRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  ROTdat1$ptype <- 1
  ROTdat2 <- getROTRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  ROTdat2$ptype <- 2
  LC4test <- rbind(ROTdat1, ROTdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis (first block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (first block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare second block
  ROTdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis(second block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (second block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
  #compare last block
  ROTdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  ROTdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Rotation horizontal compared to vertical axis (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdat2$reachdev))
  cat('Bayesian t-test Rotation horizontal compared to vertical axis (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdat2$reachdev, paired = F))
  
}

#Axis orientation effects (LEARNING): MIR----

getMIRAxisEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getPercCompMIRGroupLCALL(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets MIR on VER axis, 4,5,6,7 gets MIR on HOR axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be vertical, second would be horizontal
  pp_MIRver <- dat[,c(1,2,3,4,9,10,11,12,17,18,19,20,25,26,27,28)]
  pp_MIRhor <- dat[,c(5,6,7,8,13,14,15,16,21,22,23,24,29,30,31,32)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,90,1)
  pp_MIRver <- cbind(trial, pp_MIRver)
  pp_MIRhor <- cbind(trial, pp_MIRhor)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRver)
  } else if (condition == 2){
    return(pp_MIRhor)
  }
  
}

getMIRAxisEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_axiseffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIRAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_CI_axiseffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIRAxisEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig52_MIR_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis orientation: Mirror", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_CI_axiseffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(60,-100,legend=c('MIR_vertical','MIR_horizontal'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRAxisBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIRAxisEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRAxisBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig52B_MIR_Axis_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(10,-90,legend=c('MIR_vertical','MIR_horizontal'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRAxisBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 15 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}

# Axis orientationeffects (WASHOUT): MIR----

getMIRRAEAxisEffects <- function(maxppid, location, condition){
  
  #each column will be one participant and their 90 trials
  dat <- getMIRGroupAftereffects(maxppid=maxppid,location=location)
  dat <- dat[,-1] #remove trial column to index properly
  
  #0,1,2,3 gets MIR on VER axis, 4,5,6,7 gets MIR on HOR axis
  #but participant count starts at 0, whereas dat already starts at one
  #first condition would be vertical, second would be horizontal
  pp_MIRver <- dat[,c(1,2,3,4,9,10,11,12,17,18,19,20,25,26,27,28)]
  pp_MIRhor <- dat[,c(5,6,7,8,13,14,15,16,21,22,23,24,29,30,31,32)]
  #then we can add trial column back to each of the separated data
  trial <- seq(1,48,1)
  pp_MIRver <- cbind(trial, pp_MIRver)
  pp_MIRhor <- cbind(trial, pp_MIRhor)
  
  #return whichever data is needed
  if (condition == 1){
    return(pp_MIRver)
  } else if (condition == 2){
    return(pp_MIRhor)
  }
  
}

getMIRRAEAxisEffectsConfidenceInterval <- function(maxppid, location, condition, type){
  
  if (condition == 1){
    data <- getMIRRAEAxisEffects(maxppid = maxppid, location = location, condition = condition)
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_axiseffects_1.csv', row.names = F) 
      
      
    }
  }else if (condition == 2){
    data <- getMIRRAEAxisEffects(maxppid = maxppid, location = location, condition = condition)
    #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    confidence <- data.frame()
    
    
    for (trial in trialno){
      cireaches <- data1[which(data$trial == trial), ]
      
      if (type == "t"){
        cireaches <- cireaches[!is.na(cireaches)]
        citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
      write.csv(confidence, file='data/MIR_RAE_CI_axiseffects_2.csv', row.names = F) 
      
      
    }
  }
  
}

plotMIRRAEAxisEffects <- function(conditions = c(1,2), target = 'inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig52A_MIR_RAE_axiseffects.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,49), ylim = c(-200,200), 
       xlab = "Trial", ylab = "Amount of Compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Axis orientation: Mirror Washout", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 48)) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
  
  for(condition in conditions){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_RAE_CI_axiseffects_%d.csv', condition))
    
    colourscheme <- getCtypeColourScheme(conditions = condition)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[condition]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[condition]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (condition in conditions) {
    # plot mean reaches for each group
    col <- colourscheme[[condition]][['S']]
    lines(meanGroupReaches[[condition]],col=col,lty=1)
  }
  
  #add legend
  legend(15,-100,legend=c('MIR_vertical','MIR_horizontal'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAEAxisBlocked <- function(maxppid = 31, location = 'feedback', targetno = 6, conditions = c(1,2)){
  for (cond in conditions){
    data <- getMIRRAEAxisEffects(maxppid = maxppid, location = location, condition = cond)
    
    #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
    n <- targetno;
    ndat <- aggregate(data, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
    ndat$trial <- c(1:length(ndat$trial))
    
    #but data is in wide format, we would want it in long format
    #this requires library(tidyr)
    ndat_long <- gather(ndat, participant, reachdev, reaches:length(ndat))
    ndat_long$participant <- as.character(ndat_long$participant)
    
    participants <- unique(ndat_long$participant)
    
    
    PPindex <- 0
    
    for (pp in participants) {
      row.idx <- which(ndat_long$participant == pp)
      ndat_long$participant[row.idx] <- sprintf('pp%d', PPindex)
      
      PPindex <- PPindex + 1
    }
  }
  
  return(ndat_long)
}

plotMIRRAEAxisBlocked <- function(maxppid=31, location='feedback', targetno=6, conditions=c(1,2), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig52C_MIR_RAE_Axis_BlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(cond in conditions){
    data <- getMIRRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = cond)
    
    participants <- unique(data$participant)
    
    colourscheme <- getCtypeColourScheme(conditions=cond)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[cond]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(cond == 1){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (cond == 2){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[cond]][['S']]
      if(cond == 1){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(cond == 2){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(cond == 1){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(cond == 2){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('MIR_vertical','MIR_horizontal'),
         col=c(colourscheme[[1]][['S']],colourscheme[[2]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getMIRRAEAxisBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  MIRdat1 <- getMIRRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 1)
  MIRdat1$ptype <- 1
  MIRdat2 <- getMIRRAEAxisBlocked(maxppid = maxppid, location = location, targetno = targetno, conditions = 2)
  MIRdat2$ptype <- 2
  LC4test <- rbind(MIRdat1, MIRdat2)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (first block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (first block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare second block
  MIRdat1 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (second block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (second block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
  #compare last block
  MIRdat1 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 1),]
  MIRdat2 <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 2),]
  
  cat('Mirror horizontal compared to vertical axis (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdat2$reachdev))
  cat('Bayesian t-test Mirror horizontal compared to vertical axis (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdat2$reachdev, paired = F))
  
}
