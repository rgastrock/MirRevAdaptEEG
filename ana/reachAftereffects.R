source('ana/shared.R')
source('ana/learningRates.R')

#Reach Aftereffects: ROT -----

getROTParticipantAftereffects <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    washoutTrials <- getParticipantTaskData(id, taskno = 13, task = 'washout1')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    washoutTrials <- getParticipantTaskData(id, taskno = 7, task = 'washout0')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 48 trials
  WT<- getReachAngles(washoutTrials, starttrial=0, endtrial=47, location = location) #washout is same length as aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    WT$reachdev[which(WT$targetangle == target)] <- WT$reachdev[which(WT$targetangle == target)] - bias
  }
  
  #rotation was not counterbalanced for this experiment, therefore we multiply everything by -1
  #since rotation was always CCW
  
  alltargetsbef <- c(67.5, 75, 82.5,
                     157.5, 165, 172.5,
                     247.5, 255, 262.5,
                     337.5, 345, 352.5) #should compensate for 30 degrees
  alltargetsaft <- c(7.5, 15, 22.5,
                     97.5, 105, 112.5,
                     187.5, 195, 202.5,
                     277.5, 285, 292.5) #compensate 30 degrees
  
  angles <- unique(WT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      WT$reachdev[which(WT$targetangle == target)] <- (((WT$reachdev[which(WT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  return(WT)
}

getROTGroupAftereffects <- function(maxppid, location) {

  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    ppangles <- getROTParticipantAftereffects(id=participant, location = location) #for every participant, get aftereffects data
    
    
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
    
  }
  return(dataoutput)
}

getROTGroupRAEConfidenceInterval <- function(maxppid = 31, location = 'feedback', type = 'b'){

  data <- getROTGroupAftereffects(maxppid = maxppid, location = location)

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
    
    write.csv(confidence, file='data/RAE_Rot_CI.csv', row.names = F) 
   
    
  }
  #}
}

#Reach Aftereffects: MIR----
getMIRParticipantAftereffects <- function(id, location){
  #same as rotation, for now we keep measures to degrees for eeg matching
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 7, task = 'washout0')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 13, task = 'washout1')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
    
  }
  #after baseline correction, we need to assign specific targets to corresponding magnitudes to compensate
  #we have 24 possible targets, but they differ depending on which side of mirror axis they are (before or after mirror)
  #this will affect calculations later on (due to negative values)
  #so we separate them by amount of compensation, and whether they are before or after mirror axis
  alltargets15bef <- c(82.5, 172.5, 262.5, 352.5) #should compensate for 15 degrees
  alltargets15aft <- c(7.5, 97.5, 187.5, 277.5)
  alltargets30bef <- c(75, 165, 255, 345) #30 degrees
  alltargets30aft <- c(15, 105, 195, 285)
  alltargets45bef <- c(67.5, 157.5, 247.5, 337.5) #45 degrees
  alltargets45aft <- c(22.5, 112.5, 202.5, 292.5)
  
  angles <- unique(RT$targetangle)
  RT['compensate'] <- NA
  
  #we want degrees for now, not percentages
  #we multily by -1 so that getting positive values mean that the hand went to the correct direction
  #percentage: above 100 values would mean an overcompensation, 0 is going directly to target, negative values are undercompensation
  for (target in angles){
    if (target %in% alltargets15bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

#grab reachdevs across all targets for EEG data
getMIRGroupAftereffects <- function(maxppid=31, location='feedback') { 
  
  participants <- seq(0,maxppid,1)
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantAftereffects(id=participant, location = location) #for every participant, get learning curve data
    
    reaches <- ppangles$reachdev #get reach deviations column from learning curve data
    trial <- c(1:length(reaches)) #sets up trial column
    dat <- cbind(trial, reaches)
    
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaches)
    }
  }
  #write.csv(dataoutput, file='data/MIR_learningcurve_degrees.csv', row.names = F) 
  return(dataoutput)
  
}

getMIRGroupRAEConfidenceInterval <- function(maxppid = 31, location = 'feedback', type = 'b'){
  
  data <- getMIRGroupAftereffects(maxppid = maxppid, location = location)
  
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
    
    write.csv(confidence, file='data/RAE_Mir_CI.csv', row.names = F) 
    
    
  }
  #}
}

#Plotting----

getBlockedIndividualRAE <- function(maxppid = 31, location = 'feedback', targetno = 6, perturb){
  
  if (perturb == 'ROT'){
    data <- getROTGroupAftereffects(maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getMIRGroupAftereffects(maxppid = maxppid, location = location)
  }
  
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
  
  return(ndat_long)
}

plotCollapsedBlockedIndRAE <- function(maxppid=31, location='feedback', targetno=6, perturbtypes=c('ROT','MIR'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig46_ROTMIRBlockedAftereffects.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Washout trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = 8, lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(perturb in perturbtypes){
    data <- getBlockedIndividualRAE(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    
    participants <- unique(data$participant)
    
    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(perturb == 'ROT'){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (perturb == 'MIR'){
        points(data$trial[row.idx]+(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      }
      
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      if(perturb == 'ROT'){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(perturb == 'MIR'){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(perturb == 'ROT'){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(perturb == 'MIR'){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  #add legend
  legend(5,-90,legend=c('Fixed rotation','Mirror reversal'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Statistics----

getRAEBlockedtTests <- function(maxppid = 31, location = 'feedback', targetno = 6){
  
  ROTdat <- getBlockedIndividualRAE(maxppid = maxppid, location = location, targetno = targetno, perturb = 'ROT')
  ROTdat$ptype <- 'ROT'
  MIRdat <- getBlockedIndividualRAE(maxppid = maxppid, location = location, targetno = targetno, perturb = 'MIR')
  MIRdat$ptype <- 'MIR'
  LC4test <- rbind(ROTdat, MIRdat)
  LC4test$ptype <- as.factor(LC4test$ptype)
  LC4test$trial <- as.factor(LC4test$trial)
  LC4test$participant <- as.factor(LC4test$participant)
  
  #compare first block
  ROTdat <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$trial == 1 & LC4test$ptype == 'MIR'),]
  
  cat('Rotation (first block) compared to Mirror (first block):\n')
  print(t.test(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat$reachdev, MIRdat$reachdev, method = 'paired'))
  cat('Bayesian t-test Rotation (first block) compared to Mirror (first block):\n')
  print(ttestBF(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  
  #compare second block
  ROTdat <- LC4test[which(LC4test$trial == 2 & LC4test$ptype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$trial == 2 & LC4test$ptype == 'MIR'),]
  
  cat('Rotation (second block) compared to Mirror (second block):\n')
  print(t.test(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat$reachdev, MIRdat$reachdev, method = 'paired'))
  cat('Bayesian t-test Rotation (second block) compared to Mirror (second block):\n')
  print(ttestBF(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  
  #compare last block
  ROTdat <- LC4test[which(LC4test$trial == 8 & LC4test$ptype == 'ROT'),]
  MIRdat <-LC4test[which(LC4test$trial == 8 & LC4test$ptype == 'MIR'),]
  
  cat('Rotation (last block) compared to Mirror (last block):\n')
  print(t.test(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat$reachdev, MIRdat$reachdev, method = 'paired'))
  cat('Bayesian t-test Rotation (last block) compared to Mirror (last block):\n')
  print(ttestBF(ROTdat$reachdev, MIRdat$reachdev, paired = TRUE))
  
  #compare rot first and last block
  ROTdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 'ROT'),]
  ROTdatlast <-LC4test[which(LC4test$trial == 8 & LC4test$ptype == 'ROT'),]
  
  cat('Rotation (first block) compared to Rotation (last block):\n')
  print(t.test(ROTdat1$reachdev, ROTdatlast$reachdev, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(ROTdat1$reachdev, ROTdatlast$reachdev, method = 'paired'))
  cat('Bayesian t-test Rotation (first block) compared to Rotation (last block):\n')
  print(ttestBF(ROTdat1$reachdev, ROTdatlast$reachdev, paired = TRUE))
  
  #compare mir first and last block
  MIRdat1 <- LC4test[which(LC4test$trial == 1 & LC4test$ptype == 'MIR'),]
  MIRdatlast <-LC4test[which(LC4test$trial == 8 & LC4test$ptype == 'MIR'),]
  
  cat('Mirror (first block) compared to Mirror (last block):\n')
  print(t.test(MIRdat1$reachdev, MIRdatlast$reachdev, paired = TRUE))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(MIRdat1$reachdev, MIRdatlast$reachdev, method = 'paired'))
  cat('Bayesian t-test Mirror (first block) compared to Mirror (last block):\n')
  print(ttestBF(MIRdat1$reachdev, MIRdatlast$reachdev, paired = TRUE))
  
}