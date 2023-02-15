source('ana/shared.R')
source('ana/learningRates.R')

#which side of the workspace did they move towards?

getAnglesWorkspace <- function(id, taskno, task, location){
  
  data <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  
  if(task == 'aligned' | task == 'random0' | task == 'random1'){
    AT<- getReachAngles(data, starttrial=0, endtrial=47, location = location)
  } else if (task == 'rotation' | task == 'mirror'){
    AT<- getReachAngles(data, starttrial=0, endtrial=89, location = location)
  }
  
  
  
  trials <- c(1:length(AT$trial))-1
  AT$direction <- NA
  
  for(trialno in trials){
    subdat <- AT[which(AT$trial == trialno),]
    target <- subdat$targetangle
    
    #ensure that reachdev is either in the left or right side of the workspace, relative to vertical midline
    #right targets
    if(target == 7.5){
      x <- subdat$reachdev
      if(x < -97.5 | x > 82.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 15){
      x <- subdat$reachdev
      if(x < -105 | x > 75){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 22.5){
      x <- subdat$reachdev
      if(x < -112.5 | x > 67.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 67.5){
      x <- subdat$reachdev
      if(x < -157.5 | x > 22.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 75){
      x <- subdat$reachdev
      if(x < -165 | x > 15){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 82.5){
      x <- subdat$reachdev
      if(x < -172.5 | x > 7.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 277.5){
      x <- subdat$reachdev
      if(x < -7.5 | x > 172.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 285){
      x <- subdat$reachdev
      if(x < -15 | x > 165){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 292.5){
      x <- subdat$reachdev
      if(x < -22.5 | x > 157.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 337.5){
      x <- subdat$reachdev
      if(x < -67.5 | x > 112.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 345){
      x <- subdat$reachdev
      if(x < -75 | x > 105){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 352.5){
      x <- subdat$reachdev
      if(x < -82.5 | x > 97.5){
        dir <- 'l'
      } else {
        dir <- 'r'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 97.5){#left targets
      x <- subdat$reachdev
      if(x < -7.5 | x > 172.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 105){
      x <- subdat$reachdev
      if(x < -15 | x > 165){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 112.5){
      x <- subdat$reachdev
      if(x < -22.5 | x > 157.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 157.5){
      x <- subdat$reachdev
      if(x < -67.5 | x > 112.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 165){
      x <- subdat$reachdev
      if(x < -75 | x > 105){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 172.5){
      x <- subdat$reachdev
      if(x < -82.5 | x > 97.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 187.5){
      x <- subdat$reachdev
      if(x < -97.5 | x > 82.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 195){
      x <- subdat$reachdev
      if(x < -105 | x > 75){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 202.5){
      x <- subdat$reachdev
      if(x < -112.5 | x > 67.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 247.5){
      x <- subdat$reachdev
      if(x < -157.5 | x > 22.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 255){
      x <- subdat$reachdev
      if(x < -165 | x > 15){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } else if(target == 262.5){
      x <- subdat$reachdev
      if(x < -172.5 | x > 7.5){
        dir <- 'r'
      } else {
        dir <- 'l'
      }
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    } 
  }
  
  return(AT)
}

getGroupMovementWorkspaceDirection <- function(maxppid = 31, groups = c('aln', 'rot', 'rdm', 'mir'), location = 'feedback') {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  for(group in groups){
    participants <- seq(0,maxppid,1)
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (group == 'aln'){
        ppangles <- getAnglesWorkspace(id=participant, taskno = 1, task = 'aligned', location = location)
      } else if (group == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesWorkspace(id = participant, taskno = 11, task = 'rotation', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesWorkspace(id = participant, taskno = 5, task = 'rotation', location = location)
        }
      } else if (group == 'rdm'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesWorkspace(id = participant, taskno = 9, task = 'random1', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesWorkspace(id = participant, taskno = 3, task = 'random0', location = location)
        }
      } else if (group == 'mir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesWorkspace(id = participant, taskno = 5, task = 'mirror', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesWorkspace(id = participant, taskno = 11, task = 'mirror', location = location)
        }
      }
      reaches <- ppangles$direction #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      #rdat <- dat$reaches
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/%s_MovementWorkspace_direction.csv', group), row.names = F) 
  }
  
}



# plot LRP differences for right and left moves ----
getWorkspaceLRPConfidenceInterval <- function(groups = c('aln_right', 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'), type = 'b', erps = 'lrp', channels = c('C3','C4')){
  for(channel in channels){
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s.csv', group, erps, channel))
      data <- data[,2:length(data)]
      
      data <- as.data.frame(data)
      timepts <- data$time
      data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
      
      confidence <- data.frame()
      
      
      for (time in timepts){
        cireaches <- data1[which(data$time == time), ]
        
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
        
        write.csv(confidence, file=sprintf('data/ERP_CI_%s_%s_%s.csv', group, erps, channel), row.names = F) 
        
      }
    }
  }
}


plotWorkspaceLRPs <- function(groups = c('aln_right', 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_Workspace_LRP.svg', width=16, height=24, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(4,2))
  
  for (group in groups){
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Move to %s side of workspace", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    text(-1, 6, 'target onset', cex = 0.85)
    text(0, 6, 'go signal', cex = 0.85)
    axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (channel in channels){
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s.csv', group, erps, channel))
      timepts <- data$time
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s_%s.csv', group, erps, channel))
      
      colourscheme <- getLRPColourScheme(channels = channel)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[channel]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
      
      # plot mean reaches for each group
      col <- colourscheme[[channel]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = mid, col = col, lty = 1, lwd = 2)
      
    }
    
    #add legend
    legend(0.2,-10,legend=c('C3','C4'),
           col=c(colourscheme[['C3']][['S']],colourscheme[['C4']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    #add movement onset
    if(group == 'aln_right' | group == 'aln_left'){
      mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
    } else if (group == 'rot_right' | group == 'rot_left'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'rdm_right' | group == 'rdm_left'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'mir_right' | group == 'mir_left'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    }
    
    colourscheme<-   list('S'='#A9A9A9ff', #dark grey
                                    'T'='#A9A9A92f')
    col <- colourscheme[['T']]
    lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['S']]
    points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
    
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Calculate LRPs-----
# Right(C3-C4) - Left(C3-C4)
getSubtractedLRP <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b', erps = 'lrp', directions = c('right', 'left')){
  
  for(group in groups){
    for(direction in directions){
      
      C3data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s_C3.csv', group, direction, erps))
      C4data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s_C4.csv', group, direction, erps))
      
      rowidx <- C3data$X
      timepts <- C3data$time
      
      diffdata <- C3data - C4data
      if(direction == 'right'){
        rightdiff <- diffdata[,2:(dim(diffdata)[2]-1)]
      } else if(direction == 'left'){
        leftdiff <- diffdata[,2:(dim(diffdata)[2]-1)]
      }
    }
    
    latdiff <- rightdiff - leftdiff
    groupLRP <- data.frame(rowidx, latdiff, timepts)
    
    write.csv(groupLRP, file=sprintf('data/Subtracted_LRP_DF_%s.csv', group), row.names = F) 
  }
}

getSubtractedLRPConfidenceInterval <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Subtracted_LRP_DF_%s.csv', group))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    timepts <- data$time
    data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
    
    confidence <- data.frame()
    
    
    for (time in timepts){
      cireaches <- data1[which(data$time == time), ]
      
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
      
      write.csv(confidence, file=sprintf('data/Subtracted_LRP_CI_%s.csv', group), row.names = F) 
      
    }
  }
  
}

plotSubtractedLRPs <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig8_Subtracted_LRP.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Lateralized Readiness Potentials across perturbation types", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  text(-1, 6, 'target onset', cex = 0.85)
  text(0, 6, 'go signal', cex = 0.85)
  axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Subtracted_LRP_DF_%s.csv', group))
    timepts <- data$time
    
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/Subtracted_LRP_CI_%s.csv', group))
    
    colourscheme <- getSubtractedLRPColourScheme(groups=group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
    
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    #lines(x = timepts, y = mid, col=col)
    lines(x = timepts, y = mid, col = col, lty = 1, lwd = 2)
    
    
    
    #add legend
    legend(0.2,-10,legend=c('aligned', 'rotation', 'random', 'mirror'),
           col=c(colourscheme[['aln']][['S']],colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
    
    
    ctr = 5
    for(group in groups){
      #add movement onset
      if(group == 'aln'){
        mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      } else if (group == 'rot'){
        mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      } else if (group == 'rdm'){
        mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      } else if (group == 'mir'){
        mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      }
      colourscheme <- getSubtractedLRPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(ctr, ctr), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = ctr, pch = 20, cex = 1.5, col=col)
      ctr = ctr - 0.5
    }
    
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


#Calculate LRPs SPLIT ACROSS LEARNING-----
# Right(C3-C4) - Left(C3-C4)
#DIFFERENT CURVE FOR EARLY AND LATE
getSplitLRP <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b', erps = 'lrp', directions = c('right', 'left'), training = c('early', 'late')){
  
  for(group in groups){
    for(t in training){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s_%s_C3.csv', group, direction, t, erps))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s_%s_C4.csv', group, direction, t, erps))
        
        rowidx <- C3data$X
        timepts <- C3data$time
        
        diffdata <- C3data - C4data
        if(direction == 'right'){
          rightdiff <- diffdata[,2:(dim(diffdata)[2]-1)]
        } else if(direction == 'left'){
          leftdiff <- diffdata[,2:(dim(diffdata)[2]-1)]
        }
      }
      
      latdiff <- rightdiff - leftdiff
      groupLRP <- data.frame(rowidx, latdiff, timepts)
      
      write.csv(groupLRP, file=sprintf('data/Subtracted_LRP_DF_%s_%s.csv', group, t), row.names = F) 
    }
  }
}

getSplitLRPConfidenceInterval <- function(groups = c('aln', 'rot', 'rdm', 'mir'), training = c('early', 'late'), type = 'b'){
  
  for (group in groups){
    for (t in training){
      data <- read.csv(file=sprintf('data/Subtracted_LRP_DF_%s_%s.csv', group, t))
      data <- data[,2:length(data)]
      
      data <- as.data.frame(data)
      timepts <- data$time
      data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
      
      confidence <- data.frame()
      
      
      for (time in timepts){
        cireaches <- data1[which(data$time == time), ]
        
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
        
        write.csv(confidence, file=sprintf('data/Subtracted_LRP_CI_%s_%s.csv', group, t), row.names = F) 
        
      }
    }
  }
}

plotSplitLRPs <- function(groups = c('aln', 'rot', 'rdm', 'mir'), training = c('early', 'late'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9_Split_LRP.svg', width=14, height=10, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,2))
  
  
  for (group in groups){
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("LRPs during early and late training: %s condition", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    text(-1, 6, 'target onset', cex = 0.85)
    text(0, 6, 'go signal', cex = 0.85)
    axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for(t in training){
      data <- read.csv(file=sprintf('data/Subtracted_LRP_DF_%s_%s.csv', group, t))
      timepts <- data$time
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/Subtracted_LRP_CI_%s_%s.csv', group, t))
      
      colourscheme <- getTrainingColourScheme(training=t)
      #take only first, last and middle columns of file
      lower <- groupconfidence[,1]
      upper <- groupconfidence[,3]
      mid <- groupconfidence[,2]
      
      col <- colourscheme[[t]][['T']] #use colour scheme according to group
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
      
      # plot mean reaches for each group
      col <- colourscheme[[t]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = mid, col = col, lty = 1, lwd = 2)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('early training', 'late training'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
      
      ctr = 5
      
      #add movement onset
      if(group == 'aln'){
        mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      } else if (group == 'rot'){
        mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      } else if (group == 'rdm'){
        mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      } else if (group == 'mir'){
        mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      }
      colourscheme <- getSubtractedLRPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(ctr, ctr), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = ctr, pch = 20, cex = 1.5, col=col)
      ctr = ctr - 0.5
      
    }
    
    
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}
















