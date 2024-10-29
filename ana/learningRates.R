source('ana/shared.R')

#Learning Rates----

getTrialReachAngleAtFeedback <- function(trialdf, location = 'feedback') {
  
  
  # location (string) determines where the angle of thereach is determines, it is one of:
  # maxvel: maximum velocity (default)
  # endpoint: end of the reach
  # feedback: the first sample in step 5 (when they see visual feedback of error)
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=1)
  
  # if the trial was rejected, return empty matrix now
  if (trialdf[1,'trialselected_bool'] == 0) {
    
    return(reachangle);
    
  }
  
  #grab specific step samples
  trialdf <- trialdf[trialdf$step == 5,]
  
  # extract the relevant reach information
  X <- trialdf[trialdf$sampleselected_bool == 1,'mousex_cm']
  Y <- trialdf[trialdf$sampleselected_bool == 1,'mousey_cm']
  MV <- trialdf[trialdf$sampleselected_bool == 1,'maxvelocity_idx']
  angle <- trialdf[1,'targetangle_deg']
  
  # print(X)
  
  # rotate the trajectory
  # (this avoids problems in the output of atan2 for large angles)
  trajectory <- rotateTrajectory(X,Y,-1*angle)
  X <- trajectory[,1]
  Y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be in the data
  if (location == 'maxvel') {
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  # end point, just the last point in the selected stretch of the reach
  if (location == 'endpoint') {
    rown <- length(X)
    invalidlocation <- FALSE
  }
  
  if (location == 'feedback') {
    rown <- 1
    invalidlocation <- FALSE
  }

  
  # if we don't have a valid location, we can't calculate an angle to return
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(Y[rown],X[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  #reachangle[1,2] <- angle #I don't know why I have to remove this for it to work!But it's the only thing keeping this function from being generic
  
  return(reachangle)
  
}

getReachAngles <- function(df, starttrial=0, endtrial=NULL, location = 'feedback') {
  
  trialnumbers <- c(starttrial:endtrial)
  
  #place holders for variables in data frame
  trial <- c()
  targetangle <- c()
  reachdev <- c()
  
  for (trialnumber in trialnumbers) {
    
    indices <- which(df$trial == trialnumber) #rows of current trial
    
    if (length(indices) > 0) { 
      #print(trialnumber)
      trialdf <- subset(df, trial == trialnumber) #get current trial number
      targetangle <- c(targetangle, trialdf$targetangle_deg[1]%%360) #target angle in degrees (all 12 for aligned); 1 is the index to get this value
      reachdev <- c(reachdev, getTrialReachAngleAtFeedback(trialdf, location = location)) #relies on reach deviation function
      trial <- c(trial, trialnumber) #counter to keep going
      
    } else {
      #set values to NA if not greater than zero
      #this part helps to fill in missing values in data
      targetangle <- c(targetangle, NA)
      reachdev <- c(reachdev, NA)
      trial <- c(trial, trialnumber) #trial numbers would still be displayed (i.e., is not NA)
      
    }
    
    
  }
  
  #build a data frame
  angularreachdeviations <- data.frame(trial, targetangle, reachdev)
  return(angularreachdeviations)
  
}

getAlignedTrainingBiases <- function(df, location) {
  
  #trials are 0 to 47
  df <- getReachAngles(df=df, starttrial = 0, endtrial = 47, location = location) 
  #get median reachdev for each angle
  trainingBiases <- aggregate(reachdev ~ targetangle, data= df, FUN = median) 
  return(trainingBiases)
  
}

plotLearningCurves <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_learningcurve.svg', width=7, height=10, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(2,1))
  
  plotROTLearningCurves()
  plotMIRLearningCurves()
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}

#identify movement onset times aligned to feedback----
getMovementOnset <- function(id, taskno, task){
  dat <- getParticipantTaskData(id = id, taskno = taskno, task = task)

  
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == c(4,5),]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      movementtime <- NA #will assign NA if step 4 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      movementtime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep4 <- subndat[1,] #need the first sample of step 4 - just after reach onset trigger is sent
      firststep5 <- subndat[subndat$step == 5,][1,]
      
      step4start <- firststep4$time_ms
      step5start <- firststep5$time_ms
      
      movementtime <- (step4start - step5start)/1000

      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, movementtime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'movement_onset', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getMovementOnsetGroup <- function(maxppid, groups = c('aln', 'rot', 'rdm', 'mir')) {
  #behavioral data aren't really split into small or large errors, just the task type
  #so we only plot movement onsets for aligned, rotation, random
  for (group in groups){
    #a consequence of adding the groups late led me to fix it in the manner below
    participants <- seq(0,maxppid,1)
    
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (group == 'aln'){
        mo <- getMovementOnset(id = participant, taskno = 1, task = 'aligned')
      } else if (group == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnset(id = participant, taskno = 11, task = 'rotation')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnset(id = participant, taskno = 5, task = 'rotation')
        }
      } else if (group == 'rdm'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnset(id = participant, taskno = 9, task = 'random1')
          mo1 <- getMovementOnset(id = participant, taskno = 3, task = 'random0')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnset(id = participant, taskno = 3, task = 'random0')
          mo1 <- getMovementOnset(id = participant, taskno = 9, task = 'random1')
        }
      } else if (group == 'mir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnset(id = participant, taskno = 5, task = 'mirror')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnset(id = participant, taskno = 11, task = 'mirror')
        }
      }
      
      
      reaches <- mo$movement_onset#get reach deviations column from learning curve data
      if (group =='rdm'){
        reaches1 <- mo1$movement_onset
        reaches <- c(reaches, reaches1)
      }
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
    write.csv(dataoutput, file=sprintf('data/Movement_Onset_%s.csv', group), row.names = F)
  }
   
}

getMovementOnsetConfidenceInterval <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Movement_Onset_%s.csv', group))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    
    data <- apply(data, 2, median) #take median movement onset for each participant

    #then take confidence interval for "group"
    confidence <- data.frame()
    cireaches <- data
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    citrial <- as.data.frame(t(citrial))
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    write.csv(confidence, file=sprintf('data/MovementOnset_CI_%s.csv', group), row.names = F) 
    
    
  }
}

# #Plotting initial error categories of EEG Data (ERN/FRN)----
# 
# getERPConfidenceInterval <- function(groups = c('aln', 'smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), type = 'b', erps = 'frn'){
#   for (group in groups){
#     data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
#     data <- data[,2:length(data)]
#     
#     data <- as.data.frame(data)
#     timepts <- data$time
#     data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
#     
#     confidence <- data.frame()
#     
#     
#     for (time in timepts){
#       cireaches <- data1[which(data$time == time), ]
#       
#       if (type == "t"){
#         cireaches <- cireaches[!is.na(cireaches)]
#         citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
#       } else if(type == "b"){
#         citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
#       }
#       
#       if (prod(dim(confidence)) == 0){
#         confidence <- citrial
#       } else {
#         confidence <- rbind(confidence, citrial)
#       }
#       
#       write.csv(confidence, file=sprintf('data/ERP_CI_%s_%s.csv', group, erps), row.names = F) 
#       
#     }
#   }
# }
# 
# #c('aln','smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir')
# plotERPs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
#   
#   for(ptype in perturbs){
#     #but we can save plot as svg file
#     if (target=='svg' & erps=='frn') {
#       svglite(file=sprintf('doc/fig/Fig1_FRN_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#     } else if (target=='svg' & erps=='ern') {
#       svglite(file=sprintf('doc/fig/Fig10_ERN_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#     }
#     
#     if(ptype == 'rot'){
#       groups = c('aln', 'smlrot', 'lrgrot')
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if (erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         #read in CI files created
#         groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#         
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if (erps == 'frn'){
#         mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
#         mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
#         
#         col <- colourscheme[['aligned']][['T']]
#         lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['aligned']][['S']]
#         points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#         col <- colourscheme[['lrg']][['T']]
#         lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['lrg']][['S']]
#         points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
#       }
#       
#       
#       #add legend
#       legend(0.8,-5,legend=c('Aligned','Small ROT', 'Large ROT'),
#              col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#     } else if (ptype == 'rdm'){
#       groups = c('aln', 'smlrdm', 'lrgrdm')
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if (erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         #read in CI files created
#         groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#         
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if (erps == 'frn'){
#         mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
#         mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
#         
#         col <- colourscheme[['aligned']][['T']]
#         lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['aligned']][['S']]
#         points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#         col <- colourscheme[['lrg']][['T']]
#         lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['lrg']][['S']]
#         points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
#       }
#       
#       
#       #add legend
#       legend(0.8,-5,legend=c('Aligned','Small RDM', 'Large RDM'),
#              col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#       
#     } else if (ptype == 'mir'){
#       groups = c('aln', 'smlmir', 'lrgmir')
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if (erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         #read in CI files created
#         groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#         
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if (erps == 'frn'){
#         mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
#         mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
#         
#         col <- colourscheme[['aligned']][['T']]
#         lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['aligned']][['S']]
#         points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#         col <- colourscheme[['lrg']][['T']]
#         lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['lrg']][['S']]
#         points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
#       }
#       
#       
#       #add legend
#       legend(0.8,-5,legend=c('Aligned','Small MIR', 'Large MIR'),
#              col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#       
#     }
#     #close everything if you saved plot as svg
#     if (target=='svg') {
#       dev.off()
#     }
#   }
# }
# 
# getDiffWavesConfidenceInterval <- function(groups = c('smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), type = 'b', erps = 'frn'){
#   for (group in groups){
#     data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
#     data <- data[,2:length(data)]
#     
#     data <- as.data.frame(data)
#     timepts <- data$time
#     data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
#     
#     confidence <- data.frame()
#     
#     
#     for (time in timepts){
#       cireaches <- data1[which(data$time == time), ]
#       
#       if (type == "t"){
#         cireaches <- cireaches[!is.na(cireaches)]
#         citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
#       } else if(type == "b"){
#         citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
#       }
#       
#       if (prod(dim(confidence)) == 0){
#         confidence <- citrial
#       } else {
#         confidence <- rbind(confidence, citrial)
#       }
#       
#       write.csv(confidence, file=sprintf('data/DiffWaves_CI_%s_%s.csv', group, erps), row.names = F) 
#       
#     }
#   }
# }
# 
# #groups = c('smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir')
# plotDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
#   
#   for(ptype in perturbs){
#     #but we can save plot as svg file
#     if (target=='svg' & erps == 'frn') {
#       svglite(file=sprintf('doc/fig/Fig2_FRN_DiffWaves_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#     } else if (target=='svg' & erps == 'ern') {
#       svglite(file=sprintf('doc/fig/Fig11_ERN_DiffWaves_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#     }
#     
#     if(ptype == 'rot'){
#       groups = c('smlrot', 'lrgrot')
#       
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if(erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         groupconfidence <- read.csv(file=sprintf('data/DiffWaves_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if(erps=='frn'){
#         mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
#         
#         col <- colourscheme[['late']][['T']]
#         lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['late']][['S']]
#         points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#       }
#       
#       #add legend
#       legend(0.8,-5,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
#              col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#       
#     } else if (ptype == 'rdm'){
#       groups = c('smlrdm', 'lrgrdm')
#       
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if(erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         groupconfidence <- read.csv(file=sprintf('data/DiffWaves_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if(erps=='frn'){
#         mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
#         
#         col <- colourscheme[['late']][['T']]
#         lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['late']][['S']]
#         points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#       }
#       
#       #add legend
#       legend(0.8,-5,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
#              col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#     } else if (ptype == 'mir'){
#       groups = c('smlmir', 'lrgmir')
#       
#       # create plot
#       meanGroupReaches <- list() #empty list so that it plots the means last
#       #NA to create empty plot
#       # could maybe use plot.new() ?
#       if(erps == 'frn'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       } else if (erps == 'ern'){
#         plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#              xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#              main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#       }
#       
#       abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#       axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#       axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#       
#       for (group in groups){
#         data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
#         timepts <- data$time
#         timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#         
#         groupconfidence <- read.csv(file=sprintf('data/DiffWaves_CI_%s_%s.csv', group, erps))
#         groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#         
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         
#         colourscheme <- getErrSizeColourScheme(err = err)
#         #take only first, last and middle columns of file
#         lower <- groupconfidence[,1]
#         upper <- groupconfidence[,3]
#         mid <- groupconfidence[,2]
#         
#         col <- colourscheme[[err]][['T']] #use colour scheme according to group
#         
#         #upper and lower bounds create a polygon
#         #polygon creates it from low left to low right, then up right to up left -> use rev
#         #x is just trial nnumber, y depends on values of bounds
#         polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#         
#         meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#       }
#       
#       for (group in groups) {
#         if(group == 'smlrot'|group == 'smlrdm'|group == 'smlmir'){
#           err <- 'sml'
#         } else if (group == 'lrgrot'|group == 'lrgrdm'|group == 'lrgmir'){
#           err <- 'lrg'
#         } else if (group == 'aln'){
#           err <- 'aligned'
#         }
#         # plot mean reaches for each group
#         col <- colourscheme[[err]][['S']]
#         #lines(x = timepts, y = mid, col=col)
#         lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#       }
#       
#       #add movement onset 
#       if(erps=='frn'){
#         mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
#         
#         col <- colourscheme[['late']][['T']]
#         lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#         col <- colourscheme[['late']][['S']]
#         points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
#         
#       }
#       
#       #add legend
#       legend(0.8,-5,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
#              col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
#              lty=1,bty='n',cex=1,lwd=2)
#     }
#     #close everything if you saved plot as svg
#     if (target=='svg') {
#       dev.off()
#     }
#   }
# }
#Plotting Early/ Late EEG Data (ERN/FRN)----

getEarlyLateERPCI <- function(groups = c('aln', 'earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotEarlyLateERPs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='frn') {
      svglite(file=sprintf('doc/fig/Fig1A_FRN_EarlyLate_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & erps=='ern') {
      svglite(file=sprintf('doc/fig/Fig10A_ERN_EarlyLate_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'earlyrot', 'laterot')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grab only -.250 to 1 sec
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
        
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early ROT', 'Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      groups = c('aln', 'earlyrdm', 'laterdm')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grab -0.25 to 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
        
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early RDM', 'Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'earlymir', 'latemir')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grab -0.25 to 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
        
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early MIR', 'Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getDiffWavesEarlyLateCI <- function(groups = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotEarlyLateDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'frn') {
      svglite(file=sprintf('doc/fig/Fig2A_FRN_DiffWaves_EarlyLate_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & erps == 'ern') {
      svglite(file=sprintf('doc/fig/Fig11A_ERN_DiffWaves_EarlyLate_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('earlyrot', 'laterot')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grab -0.25 to 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if(erps=='frn'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,-5,legend=c('Early ROT - Aligned', 'Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'rdm'){
      groups = c('earlyrdm', 'laterdm')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grab -0.25 to 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if(erps=='frn'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,-5,legend=c('Early RDM - Aligned', 'Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      groups = c('earlymir', 'latemir')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #grabe -0.25 to 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        colourscheme <- getTrainingColourScheme(err = err)
        #take only first, last and middle columns of file
        lower <- groupconfidence[,1]
        upper <- groupconfidence[,3]
        mid <- groupconfidence[,2]
        
        col <- colourscheme[[err]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
        
        meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
      }
      
      for (group in groups) {
        if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
          err <- 'early'
        } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
          err <- 'late'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      if(erps=='frn'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,-5,legend=c('Early MIR - Aligned', 'Late MIR - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getPTypeDiffWavesEarlyLateCI <- function(groups = c('rot_diff', 'rdm_diff', 'mir_diff'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_EvL_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_EarlyLate_EvL_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotPTypeEarlyLateDiffWaves <- function(groups = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  #but we can save plot as svg file
  if (target=='svg' & erps == 'frn') {
    svglite(file='doc/fig/Fig2D_FRN_DiffWaves_EarlyLate_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & erps == 'ern') {
    svglite(file='doc/fig/Fig11D_ERN_DiffWaves_EarlyLate_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if (erps == 'frn'){
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = "Difference waves time-locked to feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  } else if (erps == 'ern'){
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = "Difference waves time-locked to movement onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_EvL_%s_diff_%s.csv', group, erps))
    timepts <- data$time
    timepts <- timepts[351:601] #grab -0.25 to 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_EvL_CI_%s_diff_%s.csv', group, erps))
    groupconfidence <- groupconfidence[351:601,] #grab timepts we need
    
    colourscheme <- getPTypeDiffWavesColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    #lines(x = timepts, y = mid, col=col)
    lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
  }
  
  #add movement onset 
  if(erps=='frn'){
    mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
    mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
    mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
    
    col <- colourscheme[['rot']][['T']]
    lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5.5, 5.5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rot']][['S']]
    points(x = mo_rot[,2], y = 5.5, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['rdm']][['T']]
    lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rdm']][['S']]
    points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['mir']][['T']]
    lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['mir']][['S']]
    points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    
  }
  
  #add legend
  legend(0.8,-5,legend=c('Rot', 'Rdm', 'Mir'),
         col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#identify movement onset times aligned to go target onset----
getMovementOnsetLRP <- function(id, taskno, task){
  dat <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  
  
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == c(3,4),]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      movementtime <- NA #will assign NA if step 3 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      movementtime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep3 <- subndat[1,] #need the first sample of step 3 - just after go target onset trigger is sent
      firststep4 <- subndat[subndat$step == 4,][1,]
      
      step3start <- firststep3$time_ms
      step4start <- firststep4$time_ms
      
      movementtime <- (step4start - step3start)/1000
      
      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, movementtime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'movement_onset', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getMovementOnsetGroupLRP <- function(maxppid, groups = c('aln', 'rot', 'rdm', 'mir')) {
  #behavioral data aren't really split into small or large errors, just the task type
  #so we only plot movement onsets for aligned, rotation, random
  for (group in groups){
    #a consequence of adding the groups late led me to fix it in the manner below
    participants <- seq(0,maxppid,1)
    
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (group == 'aln'){
        mo <- getMovementOnsetLRP(id = participant, taskno = 1, task = 'aligned')
      } else if (group == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnsetLRP(id = participant, taskno = 11, task = 'rotation')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnsetLRP(id = participant, taskno = 5, task = 'rotation')
        }
      } else if (group == 'rdm'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnsetLRP(id = participant, taskno = 9, task = 'random1')
          mo1 <- getMovementOnsetLRP(id = participant, taskno = 3, task = 'random0')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnsetLRP(id = participant, taskno = 3, task = 'random0')
          mo1 <- getMovementOnsetLRP(id = participant, taskno = 9, task = 'random1')
        }
      } else if (group == 'mir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getMovementOnsetLRP(id = participant, taskno = 5, task = 'mirror')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getMovementOnsetLRP(id = participant, taskno = 11, task = 'mirror')
        }
      }
      
      
      reaches <- mo$movement_onset#get reach deviations column from learning curve data
      if (group =='rdm'){
        reaches1 <- mo1$movement_onset
        reaches <- c(reaches, reaches1)
      }
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
    write.csv(dataoutput, file=sprintf('data/Movement_Onset_%s_lrp.csv', group), row.names = F)
  }
  
}

getMovementOnsetConfidenceIntervalLRP <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Movement_Onset_%s_lrp.csv', group))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    
    data <- apply(data, 2, median) #take median movement onset for each participant
    
    #then take confidence interval for "group"
    confidence <- data.frame()
    cireaches <- data
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    citrial <- as.data.frame(t(citrial))
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    write.csv(confidence, file=sprintf('data/MovementOnset_CI_%s_lrp.csv', group), row.names = F) 
    
    
  }
}

# #Plotting initial error categories for LRPs----
# getLRPConfidenceInterval <- function(groups = c('aln', 'smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), type = 'b', erps = 'lrp', channels = c('C3','C4')){
#   for(channel in channels){
#     for (group in groups){
#       data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s.csv', group, erps, channel))
#       data <- data[,2:length(data)]
#       
#       data <- as.data.frame(data)
#       timepts <- data$time
#       data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
#       
#       confidence <- data.frame()
#       
#       
#       for (time in timepts){
#         cireaches <- data1[which(data$time == time), ]
#         
#         if (type == "t"){
#           cireaches <- cireaches[!is.na(cireaches)]
#           citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
#         } else if(type == "b"){
#           citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
#         }
#         
#         if (prod(dim(confidence)) == 0){
#           confidence <- citrial
#         } else {
#           confidence <- rbind(confidence, citrial)
#         }
#         
#         write.csv(confidence, file=sprintf('data/ERP_CI_%s_%s_%s.csv', group, erps, channel), row.names = F) 
#         
#       }
#     }
#   }
# }
# 
# 
# plotLRPs <- function(groups = c('aln','smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
#   
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig4_LRP.svg', width=16, height=24, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   par(mfrow = c(4,2))
#   
#   for (group in groups){
#     #NA to create empty plot
#     # could maybe use plot.new() ?
#     plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#          xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#          main = sprintf("ERP time-locked to go signal onset, %s", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#     abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#     text(-1, 6, 'target onset', cex = 0.85)
#     text(0, 6, 'go signal', cex = 0.85)
#     axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#     axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#     
#     for (channel in channels){
#       data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s.csv', group, erps, channel))
#       timepts <- data$time
#       timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#       
#       #read in files created by getGroupConfidenceInterval in filehandling.R
#       groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s_%s.csv', group, erps, channel))
#       groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#       
#       colourscheme <- getLRPColourScheme(channels = channel)
#       #take only first, last and middle columns of file
#       lower <- groupconfidence[,1]
#       upper <- groupconfidence[,3]
#       mid <- groupconfidence[,2]
#       
#       col <- colourscheme[[channel]][['T']] #use colour scheme according to group
#       
#       #upper and lower bounds create a polygon
#       #polygon creates it from low left to low right, then up right to up left -> use rev
#       #x is just trial nnumber, y depends on values of bounds
#       polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#       
#       # plot mean reaches for each group
#       col <- colourscheme[[channel]][['S']]
#       #lines(x = timepts, y = mid, col=col)
#       lines(x = timepts, y = mid, col = col, lty = 1, lwd = 2)
# 
#     }
#     
#     #add legend
#     legend(0.2,-10,legend=c('C3','C4'),
#            col=c(colourscheme[['C3']][['S']],colourscheme[['C4']][['S']]),
#            lty=1,bty='n',cex=1,lwd=2)
#     
#     #add movement onset
#     if(group == 'aln'){
#       mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
#     } else if (group == 'smlrot'){
#       mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
#     } else if (group == 'lrgrot'){
#       mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
#     } else if (group == 'smlrdm'){
#       mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
#     } else if (group == 'lrgrdm'){
#       mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
#     } else if (group == 'smlmir'){
#       mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
#     } else if (group == 'lrgmir'){
#       mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
#     }
#     colourscheme <- getERPColourScheme(groups = group)
#     col <- colourscheme[[group]][['T']]
#     lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#     col <- colourscheme[[group]][['S']]
#     points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
#     
# 
#   }
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }
# 
# #C3 only (finding out which is more negative)
# getLRPDiffWavesConfidenceInterval <- function(groups = c('smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), type = 'b', erps = 'lrp'){
#   for (group in groups){
#     data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s_C3.csv', group, erps))
#     data <- data[,2:length(data)]
#     
#     data <- as.data.frame(data)
#     timepts <- data$time
#     data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
#     
#     confidence <- data.frame()
#     
#     
#     for (time in timepts){
#       cireaches <- data1[which(data$time == time), ]
#       
#       if (type == "t"){
#         cireaches <- cireaches[!is.na(cireaches)]
#         citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
#       } else if(type == "b"){
#         citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
#       }
#       
#       if (prod(dim(confidence)) == 0){
#         confidence <- citrial
#       } else {
#         confidence <- rbind(confidence, citrial)
#       }
#       
#       write.csv(confidence, file=sprintf('data/DiffWaves_CI_%s_%s_C3.csv', group, erps), row.names = F) 
#       
#     }
#   }
# }
# 
# plotLRPDiffWaves <- function(groups = c('smlrot', 'lrgrot', 'smlrdm', 'lrgrdm', 'smlmir', 'lrgmir'), target='inline', erps = 'lrp') {
#   
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig5_LRP_DiffWaves.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   # create plot
#   meanGroupReaches <- list() #empty list so that it plots the means last
#   
#   for (group in groups){
#     data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s_C3.csv', group, erps))
#     timepts <- data$time
#     timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
#   }
#   
#   #NA to create empty plot
#   # could maybe use plot.new() ?
#   plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
#        xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
#        main = "C3 Difference Waves time-locked to go signal onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   text(-1, 6, 'target onset', cex = 0.85)
#   text(0, 6, 'go signal', cex = 0.85)
#   axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
#   axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
#   
#   for(group in groups){
#     #read in files created by getGroupConfidenceInterval in filehandling.R
#     groupconfidence <- read.csv(file=sprintf('data/DiffWaves_CI_%s_%s_C3.csv', group, erps))
#     groupconfidence <- groupconfidence[101:701,] #grab timepts we need
#     
#     colourscheme <- getERPColourScheme(groups = group)
#     #take only first, last and middle columns of file
#     lower <- groupconfidence[,1]
#     upper <- groupconfidence[,3]
#     mid <- groupconfidence[,2]
#     
#     col <- colourscheme[[group]][['T']] #use colour scheme according to group
#     
#     #upper and lower bounds create a polygon
#     #polygon creates it from low left to low right, then up right to up left -> use rev
#     #x is just trial nnumber, y depends on values of bounds
#     polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
#     
#     meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
#   }
#   
#   
#   for (group in groups) {
#     # plot mean reaches for each group
#     col <- colourscheme[[group]][['S']]
#     #lines(x = timepts, y = mid, col=col)
#     lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
#     
#     #add movement onset
#     if(group == 'aln'){
#       mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
#       colourscheme <- getERPColourScheme(groups = group)
#       col <- colourscheme[[group]][['T']]
#       lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
#       col <- colourscheme[[group]][['S']]
#       points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
#     } else if (group == 'lrgrot'){
#       mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
#       colourscheme <- getERPColourScheme(groups = group)
#       col <- colourscheme[[group]][['T']]
#       lines(x = c(mo[,1], mo[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
#       col <- colourscheme[[group]][['S']]
#       points(x = mo[,2], y = 4, pch = 20, cex = 1.5, col=col)
#     } else if (group == 'lrgrdm'){
#       mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
#       colourscheme <- getERPColourScheme(groups = group)
#       col <- colourscheme[[group]][['T']]
#       lines(x = c(mo[,1], mo[,3]), y = c(3.5, 3.5), col = col, lty = 1, lwd = 8)
#       col <- colourscheme[[group]][['S']]
#       points(x = mo[,2], y = 3.5, pch = 20, cex = 1.5, col=col)
#     } else if (group == 'lrgmir'){
#       mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
#       colourscheme <- getERPColourScheme(groups = group)
#       col <- colourscheme[[group]][['T']]
#       lines(x = c(mo[,1], mo[,3]), y = c(3, 3), col = col, lty = 1, lwd = 8)
#       col <- colourscheme[[group]][['S']]
#       points(x = mo[,2], y = 3, pch = 20, cex = 1.5, col=col)
#     }
#   }
#   
#   
#   #add legend
#   legend(0.8,-5,legend=c('Small ROT - Aligned', 'Large ROT - Aligned', 'Small RDM - Aligned', 'Large RDM - Aligned', 'Small MIR - Aligned', 'Large MIR - Aligned'),
#          col=c(colourscheme[['smlrot']][['S']],colourscheme[['lrgrot']][['S']],colourscheme[['smlrdm']][['S']],colourscheme[['lrgrdm']][['S']],colourscheme[['smlmir']][['S']],colourscheme[['lrgmir']][['S']]),
#          lty=1,bty='n',cex=1,lwd=2)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }

#Plotting Early/Late LRPs----
getLRPEarlyLateCI <- function(groups = c('aln', 'earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'lrp', channels = c('C3','C4')){
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
        
        write.csv(confidence, file=sprintf('data/ERP_EarlyLate_CI_%s_%s_%s.csv', group, erps, channel), row.names = F) 
        
      }
    }
  }
}

plotEarlyLateLRPs <- function(groups = c('aln','earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4A_LRP_EarlyLate.svg', width=16, height=24, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  par(mfrow = c(4,2))
  
  for (group in groups){
    #NA to create empty plot
    # could maybe use plot.new() ?
    plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("ERP time-locked to go signal onset, %s", group), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    text(-1, 6, 'target onset', cex = 0.85)
    text(0, 6, 'go signal', cex = 0.85)
    axis(1, at = c( -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (channel in channels){
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_%s.csv', group, erps, channel))
      timepts <- data$time
      timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s_%s.csv', group, erps, channel))
      groupconfidence <- groupconfidence[201:701,] #grab timepts we need
      
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
    if(group == 'aln'){
      mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
    } else if (group == 'earlyrot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'laterot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'earlyrdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'laterdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'earlymir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    } else if (group == 'latemir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    }
    colourscheme <- getEarlyLateERPColourScheme(groups = group)
    col <- colourscheme[[group]][['T']]
    lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[[group]][['S']]
    points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
    
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#C3 only (finding out which is more negative)
getLRPDiffWavesEarlyLateCI <- function(groups = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'lrp'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s_C3.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s_C3.csv', group, erps), row.names = F) 
      
    }
  }
}

plotLRPDiffWavesEarlyLate <- function(groups = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'lrp') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5A_LRP_EarlyLate_DiffWaves.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s_C3.csv', group, erps))
    timepts <- data$time
    timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
  }
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "C3 Difference Waves time-locked to go signal onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  text(-1, 6, 'target onset', cex = 0.85)
  text(0, 6, 'go signal', cex = 0.85)
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s_C3.csv', group, erps))
    groupconfidence <- groupconfidence[201:701,] #grab timepts we need
    
    colourscheme <- getEarlyLateERPColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(timepts, rev(timepts)), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    #lines(x = timepts, y = mid, col=col)
    lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    
    #add movement onset
    if(group == 'aln'){
      mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      colourscheme <- getEarlyLateERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
    } else if (group == 'laterot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      colourscheme <- getEarlyLateERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 4, pch = 20, cex = 1.5, col=col)
    } else if (group == 'laterdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      colourscheme <- getEarlyLateERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(3.5, 3.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 3.5, pch = 20, cex = 1.5, col=col)
    } else if (group == 'latemir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      colourscheme <- getEarlyLateERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(3, 3), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 3, pch = 20, cex = 1.5, col=col)
    }
  }
  
  
  #add legend
  legend(0.8,-5,legend=c('Early ROT - Aligned', 'Late ROT - Aligned', 'Early RDM - Aligned', 'Late RDM - Aligned', 'Early MIR - Aligned', 'Late MIR - Aligned'),
         col=c(colourscheme[['earlyrot']][['S']],colourscheme[['laterot']][['S']],colourscheme[['earlyrdm']][['S']],colourscheme[['laterdm']][['S']],colourscheme[['earlymir']][['S']],colourscheme[['latemir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Plotting ALL DATA LRPs----
getAllLRPConfidenceInterval <- function(type = 'b', erps = 'lrp', channels = c('C3','C4')){
  for(channel in channels){
    
    data <- read.csv(file=sprintf('data/Evoked_DF_alldata_%s_%s.csv', erps, channel))
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
      
      write.csv(confidence, file=sprintf('data/ERP_CI_alldata_%s_%s.csv', erps, channel), row.names = F) 
      
    }
    
  }
}


plotAllLRPs <- function(groups = c('aln', 'lrgrot', 'lrgrdm', 'lrgmir'),target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4B_AlldataLRP.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "ERP time-locked to go signal onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  text(-1, 6, 'target onset', cex = 0.85)
  text(0, 6, 'go signal', cex = 0.85)
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (channel in channels){
    data <- read.csv(file=sprintf('data/Evoked_DF_alldata_%s_%s.csv', erps, channel))
    timepts <- data$time
    timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
    
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ERP_CI_alldata_%s_%s.csv', erps, channel))
    groupconfidence <- groupconfidence[201:701,] #grab timepts we need
    
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
    lines(x = timepts, y = mid, col = col, lty = 1, lwd = 1)
    
  }
  
  #add legend
  legend(0.2,-10,legend=c('C3','C4'),
         col=c(colourscheme[['C3']][['S']],colourscheme[['C4']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  ctr = 5
  for(group in groups){
    #add movement onset
    if(group == 'aln'){
      mo <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
    } else if (group == 'lrgrot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'lrgrdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'lrgmir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    }
    colourscheme <- getERPColourScheme(groups = group)
    col <- colourscheme[[group]][['T']]
    lines(x = c(mo[,1], mo[,3]), y = c(ctr, ctr), col = col, lty = 1, lwd = 8)
    col <- colourscheme[[group]][['S']]
    points(x = mo[,2], y = ctr, pch = 20, cex = 1.5, col=col)
    ctr = ctr - 0.5
  }

  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#identify stimulus onset times aligned to go target onset----
getStimulusOnsetLRP <- function(id, taskno, task){
  dat <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  
  
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == c(2,3),]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      movementtime <- NA #will assign NA if step 3 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      movementtime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep2 <- subndat[1,] #need the first sample of step 3 - just after go target onset trigger is sent
      firststep3 <- subndat[subndat$step == 3,][1,]
      
      step2start <- firststep2$time_ms
      step3start <- firststep3$time_ms
      
      stimtime <- (step2start - step3start)/1000
      
      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, stimtime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'stimulus_onset', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getStimulusOnsetGroupLRP <- function(maxppid, groups = c('aln', 'rot', 'rdm', 'mir')) {
  #behavioral data aren't really split into small or large errors, just the task type
  #so we only plot movement onsets for aligned, rotation, random
  for (group in groups){
    #a consequence of adding the groups late led me to fix it in the manner below
    participants <- seq(0,maxppid,1)
    
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (group == 'aln'){
        mo <- getStimulusOnsetLRP(id = participant, taskno = 1, task = 'aligned')
      } else if (group == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getStimulusOnsetLRP(id = participant, taskno = 11, task = 'rotation')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getStimulusOnsetLRP(id = participant, taskno = 5, task = 'rotation')
        }
      } else if (group == 'rdm'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getStimulusOnsetLRP(id = participant, taskno = 9, task = 'random1')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getStimulusOnsetLRP(id = participant, taskno = 3, task = 'random0')
        }
      } else if (group == 'mir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getStimulusOnsetLRP(id = participant, taskno = 5, task = 'mirror')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getStimulusOnsetLRP(id = participant, taskno = 11, task = 'mirror')
        }
      }
      
      
      reaches <- mo$stimulus_onset#get reach deviations column from learning curve data
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
    write.csv(dataoutput, file=sprintf('data/Stimulus_Onset_%s_lrp.csv', group), row.names = F)
  }
  
}

getStimulusOnsetConfidenceInterval <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Stimulus_Onset_%s_lrp.csv', group))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    
    data <- apply(data, 2, median) #take median movement onset for each participant
    
    #then take confidence interval for "group"
    confidence <- data.frame()
    cireaches <- data
    
    if (type == "t"){
      cireaches <- cireaches[!is.na(cireaches)]
      citrial <- t.interval(data = cireaches, variance = var(cireaches), conf.level = 0.95)
    } else if(type == "b"){
      citrial <- getBSConfidenceInterval(data = cireaches, resamples = 1000)
    }
    
    citrial <- as.data.frame(t(citrial))
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
    
    write.csv(confidence, file=sprintf('data/StimulusOnset_CI_%s_lrp.csv', group), row.names = F) 
    
    
  }
}

#Aligned----
getALNParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  AT<- getReachAngles(alignedTraining, starttrial=0, endtrial=47, location = location) 
  
  return(AT)
}

getALNGroupLearningCurves <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getALNParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/ALIGNED_learningcurve_degrees.csv', row.names = F) 
}

getALNGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getALNGroupLearningCurves(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/ALN_CI_learningcurve.csv', row.names = F) 
    
    
  }
  #}
}

#can change function below to include a cutoff and check data
#not necessary for just running functions, since we index in python scripts
getsmallALNErrors <- function(maxppid = 31, location = 'feedback'){#, cutoff = 5){
 
  data <- getALNGroupLearningCurves(maxppid = maxppid, location = location)
  trial <- data[,1]
  data <- data[,2:ncol(data)]
  #data[data >= cutoff] <- NA
  #data[data <= cutoff*-1] <- NA
  ndat <- data.frame(trial, data)
  
  write.csv(ndat, file='data/ALIGNED_learningcurve_degrees.csv', row.names = F) 
  
  
}

# Learning curves RANDOM ROTATION----
getRDMParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 9, task = 'random1')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 3, task = 'random0')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])*-1)
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)]))
    }
  }
  
  
  
  
  return(RT)
}

getRDMGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getRDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMROT_learningcurve_degrees.csv', row.names = F) 
}

getRDMGroupLearningCurves <- function(maxppid, location, angles = c(15, 25, 35)) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  for (angle in angles){
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      ppangles <- getRDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
      ppangles$rotation <- abs(ppangles$rotation)
      for (irow in c(1:nrow(ppangles))){
        subdat <- ppangles[irow,]
        if (subdat$rotation != angle){
          subdat$reachdev <- NA
          ppangles[irow,] <- subdat
        }
      }
      
      reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
      
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle), row.names = F) 
  }
}

getRDMGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRDMGroupLCALL(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/RDMROT_CI_learningcurve.csv', row.names = F) 
    
    
  }
  #}
}

plotRDMLearningCurves <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/FigX_ROT_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,48), ylim = c(-10,36), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: ROT", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 48)) #tick marks for x axis
  axis(2, at = c(-10, 0, 10, 20, 30, 35)) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  groupconfidence <- read.csv(file='data/RDMROT_CI_learningcurve.csv')
  
  colourscheme <- list('S'='#e51636ff', #vivid/york red
                       'T'='#e516362f')
  
  #colourscheme <- getColourScheme(groups = group)
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  col <- colourscheme[['T']] #use colour scheme according to group
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
  
  meanGroupReaches <- mid #use mean to fill in empty list for each group
  
  
  
  
  # plot mean reaches for each group
  col <- colourscheme[['S']]
  lines(meanGroupReaches,col=col,lty=1)
  
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning curves RANDOM MIRROR----
getRDMMIRParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 3, task = 'random0')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 9, task = 'random1')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])*-1)
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)]))
    }
  }
  
  
  
  
  return(RT)
}

getRDMMIRGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getRDMMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMMIR_learningcurve_degrees.csv', row.names = F) 
}

getRDMMIRGroupLearningCurves <- function(maxppid, location, angles = c(15, 25, 35)) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  for (angle in angles){
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      ppangles <- getRDMMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
      ppangles$rotation <- abs(ppangles$rotation)
      for (irow in c(1:nrow(ppangles))){
        subdat <- ppangles[irow,]
        if (subdat$rotation != angle){
          subdat$reachdev <- NA
          ppangles[irow,] <- subdat
        }
      }
      
      reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
      
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle), row.names = F) 
  }
}

getRDMMIRGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRDMMIRGroupLCALL(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/RDMMIR_CI_learningcurve.csv', row.names = F) 
    
    
  }
  #}
}

plotRDMMIRLearningCurves <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/FigX_ROTMIR_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,48), ylim = c(-10,36), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: RDM", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 48)) #tick marks for x axis
  axis(2, at = c(-10, 0, 10, 20, 30, 35)) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  groupconfidence <- read.csv(file='data/RDMMIR_CI_learningcurve.csv')
  
  colourscheme <- list('S'='#e51636ff', #vivid/york red
                       'T'='#e516362f')
  
  #colourscheme <- getColourScheme(groups = group)
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  col <- colourscheme[['T']] #use colour scheme according to group
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
  
  meanGroupReaches <- mid #use mean to fill in empty list for each group
  
  
  
  
  # plot mean reaches for each group
  col <- colourscheme[['S']]
  lines(meanGroupReaches,col=col,lty=1)
  
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning Curves ROTATION----
getROTParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'rotation')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'rotation')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
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
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])*-1)#/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)])*-1)#/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  
  return(RT)
}

getROTGroupLearningCurves <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getROTParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/ROT_learningcurve_degrees.csv', row.names = F) 
}

getROTGroupConfidenceInterval <- function(maxppid, location, type){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupLearningCurves(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/ROT_CI_learningcurve.csv', row.names = F) 

    
  }
  #}
}

# plotROTLearningCurves <- function(target='inline') {
#   
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/FigX_ROT_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   # create plot
#   meanGroupReaches <- list() #empty list so that it plots the means last
#   
#   #NA to create empty plot
#   # could maybe use plot.new() ?
#   plot(NA, NA, xlim = c(0,91), ylim = c(-200,200), 
#        xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
#        main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   abline(h = c(0, 15, 30, 45), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
#   axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
#   
#   
#   #read in files created by getGroupConfidenceInterval in filehandling.R
#   groupconfidence <- read.csv(file='data/ROT_CI_learningcurve.csv')
#   
#   colourscheme <- list('S'='#e51636ff', #vivid/york red
#                        'T'='#e516362f')
#   
#   #colourscheme <- getColourScheme(groups = group)
#   #take only first, last and middle columns of file
#   lower <- groupconfidence[,1]
#   upper <- groupconfidence[,3]
#   mid <- groupconfidence[,2]
#   
#   col <- colourscheme[['T']] #use colour scheme according to group
#   
#   #upper and lower bounds create a polygon
#   #polygon creates it from low left to low right, then up right to up left -> use rev
#   #x is just trial nnumber, y depends on values of bounds
#   polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
#   
#   meanGroupReaches <- mid #use mean to fill in empty list for each group
#   
#   
#   
#   
#   # plot mean reaches for each group
#   col <- colourscheme[['S']]
#   lines(meanGroupReaches,col=col,lty=1)
#   
#   
#   #add legend
#   # legend(70,-100,legend=c('Non-Instructed','Instructed'),
#   #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
#   #        lty=1,bty='n',cex=1,lwd=2)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }

# Learning Curves MIRROR----
getMIRParticipantLearningCurve <- function(id, location){
  #same as rotation, for now we keep measures to degrees for eeg matching
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'mirror')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
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
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)]))#/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets15aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1))#/15)*100
      RT$compensate[which(RT$targetangle == target)] <- 15
    } else if (target %in% alltargets30bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)]))#/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets30aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1))#/30)*100
      RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargets45bef){
      RT$reachdev[which(RT$targetangle == target)] <- ((RT$reachdev[which(RT$targetangle == target)]))#/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    } else if (target %in% alltargets45aft){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1))#/45)*100
      RT$compensate[which(RT$targetangle == target)] <- 45
    }
  }
  #write.csv(RT, file='data/PPLCmir.csv', row.names = F)
  return(RT)  
}

#grab reachdevs across all targets for EEG data
getMIRGroupLCALL <- function(maxppid=31, location='feedback') { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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

getMIRGroupLearningCurves <- function(maxppid, location, angles = c(15, 30, 45)) { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  
  #get csv for each angle, combining all into one plot will produce great variabilities
  
  for(angle in angles){
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      ppangles <- getMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
      
      # reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      # trial <- c(1:length(reaches)) #sets up trial column
      # dat <- cbind(trial, reaches)
      #rdat <- dat$reaches
      
      # if (prod(dim(dataoutput)) == 0){
      #   dataoutput <- dat
      # } else {
      #   dataoutput <- cbind(dataoutput, reaches)
      # }
      
      for (irow in c(1:nrow(ppangles))){
        subdat <- ppangles[irow,]
        if (subdat$compensate != angle){
          subdat$reachdev <- NA
          ppangles[irow,] <- subdat
        }
      }
      
      reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
    }
    write.csv(dataoutput, file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle), row.names = F) 
  }
  #return(dataoutput)
  
}

getMIRGroupConfidenceInterval <- function(angles = c(15,30,45), type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  #compangle <- c(15,30,45)
  #for (comp in compangle){
  #data <- getMIRGroupLearningCurves(group = group, maxppid = maxppid, location = location) #angle = comp
  #data <- data[,-6] #remove faulty particiapnt (pp004) so the 6th column REMOVE ONCE RESOLVED
  for(angle in angles){
    data <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    
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
      
      write.csv(confidence, file=sprintf('data/MIR_CI_learningcurve_%02d.csv', angle), row.names = F) 
      
    }
  }
}

plotMIRLearningCurves <- function(angles = c(15,30,45), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig7_MIR_learningcurve_degrees.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,91), ylim = c(-20, 60), 
       xlab = "Trial", ylab = "Amount of Compensation (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: MIR", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 15, 30, 45), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90)) #tick marks for x axis
  axis(2, at = c(-15, -5, 0, 5, 15, 30, 45,60)) #tick marks for y axis
  
  for(angle in angles){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/MIR_CI_learningcurve_%02d.csv', angle))
    
    colourscheme <- getColourScheme(angles = angle)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[angle]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:90), rev(c(1:90))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[angle]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (angle in angles) {
    # plot mean reaches for each group
    col <- colourscheme[[angle]][['S']]
    lines(meanGroupReaches[[angle]],col=col,lty=1, lwd=2)
  }
  
  #add legend
  legend(70,0,legend=c('15°','30°','45°'),
         col=c(colourscheme[[15]][['S']],colourscheme[[30]][['S']],colourscheme[[45]][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning curves FIRST RANDOM ROTATION BLOCK----
getBlock1RDMParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  #regardless of ID, we want the first random block
  rotatedTraining <- getParticipantTaskData(id, taskno = 3, task = 'random0')
  
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])*-1)
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)]))
    }
  }
  
  
  
  
  return(RT)
}

getBlock1RDMGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getBlock1RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMROT_FirstBlock_learningcurve_degrees.csv', row.names = F) 
}

getBlock1RDMGroupLearningCurves <- function(maxppid, location, angles = c(15, 25, 35)) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  for (angle in angles){
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      ppangles <- getBlock1RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
      ppangles$rotation <- abs(ppangles$rotation)
      for (irow in c(1:nrow(ppangles))){
        subdat <- ppangles[irow,]
        if (subdat$rotation != angle){
          subdat$reachdev <- NA
          ppangles[irow,] <- subdat
        }
      }
      
      reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
      
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/RDMROT_FirstBlock_learningcurve_degrees_%02d.csv', angle), row.names = F) 
  }
}

getBlock1RDMGroupConfidenceInterval <- function(maxppid = 31, location = 'feedback', type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getBlock1RDMGroupLCALL(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/RDMROT_CI_FirstBlock_learningcurve.csv', row.names = F) 
    
    
  }
  #}
}

plotBlock1RDMLearningCurves <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/FigX_ROT_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,48), ylim = c(-10,36), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: First block RDM", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 48)) #tick marks for x axis
  axis(2, at = c(-10, 0, 10, 20, 30, 35)) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  groupconfidence <- read.csv(file='data/RDMROT_CI_FirstBlock_learningcurve.csv')
  
  colourscheme <- list('S'='#e51636ff', #vivid/york red
                       'T'='#e516362f')
  
  #colourscheme <- getColourScheme(groups = group)
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  col <- colourscheme[['T']] #use colour scheme according to group
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
  
  meanGroupReaches <- mid #use mean to fill in empty list for each group
  
  
  
  
  # plot mean reaches for each group
  col <- colourscheme[['S']]
  lines(meanGroupReaches,col=col,lty=1)
  
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Learning curves SECOND RANDOM ROTATION BLOCK----
getBlock2RDMParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  #regardless of ID, we want the first random block
  rotatedTraining <- getParticipantTaskData(id, taskno = 9, task = 'random1')
  
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])*-1)
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)]))
    }
  }
  
  
  
  
  return(RT)
}

getBlock2RDMGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getBlock2RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMROT_FirstBlock_learningcurve_degrees.csv', row.names = F) 
}

getBlock2RDMGroupLearningCurves <- function(maxppid, location, angles = c(15, 25, 35)) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  for (angle in angles){
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      ppangles <- getBlock2RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
      ppangles$rotation <- abs(ppangles$rotation)
      for (irow in c(1:nrow(ppangles))){
        subdat <- ppangles[irow,]
        if (subdat$rotation != angle){
          subdat$reachdev <- NA
          ppangles[irow,] <- subdat
        }
      }
      
      reaches <- ppangles$reachdev #get reach deviations column from learning curve data
      trial <- c(1:length(reaches)) #sets up trial column
      dat <- cbind(trial, reaches)
      
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, reaches)
      }
      
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/RDMROT_SecondBlock_learningcurve_degrees_%02d.csv', angle), row.names = F) 
  }
}

getBlock2RDMGroupConfidenceInterval <- function(maxppid = 31, location = 'feedback', type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getBlock2RDMGroupLCALL(maxppid = maxppid, location = location)
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
    
    write.csv(confidence, file='data/RDMROT_CI_SecondBlock_learningcurve.csv', row.names = F) 
    
    
  }
  #}
}

plotBlock2RDMLearningCurves <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/FigX_ROT_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,48), ylim = c(-10,36), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach Learning over Time: Second block RDM", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 10, 20, 30, 40, 48)) #tick marks for x axis
  axis(2, at = c(-10, 0, 10, 20, 30, 35)) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  groupconfidence <- read.csv(file='data/RDMROT_CI_SecondBlock_learningcurve.csv')
  
  colourscheme <- list('S'='#e51636ff', #vivid/york red
                       'T'='#e516362f')
  
  #colourscheme <- getColourScheme(groups = group)
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  col <- colourscheme[['T']] #use colour scheme according to group
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
  
  meanGroupReaches <- mid #use mean to fill in empty list for each group
  
  
  
  
  # plot mean reaches for each group
  col <- colourscheme[['S']]
  lines(meanGroupReaches,col=col,lty=1)
  
  
  #add legend
  # legend(70,-100,legend=c('Non-Instructed','Instructed'),
  #        col=c(colourscheme[['noninstructed']][['S']],colourscheme[['instructed']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plot aligned and learning curves for rot and random----
plotROTRDMLearningCurves <- function(tasks = c('aln', 'rot', 'rdm'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_ROTRDM_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,187), ylim = c(-10,35), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach learning over time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(1,48), labels=c('1','48'))
  axis(side=1, at=c(49,96), labels=c('1','48'))
  axis(side=1, at=c(97,186), labels=c('1','90'))
  axis(2, at = c(0, 5, 10, 15, 20, 25, 30), las = 2) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  for(task in tasks){
    if(task == 'rot'){
      groupconfidence <- read.csv(file='data/ROT_CI_learningcurve.csv')
    } else if(task == 'rdm'){
      groupconfidence <- read.csv(file='data/RDMROT_CI_learningcurve.csv')
    } else if(task == 'aln'){
      groupconfidence <- read.csv(file='data/ALN_CI_learningcurve.csv')
    }
    
    
    colourscheme <- getPtypeColourScheme(tasks=task)
    
    #colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[task]][['T']]
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if(task == 'rot'){
      polygon(x = c(c(97:186), rev(c(97:186))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(97:186), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'rdm'){
      polygon(x = c(c(49:96), rev(c(49:96))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(49:96), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'aln'){
      polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(1:48), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    }
    
    
    

  }
  
  
  
  #add legend
  legend(110,0,legend=c('ALIGNED', 'ROT','RDM'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plot aligned, rdmrot, rot, rdmmir, mir----
plotExperimentLearningCurves <- function(tasks = c('aln', 'rdmrot', 'rot', 'rdmmir', 'mir15', 'mir30', 'mir45'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig12_Experiment_learningcurve.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0,325), ylim = c(-20,65), 
       xlab = "Trial", ylab = "Angular deviation of hand (°)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Reach learning over time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0, 15, 30, 45), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(1,48), labels=c('1','48'))
  axis(side=1, at=c(49,96), labels=c('','48'))
  axis(side=1, at=c(97,186), labels=c('','90'))
  axis(side=1, at=c(187,234), labels=c('','48'))
  axis(side=1, at=c(235,324), labels=c('','90'))
  axis(2, at = c(-15, 0, 15, 25, 30, 35, 45), las = 2) #tick marks for y axis
  
  
  #read in files created by getGroupConfidenceInterval in filehandling.R
  for(task in tasks){
    if(task == 'rot'){
      groupconfidence <- read.csv(file='data/ROT_CI_learningcurve.csv')
    } else if(task == 'rdmrot'){
      groupconfidence <- read.csv(file='data/RDMROT_CI_FirstBlock_learningcurve.csv')
    } else if(task == 'aln'){
      groupconfidence <- read.csv(file='data/ALN_CI_learningcurve.csv')
    } else if(task == 'rdmmir'){
      groupconfidence <- read.csv(file='data/RDMROT_CI_SecondBlock_learningcurve.csv') #replaced with first and second block of rdm
    } else if(task == 'mir15'){
      groupconfidence <- read.csv(file='data/MIR_CI_learningcurve_15.csv')
    } else if(task == 'mir30'){
      groupconfidence <- read.csv(file='data/MIR_CI_learningcurve_30.csv')
    } else if(task == 'mir45'){
      groupconfidence <- read.csv(file='data/MIR_CI_learningcurve_45.csv')
    }
    
    
    colourscheme <- getPtypeColourScheme(tasks=task)
    
    #colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[task]][['T']]
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    if(task == 'rot'){
      polygon(x = c(c(97:186), rev(c(97:186))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(97:186), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'rdmrot'){
      polygon(x = c(c(49:96), rev(c(49:96))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(49:96), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'aln'){
      polygon(x = c(c(1:48), rev(c(1:48))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(1:48), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'rdmmir'){
      polygon(x = c(c(187:234), rev(c(187:234))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(187:234), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'mir15'){
      polygon(x = c(c(235:324), rev(c(235:324))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(235:324), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'mir30'){
      polygon(x = c(c(235:324), rev(c(235:324))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(235:324), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    } else if (task == 'mir45'){
      polygon(x = c(c(235:324), rev(c(235:324))), y = c(lower, rev(upper)), border=NA, col=col)
      meanGroupReaches <- mid #use mean to fill in empty list for each group
      # plot mean reaches for each group
      col <- colourscheme[[task]][['S']]
      lines(x = c(235:324), y = meanGroupReaches, col = col, lty = 1, lwd = 2)
    }
    
    
    
    
  }
  
  
  
  #add legend
  legend(1,65,legend=c('ALIGNED', 'RDM','ROT','MIR, 15°', 'MIR, 30°', 'MIR, 45°'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rdmrot']][['S']],colourscheme[['rot']][['S']],colourscheme[['mir15']][['S']],colourscheme[['mir30']][['S']],colourscheme[['mir45']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plot histograms for error distribution-----
getDists <- function(maxppid = 31, location = 'feedback'){
  
  participants <- seq(0,maxppid,1)
  par(mfrow = c(3,3))
  
  for(participant in participants){
    #specify function for perturbation
    data <- getMIRParticipantLearningCurve(id = participant, location = location)
    hist(data$reachdev, xlim = c(-180, 180), ylim = c(0,15), breaks = 90,
         main = sprintf('Participant %s', participant), xlab = 'Degrees')
  }
}

# Learning Curves using Amount of Compensation----
getPercCompROTParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'rotation')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'rotation')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
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
  
  angles <- unique(RT$targetangle)
  #RT['compensate'] <- NA
  
  for (target in angles){
    if (target %in% alltargetsbef){
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100
      #RT$compensate[which(RT$targetangle == target)] <- 30
    } else if (target %in% alltargetsaft){
      #multiply by negative 1 bec targets after axis will have negative values
      RT$reachdev[which(RT$targetangle == target)] <- (((RT$reachdev[which(RT$targetangle == target)])*-1)/30)*100 #need to multiply by neg 1, rotation always in same direction
      #RT$compensate[which(RT$targetangle == target)] <- 30
    }
  }
  
  
  return(RT)
}

getPercCompROTGroupLearningCurves <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getPercCompROTParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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

getPercCompMIRParticipantLearningCurve <- function(id, location){
  #same as rotation, for now we keep measures to degrees for eeg matching
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  if (id%%2 == 1){
    #mirror then rotation if odd id
    rotatedTraining <- getParticipantTaskData(id, taskno = 5, task = 'mirror')
  } else if (id%%2 == 0){
    #if pp id is even
    #rotation first then mirror
    rotatedTraining <- getParticipantTaskData(id, taskno = 11, task = 'mirror')
  }
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=89, location = location) #rotated is 90 trials; appended to end of aligned
  
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
getPercCompMIRGroupLCALL <- function(maxppid=31, location='feedback') { # add angle?
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppangles <- getPercCompMIRParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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

getPercCompBlock1RDMParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  #regardless of ID, we want the first random block
  rotatedTraining <- getParticipantTaskData(id, taskno = 3, task = 'random0')
  
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    rotsize <- abs(rot)
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- (((RT$reachdev[which(RT$trial == t)])*-1)/rotsize)*100
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])/rotsize)*100
    }
  }
  
  
  
  
  return(RT)
}

getPercCompBlock1RDMGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getPercCompBlock1RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMROT_FirstBlock_learningcurve_degrees.csv', row.names = F) 
}

getPercCompBlock2RDMParticipantLearningCurve <- function(id, location) {
  
  #take learnive curve for both aligned and perturbed (rot, mir, rand) sessions
  #rotation should show percentage of compensation (not angular deviation of hand)
  #because this makes it comparable to mirror reversal where angular deviation will differ depending on location of target relative to mirror
  #measure where hand should be minus where it is: if this is spot on then percentage is 0%
  
  alignedTraining <- getParticipantTaskData(id, taskno = 1, task = 'aligned') #these values will change if need nocursor or localization
  
  #regardless of ID, we want the first random block
  rotatedTraining <- getParticipantTaskData(id, taskno = 9, task = 'random1')
  
  
  biases <- getAlignedTrainingBiases(alignedTraining, location = location) #use function to get biases
  #AT<- getReachAngles(alignedTraining, starttrial = 1, endtrial = 45) #aligned is first 45 trials
  RT<- getReachAngles(rotatedTraining, starttrial=0, endtrial=47, location = location) #rotated is 90 trials; appended to end of aligned
  
  #include rotation values, because they differ
  rotation <- c()
  for (trialno in c(0: dim(RT)[1] - 1)){
    rotsize <- unique(rotatedTraining$rotation[which(rotatedTraining$trial == trialno)])
    rotation <- c(rotation, rotsize)
  }
  
  RT$rotation <- rotation
  
  for (biasno in c(1: dim(biases)[1])){ #from 1 to however many biases there are in data
    
    target<- biases[biasno, 'targetangle'] #get corresponding target angle
    bias<- biases[biasno, 'reachdev'] #get corresponding reachdev or bias
    
    #subtract bias from reach deviation for rotated session only
    RT$reachdev[which(RT$targetangle == target)] <- RT$reachdev[which(RT$targetangle == target)] - bias
  }
  
  
  
  negvals <- c(-15, -25, -35)
  posvals <- c(15, 25, 35)
  
  for (t in RT$trial){
    rot <- RT$rotation[which(RT$trial == t)]
    rotsize <- abs(rot)
    if (rot %in% posvals){
      RT$reachdev[which(RT$trial == t)] <- (((RT$reachdev[which(RT$trial == t)])*-1)/rotsize)*100
    } else if (rot %in% negvals){
      RT$reachdev[which(RT$trial == t)] <- ((RT$reachdev[which(RT$trial == t)])/rotsize)*100
    }
  }
  
  
  
  
  return(RT)
}

getPercCompBlock2RDMGroupLCALL <- function(maxppid, location) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    #print(participant)
    ppangles <- getPercCompBlock2RDMParticipantLearningCurve(id=participant, location = location) #for every participant, get learning curve data
    
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
  #write.csv(dataoutput, file='data/RDMROT_FirstBlock_learningcurve_degrees.csv', row.names = F) 
}

# Learning Curves Blocked and INDIVIDUAL Data----

#first split 90 trials into sets of 6 trials each
#then in each set, plot individual data as lines

getBlockedIndividualLearningCurves <- function(maxppid = 31, location = 'feedback', targetno = 6, perturb){
  
  if (perturb == 'ROT'){
    data <- getPercCompROTGroupLearningCurves(maxppid = maxppid, location = location)
  } else if (perturb == 'MIR'){
    data <- getPercCompMIRGroupLCALL(maxppid = maxppid, location = location)
  } else if (perturb == 'RDM0'){
    data <- getPercCompBlock1RDMGroupLCALL(maxppid = maxppid, location = location)
  } else if (perturb == 'RDM1'){
    data <- getPercCompBlock2RDMGroupLCALL(maxppid = maxppid, location = location)
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


plotBlockedIndLC <- function(maxppid = 31, location = 'feedback', targetno = 6, perturb, target='inline'){
  
  if (perturb == 'ROT'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/pilot/Fig29A_ROT_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Visuomotor rotation", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    
    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
      #linetypeidx <- linetypeidx + 1
      #colidx <- colidx +1
    }
    
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    
  } else if (perturb == 'MIR'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/pilot/Fig29A_MIR_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,16), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Mirror reversal", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)

    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
  } else if (perturb == 'RDM0'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/pilot/Fig29A_RDM0_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Random Rotation Block 1", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    
    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
  } else if (perturb == 'RDM1'){
    
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file='doc/fig/pilot/Fig29A_RDM1_BlockedIndLearningCurve.svg', width=12, height=7, pointsize=16, system_fonts=list(sans="Arial"))
    }
    
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    #remove pp004 because they anti-learned
    #data <- subset(data, participant != 'pp4')
    # data <- subset(data, participant != 'pp0')
    # data <- subset(data, participant != 'pp1')
    
    plot(NA, NA, xlim = c(0,9), ylim = c(-200,210), 
         xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
         main = "Random Rotation Block 1", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(h = 0, col = '#000000', lty = 2)
    axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
    axis(2, at = c(-200, -100, 0, 100, 200)) #tick marks for y axis
    
    
    participants <- unique(data$participant)
    
    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      points(data$trial[row.idx],data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      
    }
    
    #then create a mean for all, according to trial number
    blockno <- unique(data$trial)
    allmeans <- data.frame()
    for(block in blockno){
      dat <- data[which(data$trial == block),]
      meandist <- getConfidenceInterval(data=dat$reachdev, method='bootstrap', resamples=5000, FUN=mean, returndist=TRUE)
      blockmean <- mean(dat$reachdev)
      col <- colourscheme[[perturb]][['S']]
      lines(x=rep(block,2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
      #print(meandist$CI95)
      points(x=block,y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
    }
    
    lines(x=c(1:length(blockno)),y=allmeans[,1], lwd = 2, lty = 1, col = col)
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotCollapsedBlockedIndLC <- function(maxppid=31, location='feedback', targetno=6, perturbtypes=c('ROT','MIR'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig29_ROTMIRBlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,15), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = '#000000', lty = 2)
  axis(1, at=c(1, 5, 10, 15))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(perturb in perturbtypes){
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    
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
  legend(12,-90,legend=c('ROT','MIR'),
         col=c(colourscheme[['ROT']][['S']],colourscheme[['MIR']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotRDMCollapsedBlockedIndLC <- function(maxppid=31, location='feedback', targetno=6, perturbtypes=c('RDM0','RDM1'), target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig30_RDMBlockedIndLearningCurve.svg', width=11.5, height=10.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  plot(NA, NA, xlim = c(1,9), ylim = c(-200,250), 
       xlab = "Block", ylab = "Amount of compensation (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = "", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = 100, col = '#000000', lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(h = 0, col = '#000000', lty = 2)
  axis(1, at=c(1, 3, 5, 8))#, labels=c('Exclusive', 'Inclusive')) #tick marks for x axis
  axis(2, at = c(-200, -100, 0, 100, 200), las=2) #tick marks for y axis
  
  for(perturb in perturbtypes){
    data <- getBlockedIndividualLearningCurves(maxppid = maxppid, location = location, targetno = targetno, perturb = perturb)
    
    participants <- unique(data$participant)
    
    colourscheme <- getBehaviorColourScheme(perturb = perturb)
    
    for (pp in participants){
      row.idx <- which(data$participant == pp)
      col <- colourscheme[[perturb]][['T']]
      #lines(data$trial[row.idx],data$reachdev[row.idx], lwd = 2, lty = 1, col = col)
      if(perturb == 'RDM0'){
        points(data$trial[row.idx]-(1/6),data$reachdev[row.idx], pch = 16, cex=1.5, col = alpha(col, .15))
      } else if (perturb == 'RDM1'){
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
      if(perturb == 'RDM0'){
        lines(x=rep((block-(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block-(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      } else if(perturb == 'RDM1'){
        lines(x=rep((block+(1/6)),2),y=meandist$CI95,col=col) #as.character(styles$color_solid[groupno]))
        points(x=(block+(1/6)),y=blockmean,pch=16,cex=1.5,col=col) #as.character(styles$color_solid[groupno]))
      }
      
      
      
      if (prod(dim(allmeans)) == 0){
        allmeans <- blockmean
      } else {
        allmeans <- rbind(allmeans, blockmean)
      }
      
    }
    
    if(perturb == 'RDM0'){
      lines(x=c((1-(1/6)):(length(blockno)-(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    } else if(perturb == 'RDM1'){
      lines(x=c((1+(1/6)):(length(blockno)+(1/6))),y=allmeans[,1], lwd = 2, lty = 1, col = col)
    }
    
  }
  
  legend(5,-90,legend=c('RDM: 1st block','RDM: 2nd block'),
         col=c(colourscheme[['RDM0']][['S']],colourscheme[['RDM1']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}