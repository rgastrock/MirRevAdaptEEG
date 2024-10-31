source('ana/shared.R')
source('ana/learningRates.R')

#Reaction Time Analysis -----

#start of step 3 is when target signal turns to go
#end of step 3 (when it switches to step 4) is when cursor distance from home is greater than radius (which is 0.5 cm * pixpercm or 35)
#so essentially, RT is difference between time that go signal occurs and time when cursor is half a centimeter away from home position

getRTTrials <- function(id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  
  dat <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 3,]
    subndatsamp <- subndat[1,]
    
    if (nrow(subndat)==0){
      reactiontime <- NA #will assign NA if step 3 does not occur
      trial <- trialno
      comment <- 'nostep' #this will help to keep track of how many trials did not have a step later on
      
    } else if(subndatsamp$trialselected_bool == 0){#we only want data that has been selected
      reactiontime <- NA
      trial <- trialno
      comment <- 'unselected'
    } else{
      firststep3 <- subndat[1,]
      laststep3 <- subndat[nrow(subndat),]
      
      step3start <- firststep3$time_ms
      step3end <- laststep3$time_ms
      
      reactiontime <- step3end - step3start
      trial <- trialno
      comment <- 'selected'
      
    }
    feedback <- c(trial, reactiontime, task, comment)
    
    
    if (prod(dim(proportion)) == 0){
      proportion <- feedback
    } else {
      proportion <- rbind(proportion, feedback)
    }
  }
  proportion <- data.frame(proportion, row.names = NULL, stringsAsFactors = F)
  colnames(proportion) <- c('trial', 'reaction_time', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getAlignedGroupRTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
 
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getRTTrials(id=participant, task = 'aligned', taskno = 1) #for every participant, get RT data
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    #task <- rep('AL', length(reaction))
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTAligned <- function(maxppid = 31){
  
  dat <- getAlignedGroupRTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedAlignedConfInt <- function(maxppid = 31, type = 'b'){
  
  #run with outlier removal?
  data <- getRTAligned(maxppid = maxppid)
  #run without outlier removal?
  #data <- getAlignedGroupRTTrials(group = group, maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ALIGNED_blocked_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getRTAlignedConfInt <- function(maxppid = 31, type = 'b'){
  
  #run with outlier removal?
  data <- getRTAligned(maxppid = maxppid)
  #run without outlier removal?
  #data <- getAlignedGroupRTTrials(group = group, maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ALIGNED_trialbytrial_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for ROTATION

getROTGroupRTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(id=participant, taskno = 5, task = 'rotation')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTRot <- function(maxppid = 31){
  
  dat <- getROTGroupRTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedROTConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRTRot(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ROT_blocked_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getRTROTConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTRot(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ROT_trialbytrial_CI_RT.csv', row.names = F) 
   
    
  }
  
}

#Do the same for MIRROR

getMIRGroupRTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
 
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getRTTrials(id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getRTTrials(id=participant, taskno = 11, task = 'mirror')
    }
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTMir <- function(maxppid = 31){
  
  dat <- getMIRGroupRTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedMIRConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRTMir(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    write.csv(confidence, file='data/MIR_blocked_CI_RT.csv', row.names = F) 
   
    
  }
  
}

#function below generates CIs for every trial
getRTMIRConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTMir(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/MIR_trialbytrial_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for RANDOM FIRST BLOCK

getRDM0GroupRTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getRTTrials(id=participant, taskno = 3, task = 'random0')
   
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTRDM0 <- function(maxppid = 31){
  
  dat <- getRDM0GroupRTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedRDM0ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRTRDM0(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    write.csv(confidence, file='data/RDM0_blocked_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getRTRDM0ConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTRDM0(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM0_trialbytrial_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for RANDOM SECOND BLOCK

getRDM1GroupRTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getRTTrials(id=participant, taskno = 9, task = 'random1')
    
    
    reaction <- as.numeric(ppRT$reaction_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getRTRDM1 <- function(maxppid = 31){
  
  dat <- getRDM1GroupRTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times below 200 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat < 200)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getRTBlockedRDM1ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRTRDM1(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    write.csv(confidence, file='data/RDM1_blocked_CI_RT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getRTRDM1ConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRTRDM1(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM1_trialbytrial_CI_RT.csv', row.names = F) 
    
    
  }
  
}

plotBlockedRT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig31_blocked_reactiontime.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_blocked_CI_RT.csv')
  dat2 <- read.csv(file='data/RDM0_blocked_CI_RT.csv')
  dat3 <- read.csv(file='data/ROT_blocked_CI_RT.csv')
  dat4 <- read.csv(file='data/RDM1_blocked_CI_RT.csv')
  dat5 <- read.csv(file='data/MIR_blocked_CI_RT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,12,1)
  X5 <- seq(13,27,1)
  X7 <- seq(28,35,1)
  X9 <- seq(36,50,1)
  
  Y <- as.numeric(dat$X50.)
  YLow <- as.numeric(dat$X2.5.)
  YUp <- as.numeric(dat$X97.5.)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Block', ylab = 'Reaction time (ms)', main = '',
       xlim = c(0,51), ylim = c(199,800))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,13,28,36,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(200, 300, 400, 500, 600, 700, 800),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,12.5,27.5,35.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col='#A9A9A9ff')
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:12], rev(YUp[5:12])), border=NA, col='#ff82002f')
  polygon(x = c(X5, rev(X5)), y = c(YLow[13:27], rev(YUp[13:27])), border=NA, col='#e516362f')
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:35], rev(YUp[28:35])), border=NA, col='#c400c42f')
  polygon(x = c(X9, rev(X9)), y = c(YLow[36:50], rev(YUp[36:50])), border=NA, col='#005de42f')
  
  lines(X1,Y[1:4], col = alpha('#000000', 1))#aligned
  lines(X3, Y[5:12], col = alpha('#ff8200ff', 1))#rdm0
  lines(X5, Y[13:27], col = alpha('#e51636ff', 1))#rot
  lines(X7, Y[28:35], col = alpha('#c400c4ff', 1))#rdm1
  lines(X9, Y[36:50], col = alpha('#005de4ff', 1))#mir
  
  
  
  # #add legend
  # legend(5,800,legend=c('Aligned','RDM: Block 1','ROT', 'RDM: Block 2', 'MIR'),
  #        col=c("#000000", '#ff8200ff', "#e51636ff", '#c400c4ff', "#005de4ff"),
  #        lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Movement time analysis ----
getMTTrials <- function(id, task, taskno){
  #allows for this function to work with each file
  #specify pp id, the task type, and task number
  #note that task type and taskno have to match, depending on present csv files
  
  
  dat <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  trials <- unique(dat$trial) 
  proportion <- data.frame()
  
  for (trialno in trials){
    
    subndat <- dat[dat$trial == trialno,]
    subndat <- subndat[subndat$step == 4,]
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
      firststep4 <- subndat[1,]
      laststep4 <- subndat[nrow(subndat),]
      
      step4start <- firststep4$time_ms
      step4end <- laststep4$time_ms
      
      movementtime <- step4end - step4start
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
  colnames(proportion) <- c('trial', 'movement_time', 'task', 'comment')
  proportion$participant <- id
  return(proportion)
}

getAlignedGroupMTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getMTTrials(id=participant, task = 'aligned', taskno = 1) #for every participant, get RT data
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    #task <- rep('AL', length(reaction))
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTAligned <- function(maxppid = 31){
  
  dat <- getAlignedGroupMTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times above 1000 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA #remove all greater than 1 second
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedAlignedConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMTAligned(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ALIGNED_blocked_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getMTAlignedConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTAligned(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ALIGNED_trialbytrial_CI_MT.csv', row.names = F) 
   
    
  }
  
}

#Do the same for ROTATION

getROTGroupMTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getMTTrials(id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(id=participant, taskno = 5, task = 'rotation')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTRot <- function(maxppid = 31){
  
  dat <- getROTGroupMTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times above 1000 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedROTConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMTRot(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ROT_blocked_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getMTROTConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTRot(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ROT_trialbytrial_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for MIRROR

getMIRGroupMTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  participants <- seq(0,maxppid,1)
 
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      ppRT <- getMTTrials(id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      ppRT <- getMTTrials(id=participant, taskno = 11, task = 'mirror')
    }
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTMir <- function(maxppid = 31){
  
  dat <- getMIRGroupMTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times above 1000(rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedMIRConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMTMir(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/MIR_blocked_CI_MT.csv', row.names = F) 
   
    
  }
  
}

#function below generates CIs for every trial
getMTMIRConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTMir(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/MIR_trialbytrial_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for RANDOM FIRST BLOCK

getRDM0GroupMTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getMTTrials(id=participant, taskno = 3, task = 'random0')
    
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTRDM0 <- function(maxppid = 31){
  
  dat <- getRDM0GroupMTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times above 1000 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedRDM0ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMTRDM0(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    write.csv(confidence, file='data/RDM0_blocked_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getMTRDM0ConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTRDM0(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM0_trialbytrial_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#Do the same for RANDOM SECOND BLOCK

getRDM1GroupMTTrials <- function(maxppid = 31) {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  #a consequence of adding the groups late led me to fix it in the manner below
  participants <- seq(0,maxppid,1)
  
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    ppRT <- getMTTrials(id=participant, taskno = 9, task = 'random1')
    
    
    reaction <- as.numeric(ppRT$movement_time)#get RT column from RT data
    trial <- c(1:length(reaction)) #sets up trial column
    dat <- cbind(trial, reaction)
    #rdat <- dat$reaches
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, reaction)
    }
    
  }
  
  return(dataoutput)
  
}

#Implement outlier removal
getMTRDM1 <- function(maxppid = 31){
  
  dat <- getRDM1GroupMTTrials(maxppid=maxppid)
  dat <- as.data.frame(dat)
  trials <- dat$trial
  
  dataoutput <- data.frame()
  
  for (trialno in trials){
    #go through each trial, get reaches, calculate mean and sd, then if it is greater than 2 sd, replace with NA
    ndat <- as.numeric(dat[trialno, 2:ncol(dat)])
    #remove all times above 1000 (rough estimate based on plotting everything)
    #i do this here instead of for all so that I wouldn't need to switch between a matrix and df format
    ndat[which(ndat > 1000)] <- NA
    trialmu <- mean(ndat, na.rm = TRUE)
    trialsigma <- sd(ndat, na.rm = TRUE)
    #print(trialsigma)
    trialclip <- abs(trialmu) + (trialsigma * 2)
    
    ndat[which(abs(ndat) > trialclip)] <- NA
    # average <- mean(ndat, na.rm = TRUE)
    # sd <- sd(ndat, na.rm = TRUE)
    # metrics <- cbind(average, sd)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ndat
    } else {
      dataoutput <- rbind(dataoutput, ndat)
    }
    
    
    #dat[trialno, 3:ncol(dat)] <- ndat
    
    #dat$average[trialno] <- mean(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
    #dat$sd[trialno] <- sd(as.numeric(dat[trialno, 3:18]), na.rm = TRUE)
  }
  dataoutput <- as.data.frame(dataoutput)
  dat <- cbind(trials,dataoutput)  
  
  return(dat)
}

getMTBlockedRDM1ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMTRDM1(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    write.csv(confidence, file='data/RDM1_blocked_CI_MT.csv', row.names = F) 
    
    
  }
  
}

#function below generates CIs for every trial
getMTRDM1ConfInt <- function(maxppid = 31, type = 'b'){
  
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMTRDM1(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM1_trialbytrial_CI_MT.csv', row.names = F) 
    
    
  }
  
}

plotBlockedMT <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig32_blocked_movementtime.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_blocked_CI_MT.csv')
  dat2 <- read.csv(file='data/RDM0_blocked_CI_MT.csv')
  dat3 <- read.csv(file='data/ROT_blocked_CI_MT.csv')
  dat4 <- read.csv(file='data/RDM1_blocked_CI_MT.csv')
  dat5 <- read.csv(file='data/MIR_blocked_CI_MT.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,12,1)
  X5 <- seq(13,27,1)
  X7 <- seq(28,35,1)
  X9 <- seq(36,50,1)
  
  Y <- as.numeric(dat$X50.)
  YLow <- as.numeric(dat$X2.5.)
  YUp <- as.numeric(dat$X97.5.)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Block', ylab = 'Movement time (ms)', main = '',
       xlim = c(0,51), ylim = c(50,200))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,13,28,36,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(50, 75, 100, 125, 150, 175, 200),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,12.5,27.5,35.5), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col='#A9A9A9ff')
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:12], rev(YUp[5:12])), border=NA, col='#ff82002f')
  polygon(x = c(X5, rev(X5)), y = c(YLow[13:27], rev(YUp[13:27])), border=NA, col='#e516362f')
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:35], rev(YUp[28:35])), border=NA, col='#c400c42f')
  polygon(x = c(X9, rev(X9)), y = c(YLow[36:50], rev(YUp[36:50])), border=NA, col='#005de42f')
  
  lines(X1,Y[1:4], col = alpha('#000000', 1))#aligned
  lines(X3, Y[5:12], col = alpha('#ff8200ff', 1))#rdm0
  lines(X5, Y[13:27], col = alpha('#e51636ff', 1))#rot
  lines(X7, Y[28:35], col = alpha('#c400c4ff', 1))#rdm1
  lines(X9, Y[36:50], col = alpha('#005de4ff', 1))#mir
  
  
  
  # #add legend
  # legend(5,200,legend=c('Aligned','RDM: Block 1','ROT', 'RDM: Block 2', 'MIR'),
  #        col=c("#000000", '#ff8200ff', "#e51636ff", '#c400c4ff', "#005de4ff"),
  #        lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Path Length analysis----
#Every sample has (x,y) coordinate.
# First sample will have distance from origin as sqrt(x^2 + y^2) - called absolute vector
# Path length is total distance (given x and y trajectories) traveled between movement onset and offset.
# One may think that simply calculating absolute vector from endpoint will measure this, but trajectories
# may curve or go in different directions. So we need to account for every sample.But we simply can't
# add absolute vectors across samples (this will include lengths accounted for by previous sample). So,
# every new sample's absolute vector will be calculated using the previous sample as its origin. Then all
# these values are added to come up with a total path length.
#Repeat this process for all trials within one participant. Then show mean measures across participants for every trial.
# Currently, measures are in cm. But can normalize this (divide path length by distance between target and home) to make it comparable
# to other experiments that have different measures.

getParticipantPathLength <- function(id, taskno, task){
  dat <- getParticipantTaskData(id=id, taskno=taskno, task=task)
  #get only selected trials, and the samples selected for this trial
  subdat <- dat[which(dat$trialselected_bool == 1),]
  subdat <- subdat[which(subdat$sampleselected_bool == 1),]
  trials <- unique(dat$trial) #need to be based off of dat, because some trials may not be selected - assign NA instead
  
  alldat <- data.frame()
  for (trialno in trials){
    trialdat <- subdat[which(subdat$trial == trialno),]
    trialdat <- trialdat[trialdat$step == 4,] 
    ndat <- data.frame()
    for (idx in 1:nrow(trialdat)){
      if (idx == 1){
        sampx <- trialdat$mousex_cm[idx]
        sampy <- trialdat$mousey_cm[idx]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      } else {
        sampx <- trialdat$mousex_cm[idx] - trialdat$mousex_cm[idx-1]
        sampy <- trialdat$mousey_cm[idx] - trialdat$mousey_cm[idx-1]
        absvec <- sqrt(((sampx)^2)+((sampy)^2))
      }
      
      
      if (prod(dim(ndat)) == 0){
        ndat <- absvec
      } else {
        ndat <- rbind(ndat, absvec)
      }
      
    }
    pathlength <- sum(ndat[1:length(ndat)])
    #print(pathlength)
    #print(trialno)
    #dat <- cbind(trialno, pathlength)
    
    if (prod(dim(alldat)) == 0){
      alldat <- pathlength
    } else {
      alldat <- rbind(alldat, pathlength)
    }
  }
  
  return(alldat)
  
}
#Aligned Reaches
getAlignedGroupPathLength <- function(maxppid = 31){
  
  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    pppath <- getParticipantPathLength(id=participant, taskno = 1, task = 'aligned')
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getAlignedPathLengthCI <- function(maxppid = 31, type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getAlignedGroupPathLength(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ALIGNED_trialbytrial_CI_PL.csv', row.names = F) 
    
    
  }
}

getPLBlockedAlignedConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getAlignedGroupPathLength(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 12 trials (they go through each of 12 possible targets in aligned)
  n <- 12;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ALIGNED_blocked_CI_PL.csv', row.names = F) 
    
    
  }
  
}

#Rotated reaches
getROTGroupPathLength <- function(maxppid =31){
  
  
  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(id=participant, taskno = 11, task = 'rotation')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(id=participant, taskno = 5, task = 'rotation')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getROTPathLengthCI <- function(maxppid =31, type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getROTGroupPathLength(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/ROT_trialbytrial_CI_PL.csv', row.names = F) 
    
    
  }
}

getPLBlockedROTConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getROTGroupPathLength(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/ROT_blocked_CI_PL.csv', row.names = F) 
    
    
  }
  
}

#Mirror reaches
getMIRGroupPathLength <- function(maxppid = 31){
  
  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    if (participant%%2 == 1){
      #mirror then rotation if odd id
      pppath <- getParticipantPathLength(id=participant, taskno = 5, task = 'mirror')
    } else if (participant%%2 == 0){
      #if pp id is even
      #rotation first then mirror
      pppath <- getParticipantPathLength(id=participant, taskno = 11, task = 'mirror')
    }
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getMIRPathLengthCI <- function(maxppid = 31, type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getMIRGroupPathLength(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/MIR_trialbytrial_CI_PL.csv', row.names = F) 
    
    
  }
}

getPLBlockedMIRConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getMIRGroupPathLength(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/MIR_blocked_CI_PL.csv', row.names = F) 
    
    
  }
  
}

#Random block 0 reaches
getRDM0GroupPathLength <- function(maxppid =31){
  
  
  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    pppath <- getParticipantPathLength(id=participant, taskno = 3, task = 'random0')
    
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getRDM0PathLengthCI <- function(maxppid =31, type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRDM0GroupPathLength(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM0_trialbytrial_CI_PL.csv', row.names = F) 
    
    
  }
}

getPLBlockedRDM0ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRDM0GroupPathLength(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/RDM0_blocked_CI_PL.csv', row.names = F) 
    
    
  }
  
}

#Random block 1 reaches
getRDM1GroupPathLength <- function(maxppid =31){
  
  
  participants <- seq(0,maxppid,1)
  
  
  dataoutput<- data.frame() #create place holder
  #go through each participant in this group
  for (participant in participants) {
    
    pppath <- getParticipantPathLength(id=participant, taskno = 9, task = 'random1')
    
    
    trial <- c(1:length(pppath))
    dat <- cbind(trial, pppath)
    #print(participant)
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- dat
    } else {
      dataoutput <- cbind(dataoutput, pppath)
    }
  }
  return(dataoutput)
}

getRDM1PathLengthCI <- function(maxppid =31, type = 'b'){
  #for (group in groups){
  # get the confidence intervals for each trial of each group
  data <- getRDM1GroupPathLength(maxppid = maxppid)
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
    
    write.csv(confidence, file='data/RDM1_trialbytrial_CI_PL.csv', row.names = F) 
    
    
  }
}

getPLBlockedRDM1ConfInt <- function(maxppid = 31, type = 'b'){
  
  data <- getRDM1GroupPathLength(maxppid = maxppid)
  subdat <- data[,2:ncol(data)]
  
  #we want to get the mean for every 6 trials (they go through each of 6 possible targets)
  n <- 6;
  ndat <- aggregate(subdat, list(rep(1:(nrow(data)%/%n+1),each=n,len=nrow(data))), mean, na.rm = T)[-1];
  blockno <- c(1:nrow(ndat))
  ndat <- as.data.frame(cbind(blockno,ndat))
  
  data1 <- as.matrix(ndat[,2:dim(ndat)[2]])
  confidence <- data.frame()
  
  
  for (block in blockno){
    cireaches <- data1[which(ndat$blockno == block), ]
    
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
    
    write.csv(confidence, file='data/RDM1_blocked_CI_PL.csv', row.names = F) 
    
    
  }
  
}

plotBlockedPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig33_blocked_pathlength.svg', width=11.5, height=8.5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #read in the csv files and plot them in one big plot
  dat1 <- read.csv(file='data/ALIGNED_blocked_CI_PL.csv')
  dat2 <- read.csv(file='data/RDM0_blocked_CI_PL.csv')
  dat3 <- read.csv(file='data/ROT_blocked_CI_PL.csv')
  dat4 <- read.csv(file='data/RDM1_blocked_CI_PL.csv')
  dat5 <- read.csv(file='data/MIR_blocked_CI_PL.csv')
  
  dat <- rbind(dat1, dat2, dat3, dat4, dat5)
  
  #separate each task, then plot as usual according to blocks
  
  X1 <- seq(1, 4,1)
  X3 <- seq(5,12,1)
  X5 <- seq(13,27,1)
  X7 <- seq(28,35,1)
  X9 <- seq(36,50,1)
  
  Y <- as.numeric(dat$X50.)
  YLow <- as.numeric(dat$X2.5.)
  YUp <- as.numeric(dat$X97.5.)
  
  plot(c(1:length(Y)), Y, type = 'n', axes = FALSE,
       xlab = 'Block', ylab = 'Path length (cm)', main = '',
       xlim = c(0,51), ylim = c(4.0,5.5))
  
  #labs <- c('1:AL','9:ROT','24:WASH','32:MIR','47:WASH','54')
  #axis(side=1, at=c(1,9,24,32,47,54), labels=labs)
  axis(side=1, at=c(1,5,13,28,36,50))
  #mtext('Trial & Task', side = 1, outer = TRUE, line=-1, cex = 1)
  axis(side=2, at=c(4, 4.25, 4.5, 4.75, 5.0, 5.25, 5.5),las=2)
  
  #abline(h = c(400,700), col = 'black', lty = 2)
  abline(v = c(4.5,12.5,27.5,35.5), h = c(4.08), col = 8, lty = 2)
  #abline(h = c(9), col = 8, lty = 2)
  
  polygon(x = c(X1, rev(X1)), y = c(YLow[1:4], rev(YUp[1:4])), border=NA, col="#A9A9A9ff")
  polygon(x = c(X3, rev(X3)), y = c(YLow[5:12], rev(YUp[5:12])), border=NA, col="#ff82002f")
  polygon(x = c(X5, rev(X5)), y = c(YLow[13:27], rev(YUp[13:27])), border=NA, col="#e516362f")
  polygon(x = c(X7, rev(X7)), y = c(YLow[28:35], rev(YUp[28:35])), border=NA, col="#c400c42f")
  polygon(x = c(X9, rev(X9)), y = c(YLow[36:50], rev(YUp[36:50])), border=NA, col="#005de42f")
  
  lines(X1,Y[1:4], col = alpha("#000000", 1))#aligned
  lines(X3, Y[5:12], col = alpha('#ff8200ff', 1))#rdm0
  lines(X5, Y[13:27], col = alpha("#e51636ff", 1))#rot
  lines(X7, Y[28:35], col = alpha("#c400c4ff", 1))#rdm1
  lines(X9, Y[36:50], col = alpha("#005de4ff", 1))#mir
  
  
  
  #add legend
  legend(5,5.6,legend=c('Aligned','RDM: Early','ROT', 'RDM: Late', 'MIR'),
         col=c("#000000", '#ff8200ff', "#e51636ff", '#c400c4ff', "#005de4ff"),
         lty=1,bty='n',cex=0.8,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Plotting behavioral data----
plotBehaviorLearning <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig34_Behavior_Learning.svg', width=10, height=8, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,3,4,5), 2, 3, byrow = TRUE), widths=c(2.8,2.8,2.8), heights=c(1.5,1))
  
  # # # # # # # # # #
  # panel A: Learning Curves across blocks for ROT and MIR
  plotCollapsedBlockedIndLC()
  mtext('a', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Learning Curves across blocks for RDM
  plotRDMCollapsedBlockedIndLC()
  mtext('b', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel C: RT
  plotBlockedRT()
  mtext('c', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: MT
  plotBlockedMT()
  mtext('d', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel E: PL
  plotBlockedPL()
  mtext('e', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}