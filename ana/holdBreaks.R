source('ana/shared.R')

getParticipantHoldBreaks <- function(id, taskno, task){
  
  data <- getParticipantTaskData(id = id, taskno = taskno, task = task)
  data <- data[which(data$trialselected_bool == 1),]
  trials <- unique(data$trial)
  
  #tasks <- c()
  holdbreaks <- c()
  
  for(trialno in trials){
    subdat <- data[which(data$trial == trialno),]
    if (is.unsorted(subdat$step)){
      holdbreak = 1
    } else{
      holdbreak = 0
    }
    #tasks <- c(tasks, task)
    holdbreaks <- c(holdbreaks, holdbreak)
  }
  
  #participant <- rep(participant, length(trials))
  #ndat <- data.frame(trials, holdbreaks, tasks, participant)
  return(holdbreaks)
  
}

getGroupHoldBreaks <- function(maxppid = 31, tasks = c('aln', 'rdm0', 'rot', 'wash0', 'rdm1', 'mirror', 'wash1')){
  
  for (task in tasks){
    #a consequence of adding the groups late led me to fix it in the manner below
    participants <- seq(0,maxppid,1)
    
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (task == 'aln'){
        mo <- getParticipantHoldBreaks(id = participant, taskno = 1, task = 'aligned')
      } else if (task == 'rdm0'){
        mo <- getParticipantHoldBreaks(id = participant, taskno = 3, task = 'random0')
      } else if (task == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getParticipantHoldBreaks(id = participant, taskno = 11, task = 'rotation')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getParticipantHoldBreaks(id = participant, taskno = 5, task = 'rotation')
        }
      } else if (task == 'wash0'){
        mo <- getParticipantHoldBreaks(id = participant, taskno = 7, task = 'washout0')
      } else if (task == 'rdm1'){
        mo <- getParticipantHoldBreaks(id = participant, taskno = 9, task = 'random1')
      } else if (task == 'mirror'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          mo <- getParticipantHoldBreaks(id = participant, taskno = 5, task = 'mirror')
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          mo <- getParticipantHoldBreaks(id = participant, taskno = 11, task = 'mirror')
        }
      } else if (task == 'wash1'){
        mo <- getParticipantHoldBreaks(id = participant, taskno = 13, task = 'washout1')
      }
      
      trial <- c(1:length(mo))-1 #sets up trial column
      dat <- cbind(trial, mo)
      #rdat <- dat$reaches
      
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- dat
      } else {
        dataoutput <- cbind(dataoutput, mo)
      }
      
    }
    #return(dataoutput)
    write.csv(dataoutput, file=sprintf('data/HoldBreaks_%s.csv', task), row.names = F)
  }
  
}

getTotalHoldBreaks <- function(tasks = c('aln', 'rdm0', 'rot', 'wash0', 'rdm1', 'mirror', 'wash1')){
  
  dataoutput<- data.frame()
  
  for(task in tasks){
    data <- read.csv(file=sprintf('data/HoldBreaks_%s.csv', task))
    subdat <- data[,2:ncol(data)]
    breaks <- sum(subdat == 1)
    nonbreaks <- sum(subdat == 0)
    totaltrials <- breaks + nonbreaks
    percentage <- (breaks/totaltrials)*100
    taskdat <- data.frame(task, breaks, totaltrials, percentage)
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- taskdat
    } else {
      dataoutput <- rbind(dataoutput, taskdat)
    }
  }
  return(dataoutput)
}