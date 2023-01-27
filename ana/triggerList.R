#data read in is from  MirRevAdapt_TriggerList.py
#python script gets all events from eeg data
#here, we want to index trials properly to know which trials we are missing a trigger


#single participant and task level
getMissingTriggers <- function(pp, startmark, endmark = 16156, trigger){
  #read in events file containing all markers for the participant
  data <- read.csv(sprintf('data/eeg/p%03d/mra_p%03d_triggerlist.csv', pp, pp))
  #subset to just markers and their indices
  trigs <- data[c(1,4)]
  colnames(trigs) <- c('idx', 'marker')
  #indicate start of task marker, and task end marker
  taskstartidx <- trigs$idx[which(trigs$marker == startmark)]
  alltaskendidx <- trigs$idx[which(trigs$marker == endmark)]
  #first end marker will be the task end for specific task
  taskendidx <- min(alltaskendidx[which(alltaskendidx > taskstartidx)])
  
  #get markers within task start and end
  tasktrigs <- trigs[which(trigs$idx >= taskstartidx & trigs$idx <= taskendidx),]
  #check if you have correct number of markers within task
  trials <- length(which(tasktrigs$marker == trigger))
  
  #know which index contains missing value
  ttidx <- tasktrigs$idx[which(tasktrigs$marker == trigger)]
  #every marker shows up after 7 other markers
  #this may be distorted a bit, since we have other missing markers,
  #but if we get an idx close to 2x this number (14) then it is obvious the specific marker was skipped for one trial
  trigpres <- 1
  trigabs <- 0
  ntrials <- c()
  for(diff in 1:length(ttidx)){
    ntrials <- c(ntrials, trigpres)
    nxtidx <- diff + 1
    if(nxtidx <= length(ttidx)){
      if(ttidx[nxtidx] - ttidx[diff] >= 14){
        ntrials <- c(ntrials, trigabs)
      }
    }
  }
  
  return(ntrials)
}

#single participant, all tasks
getAllTriggers <- function(maxppid = 31, trigger = 16142, endmark = 16156, tasks = c(16149, 16150, 16151, 16152, 16153, 16154, 16155)){
  
  participants <- seq(0,maxppid,1)

  for (participant in participants){
    trigidx <- c()
    trialno <- c()
    tasktrig<- c()
    for (task in tasks){
      trigs <- getMissingTriggers(pp = participant, startmark = task, endmark = endmark, trigger = trigger)
      trials <- c(1:length(trigs)-1)
      trialtype <- rep(task, length(trigs))
      
      trigidx <- c(trigidx, trigs)
      trialno <- c(trialno, trials)
      tasktrig <- c(tasktrig, trialtype)
    }

    alldat <- data.frame(trialno, trigidx, tasktrig)
    if (trigger == 16142){
      write.csv(alldat, file=sprintf('data/eeg/p%03d/frn/mra_p%03d_tasktrigindex.csv', participant, participant), row.names = F) 
    } else if (trigger == 16140){
      write.csv(alldat, file=sprintf('data/eeg/p%03d/lrp/mra_p%03d_tasktrigindex.csv', participant, participant), row.names = F) 
    }
    
   
  }
  
}







