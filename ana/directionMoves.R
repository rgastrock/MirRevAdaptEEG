source('ana/shared.R')
source('ana/learningRates.R')

#which side of the workspace did they move towards?

# getAnglesWorkspace <- function(id, taskno, task, location){
#   
#   data <- getParticipantTaskData(id = id, taskno = taskno, task = task)
#   
#   if(task == 'aligned' | task == 'random0' | task == 'random1'){
#     AT<- getReachAngles(data, starttrial=0, endtrial=47, location = location)
#   } else if (task == 'rotation' | task == 'mirror'){
#     AT<- getReachAngles(data, starttrial=0, endtrial=89, location = location)
#   }
#   
#   
#   
#   trials <- c(1:length(AT$trial))-1
#   AT$direction <- NA
#   
#   for(trialno in trials){
#     subdat <- AT[which(AT$trial == trialno),]
#     target <- subdat$targetangle
#     
#     #ensure that reachdev is either in the left or right side of the workspace, relative to vertical midline
#     #right targets
#     if(target == 7.5){
#       x <- subdat$reachdev
#       if(x < -97.5 | x > 82.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 15){
#       x <- subdat$reachdev
#       if(x < -105 | x > 75){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 22.5){
#       x <- subdat$reachdev
#       if(x < -112.5 | x > 67.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 67.5){
#       x <- subdat$reachdev
#       if(x < -157.5 | x > 22.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 75){
#       x <- subdat$reachdev
#       if(x < -165 | x > 15){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 82.5){
#       x <- subdat$reachdev
#       if(x < -172.5 | x > 7.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 277.5){
#       x <- subdat$reachdev
#       if(x < -7.5 | x > 172.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 285){
#       x <- subdat$reachdev
#       if(x < -15 | x > 165){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 292.5){
#       x <- subdat$reachdev
#       if(x < -22.5 | x > 157.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 337.5){
#       x <- subdat$reachdev
#       if(x < -67.5 | x > 112.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 345){
#       x <- subdat$reachdev
#       if(x < -75 | x > 105){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 352.5){
#       x <- subdat$reachdev
#       if(x < -82.5 | x > 97.5){
#         dir <- 'l'
#       } else {
#         dir <- 'r'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 97.5){#left targets
#       x <- subdat$reachdev
#       if(x < -7.5 | x > 172.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 105){
#       x <- subdat$reachdev
#       if(x < -15 | x > 165){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 112.5){
#       x <- subdat$reachdev
#       if(x < -22.5 | x > 157.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 157.5){
#       x <- subdat$reachdev
#       if(x < -67.5 | x > 112.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 165){
#       x <- subdat$reachdev
#       if(x < -75 | x > 105){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 172.5){
#       x <- subdat$reachdev
#       if(x < -82.5 | x > 97.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 187.5){
#       x <- subdat$reachdev
#       if(x < -97.5 | x > 82.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 195){
#       x <- subdat$reachdev
#       if(x < -105 | x > 75){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 202.5){
#       x <- subdat$reachdev
#       if(x < -112.5 | x > 67.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 247.5){
#       x <- subdat$reachdev
#       if(x < -157.5 | x > 22.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 255){
#       x <- subdat$reachdev
#       if(x < -165 | x > 15){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } else if(target == 262.5){
#       x <- subdat$reachdev
#       if(x < -172.5 | x > 7.5){
#         dir <- 'r'
#       } else {
#         dir <- 'l'
#       }
#       subdat$direction <- dir
#       AT[which(AT$trial == trialno),] <- subdat
#     } 
#   }
#   
#   return(AT)
# }

# movement workspace direction: targets on HORIZONTAL axis----

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
    } else {
      dir <- NA
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    }
  }
  
  return(AT)
}

getGroupMovementWorkspaceDirection <- function(maxppid = 31, groups = c('aln', 'rot', 'rdmrot', 'rdmmir', 'mir'), location = 'feedback') {
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
      } else if (group == 'rdmrot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesWorkspace(id = participant, taskno = 9, task = 'random1', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesWorkspace(id = participant, taskno = 3, task = 'random0', location = location)
        }
      } else if (group == 'rdmmir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesWorkspace(id = participant, taskno = 3, task = 'random0', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesWorkspace(id = participant, taskno = 9, task = 'random1', location = location)
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

getRDMMovementWorkspaceDirection <- function(){
  
  rdmrot <- read.csv(file='data/rdmrot_MovementWorkspace_direction.csv')
  rdmmir <- read.csv(file='data/rdmmir_MovementWorkspace_direction.csv')
  
  #assign rdmmir values to NA values in rdmrot
  rdmrot[is.na(rdmrot)] <- rdmmir[is.na(rdmrot)]
  
  write.csv(rdmrot, file='data/rdm_MovementWorkspace_direction.csv', row.names = F) 
  
}

# movement workspace direction: targets on VERTICAL axis----
getAnglesVerticalWorkspace <- function(id, taskno, task, location){
  
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
    if(target == 67.5){
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
    } else if(target == 97.5){ #left targets
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
    } else {
      dir <- NA
      subdat$direction <- dir
      AT[which(AT$trial == trialno),] <- subdat
    }
  }
  
  return(AT)
}

getGroupMovementVerticalWorkspaceDirection <- function(maxppid = 31, groups = c('aln', 'rot', 'rdmrot', 'rdmmir', 'mir'), location = 'feedback') {
  #participants <- getGroupParticipants(group) #the function that gives all participant ID's for a specified group
  
  for(group in groups){
    participants <- seq(0,maxppid,1)
    dataoutput<- data.frame() #create place holder
    #go through each participant in this group
    for (participant in participants) {
      #print(participant)
      if (group == 'aln'){
        ppangles <- getAnglesVerticalWorkspace(id=participant, taskno = 1, task = 'aligned', location = location)
      } else if (group == 'rot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 11, task = 'rotation', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 5, task = 'rotation', location = location)
        }
      } else if (group == 'rdmrot'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 9, task = 'random1', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 3, task = 'random0', location = location)
        }
      } else if (group == 'rdmmir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 3, task = 'random0', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 9, task = 'random1', location = location)
        }
      } else if (group == 'mir'){
        if (participant%%2 == 1){
          #mirror then rotation if odd id
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 5, task = 'mirror', location = location)
        } else if (participant%%2 == 0){
          #if pp id is even
          #rotation first then mirror
          ppangles <- getAnglesVerticalWorkspace(id = participant, taskno = 11, task = 'mirror', location = location)
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
    write.csv(dataoutput, file=sprintf('data/%s_MovementVerticalWorkspace_direction.csv', group), row.names = F) 
  }
  
}

getRDMMovementVerticalWorkspaceDirection <- function(){
  
  rdmrot <- read.csv(file='data/rdmrot_MovementVerticalWorkspace_direction.csv')
  rdmmir <- read.csv(file='data/rdmmir_MovementVerticalWorkspace_direction.csv')
  
  #assign rdmmir values to NA values in rdmrot
  rdmrot[is.na(rdmrot)] <- rdmmir[is.na(rdmrot)]
  
  write.csv(rdmrot, file='data/rdm_MovementVerticalWorkspace_direction.csv', row.names = F) 
  
}

# plot LRP differences for right and left moves on HORIZONTAL axis ----
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

#, 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'
plotWorkspaceLRPs <- function(groups = c('aln_right', 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6_Workspace_LRP.svg', width=14, height=18, pointsize=14, system_fonts=list(sans="Arial"))
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
      timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s_%s.csv', group, erps, channel))
      groupconfidence <- groupconfidence[101:701,] #grab timepts we need
      
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

# plot LRP differences for right and left moves on VERTICAL axis ----
getVerticalWorkspaceLRPConfidenceInterval <- function(groups = c('aln_right', 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'), type = 'b', erps = 'lrp', channels = c('C3','C4')){
  for(channel in channels){
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_vertical_%s_%s_%s.csv', group, erps, channel))
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
        
        write.csv(confidence, file=sprintf('data/ERP_CI_vertical_%s_%s_%s.csv', group, erps, channel), row.names = F) 
        
      }
    }
  }
}

#, 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'
plotVerticalWorkspaceLRPs <- function(groups = c('aln_right', 'aln_left', 'rot_right', 'rot_left', 'rdm_right', 'rdm_left', 'mir_right', 'mir_left'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig6A_Vertical_Workspace_LRP.svg', width=14, height=18, pointsize=14, system_fonts=list(sans="Arial"))
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
      data <- read.csv(file=sprintf('data/Evoked_DF_vertical_%s_%s_%s.csv', group, erps, channel))
      timepts <- data$time
      timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ERP_CI_vertical_%s_%s_%s.csv', group, erps, channel))
      groupconfidence <- groupconfidence[101:701,] #grab timepts we need
      
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

#Calculate initial error categories LRPs-----
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
    timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
    
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/Subtracted_LRP_CI_%s.csv', group))
    groupconfidence <- groupconfidence[101:701,] #grab timepts we need
    
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



#CALCULATE INITIAL CATEGORIES LRPs ACROSS LEARNING BLOCKS-----
# Right(C3-C4) - Left(C3-C4)
getBLockedLRP <- function(groups = c('aln', 'rot', 'rdm', 'mir'), directions = c('right', 'left')){
  
  for(group in groups){
    #separate condition for aligned
    if(group == 'aln'){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_C3.csv', group, direction))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_C4.csv', group, direction))
        
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
      
      write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_%s.csv', group), row.names = F) 
    } else if(group == 'rot' | group == 'mir'){
      blocks <- c(0:3)
      for(blockno in blocks){
        for(direction in directions){
          C3data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_b%d_C3.csv', group, direction, blockno))
          C4data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_b%d_C4.csv', group, direction, blockno))
          
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
        write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_%s_b%d.csv', group, blockno), row.names = F) 
        
      }
    } else if(group == 'rdm'){
      blocks <- c(0)
      for(blockno in blocks){
        for(direction in directions){
          C3data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_b%d_C3.csv', group, direction, blockno))
          C4data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s_b%d_C4.csv', group, direction, blockno))
          
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
        write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_%s_b%d.csv', group, blockno), row.names = F) 
        
      }
    }
    
    
  }
}

#then we want to get the average LRP (uV) for every participant, around 300 ms to 0 s
getAverageBLockedLRP <- function(group){
  
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s.csv', group))
    #subset for rows from -300 ms to 0
    startidx <- data$rowidx[which(data$timepts == -0.30)]
    endidx <- (startidx + 60) - 1
    idxs <- seq(startidx, endidx, 1)
    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$rowidx == i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    ndatavgs <- ndatavgs * -1
    
  } else if(group == 'rot' | group == 'mir'){
    blocks <- c(0:3)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/BLocked_LRP_DF_%s_b%d.csv', group, blockno))
      #subset for rows from -300 ms to 0
      startidx <- data$rowidx[which(data$timepts == -0.30)]
      endidx <- (startidx + 60) - 1
      idxs <- seq(startidx, endidx, 1)
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$rowidx == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      ndatavg <- ndatavg * -1
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } else if(group == 'rdm'){
    blocks <- c(0)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/BLocked_LRP_DF_%s_b%d.csv', group, blockno))
      #subset for rows from -300 ms to 0
      startidx <- data$rowidx[which(data$timepts == -0.30)]
      endidx <- (startidx + 60) - 1
      idxs <- seq(startidx, endidx, 1)
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$rowidx == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      ndatavg <- ndatavg * -1
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
  }
  
  return(ndatavgs)
  
}

getAverageBlockedLRPConfidenceInterval <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  
  for (group in groups){
    
    data <- getAverageBLockedLRP(group=group)
    if(group == 'aln' | group == 'rdm'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/Blocked_LRP_CI_%s.csv', group), row.names = F) 
    } else if (group == 'rot' | group == 'mir'){
      data <- as.data.frame(data)
      blocks <- c(1:nrow(data))
      data1 <- as.matrix(data)
      
      confidence <- data.frame()
      
      
      for (blockno in blocks){
        cireaches <- as.numeric(data1[blockno,])
        
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
        
        write.csv(confidence, file=sprintf('data/Blocked_LRP_CI_%s.csv', group), row.names = F) 
        
      }
    }
  }
}

plotBlockedLRPs <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9_Blocked_LRP.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 7), ylim = c(-6, 16), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Lateralized Readiness Potentials across learning", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5, 4.5, 5.5), labels=c('pre-training', 'block 1', 'block 2', 'block 3', 'block 4'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-5, 0, 5, 10, 15), labels=c('5','0','-5','-10','-15'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_CI_%s.csv', group))
    if (group == 'aln' | group =='rdm'){
      data <- t(data)
      colourscheme <- getSubtractedLRPColourScheme(groups=group)
      #take only first, last and middle columns of file
      lower <- data[,1]
      upper <- data[,3]
      mid <- data[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      if(group == 'aln'){
        blockX <- 1.45
      } else if (group == 'rdm'){
        blockX <- 1.55
      }
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
      
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      #lines(x = timepts, y = mid, col=col)
      points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      
      
    } else if (group == 'rot' | group == 'mir'){
      for (blockno in 1:nrow(data)){
        subdat <- data[blockno,]
        colourscheme <- getSubtractedLRPColourScheme(groups=group)
        #take only first, last and middle columns of file
        lower <- subdat[,1]
        upper <- subdat[,3]
        mid <- subdat[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        if(group == 'rot'){
          blockX <- blockno + 1.45
        } else if (group == 'mir'){
          blockX <- blockno + 1.55
        } 
       
        lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
        
        # plot mean reaches for each group
        col <- colourscheme[[group]][['S']]
        #lines(x = timepts, y = mid, col=col)
        points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      }
      

    }
  }
  
  #add legend
  legend(1,15,legend=c('aligned', 'rotation', 'random', 'mirror'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getAverageBlockedLRPComparisons <- function(){
  
  aln <- getAverageBLockedLRP(group='aln')
  rdm <- getAverageBLockedLRP(group='rdm')
  rot <- getAverageBLockedLRP(group='rot')
  mir <- getAverageBLockedLRP(group='mir')
  
  cat('t-test (Aligned vs. Random): \n')
  print(t.test(aln, rdm))
  
  cat('t-test (Aligned vs. Rotation Block 1): \n')
  print(t.test(aln, rot[1,]))
  
  cat('t-test (Aligned vs. Mirror Block 1): \n')
  print(t.test(aln, mir[1,]))
  
  cat('t-test (Rotation Block 1 vs. Mirror Block 1): \n')
  print(t.test(rot[1,], mir[1,]))
  
}

#CALCULATE EARLY/ LATE LRPs-----
# Right(C3-C4) - Left(C3-C4)
getEarlyLateLRP <- function(groups = c('aln', 'alnrot', 'alnmir', 'rot', 'rdm', 'mir'), directions = c('right', 'left')){
  
  for(group in groups){
    #separate condition for aligned
    if(group == 'aln'){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_C3.csv', group, direction))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_C4.csv', group, direction))
        
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
      
      write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group), row.names = F) 
    } else if (group == 'alnrot' | group == 'alnmir'){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_C3.csv', group, direction))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_C4.csv', group, direction))
        
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
      
      write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group), row.names = F) 
    } else if(group == 'rot' | group == 'mir'){
      blocks <- c(0:1)
      for(blockno in blocks){
        for(direction in directions){
          C3data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_b%d_C3.csv', group, direction, blockno))
          C4data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_b%d_C4.csv', group, direction, blockno))
          
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
        write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s_b%d.csv', group, blockno), row.names = F) 
        
      }
    } else if(group == 'rdm'){
      blocks <- c(0:1)
      for(blockno in blocks){
        for(direction in directions){
          C3data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_b%d_C3.csv', group, direction, blockno))
          C4data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s_b%d_C4.csv', group, direction, blockno))
          
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
        write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s_b%d.csv', group, blockno), row.names = F) 
        
      }
    }
    
    
  }
}

getEarlyLateLateralizedCI <- function(groups = c('aln', 'alnrot', 'alnmir', 'rot_b0', 'rot_b1', 'rdm_b0', 'rdm_b1', 'mir_b0', 'mir_b1'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
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
      
      write.csv(confidence, file=sprintf('data/EarlyLate_LRP_CI_%s.csv', group), row.names = F) 
      
    }
  }
}

plotEarlyLateLateralized <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8A_EarlyLate_Subtracted_LRP_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('alnrot', 'rot_b0', 'rot_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 0.10), ylim = c(-16, 6), 
            xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
            main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:401,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      # mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      # mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      #   
      # col <- colourscheme[['aligned']][['T']]
      # lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['aligned']][['S']]
      # points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      #   
      # col <- colourscheme[['late']][['T']]
      # lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['late']][['S']]
      # points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early ROT', 'Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      groups = c('aln', 'rdm_b0', 'rdm_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 0.10), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:401,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      # mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      # mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      # 
      # col <- colourscheme[['aligned']][['T']]
      # lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['aligned']][['S']]
      # points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      # 
      # col <- colourscheme[['late']][['T']]
      # lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['late']][['S']]
      # points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early RDM', 'Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('alnmir', 'mir_b0', 'mir_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 0.10), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:401,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
          err <- 'late'
        } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      # mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      # mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      # 
      # col <- colourscheme[['aligned']][['T']]
      # lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['aligned']][['S']]
      # points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      # 
      # col <- colourscheme[['late']][['T']]
      # lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      # col <- colourscheme[['late']][['S']]
      # points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early MIR', 'Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#need difference waves between each condition and aligned, as well as early vs rot
getEarlyLatevsALignedLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    if (ptype == 'rot'){
      groups = c('rot_b0', 'rot_b1')
      adata <- read.csv(file='data/Blocked_LRP_DF_EarlyLate_alnrot.csv')
    } else if (ptype == 'mir'){
      groups = c('mir_b0', 'mir_b1')
      adata <- read.csv(file='data/Blocked_LRP_DF_EarlyLate_alnmir.csv')
    } else if (ptype == 'rdm'){
      groups = c('rdm_b0', 'rdm_b1')
      adata <- read.csv(file='data/Blocked_LRP_DF_EarlyLate_aln.csv')
    }
    
    for (group in groups){
      pdata <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
      row_idx <- adata$rowidx
      timepts <- adata$timepts
      n <- length(adata) - 1
      diffdata <- pdata[,2:n] - adata[,2:n]
      diffdata$idx <- row_idx
      diffdata$timepts <- timepts
      write.csv(diffdata, file=sprintf('data/Blocked_LRP_DF_%s_vsAligned.csv', group), row.names = F) 
    }
  }
}

getEarlyLatevsALignedLRPDiffWavesCI <- function(groups = c('rot_b0', 'rot_b1', 'rdm_b0', 'rdm_b1', 'mir_b0', 'mir_b1'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s_vsAligned.csv', group))
    timepts <- data$timepts
    n <- length(data) - 2 #remove idx and timepts cols
    data <- data[,1:n]
    data$time <- timepts
    
    data <- as.data.frame(data)
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
      
      write.csv(confidence, file=sprintf('data/Blocked_LRP_DF_%s_vsAligned_CI.csv', group), row.names = F) 
      
    }
  }
}

#need difference waves between each condition and aligned, as well as early vs rot
getEarlyvsLatePTypeLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    
    earlydat <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s_b0_vsALigned.csv', ptype))
    latedat <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s_b1_vsALigned.csv', ptype))
    
    row_idx <- earlydat$idx
    timepts <- earlydat$timepts
    n <- length(earlydat) - 2
    diffdata <- latedat[,1:n] - earlydat[,1:n]
    diffdata$idx <- row_idx
    diffdata$timepts <- timepts
    write.csv(diffdata, file=sprintf('data/Blocked_LRP_DF_EarlyvsLate_%s.csv', ptype), row.names = F) 
    
  }
}

getEarlyvsLatePTypeLRPDiffWavesCI <- function(perturbs = c('rot', 'rdm', 'mir'), type = 'b'){
  for (ptype in perturbs){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyvsLate_%s.csv', ptype))
    timepts <- data$timepts
    n <- length(data) - 2 #remove idx and timepts cols
    data <- data[,1:n]
    data$time <- timepts
    
    data <- as.data.frame(data)
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
      
      write.csv(confidence, file=sprintf('data/Blocked_LRP_DF_EarlyvsLate_%s_CI.csv', ptype), row.names = F) 
      
    }
  }
}

#then we want to get the average LRP (uV) for every participant, around 300 ms to 0 s
getAverageEarlyLateLRP <- function(group){
  
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
    #subset for rows from -300 ms to 0
    startidx <- data$rowidx[which(data$timepts == -0.30)]
    endidx <- (startidx + 60)
    idxs <- seq(startidx, endidx, 1)
    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$rowidx == i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    ndatavgs <- ndatavgs * -1
    
  } else if(group == 'rot' | group == 'mir'){
    blocks <- c(0:1)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s_b%d.csv', group, blockno))
      #subset for rows from -300 ms to 0
      startidx <- data$rowidx[which(data$timepts == -0.30)]
      endidx <- (startidx + 60)
      idxs <- seq(startidx, endidx, 1)
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$rowidx == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      ndatavg <- ndatavg * -1
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } else if(group == 'rdm'){
    blocks <- c(0:1)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s_b%d.csv', group, blockno))
      #subset for rows from -300 ms to 0
      startidx <- data$rowidx[which(data$timepts == -0.30)]
      endidx <- (startidx + 60)
      idxs <- seq(startidx, endidx, 1)
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$rowidx == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      ndatavg <- ndatavg * -1
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
  }
  
  return(ndatavgs)
  
}

getAverageEarlyLateLRPCI <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  
  for (group in groups){
    
    data <- getAverageEarlyLateLRP(group=group)
    if(group == 'aln'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/EarlyLate_Average_LRP_CI_%s.csv', group), row.names = F) 
    } else if (group == 'rot' | group == 'mir' | group == 'rdm'){
      data <- as.data.frame(data)
      blocks <- c(1:nrow(data))
      data1 <- as.matrix(data)
      
      confidence <- data.frame()
      
      
      for (blockno in blocks){
        cireaches <- as.numeric(data1[blockno,])
        
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
        
        write.csv(confidence, file=sprintf('data/EarlyLate_Average_LRP_CI_%s.csv', group), row.names = F) 
        
      }
    }
  }
}

plotAverageEarlyLateLRPs <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9A_Average_EarlyLate_LRP.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 5), ylim = c(-6, 16), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Lateralized Readiness Potentials across learning", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5), labels=c('pre-training', 'early', 'late'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-5, 0, 5, 10, 15), labels=c('5','0','-5','-10','-15'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/EarlyLate_Average_LRP_CI_%s.csv', group))
    if (group == 'aln'){
      data <- t(data)
      colourscheme <- getSubtractedLRPColourScheme(groups=group)
      #take only first, last and middle columns of file
      lower <- data[,1]
      upper <- data[,3]
      mid <- data[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      blockX <- 1.5
      
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
      
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      #lines(x = timepts, y = mid, col=col)
      points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      
      
    } else if (group == 'rot' | group == 'mir' | group == 'rdm'){
      for (blockno in 1:nrow(data)){
        subdat <- data[blockno,]
        colourscheme <- getSubtractedLRPColourScheme(groups=group)
        #take only first, last and middle columns of file
        lower <- subdat[,1]
        upper <- subdat[,3]
        mid <- subdat[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        if(group == 'rdm'){
          blockX <- blockno + 1.40
        } else if (group == 'rot'){
          blockX <- blockno + 1.50
        } else if (group == 'mir'){
          blockX <- blockno + 1.60
        } 
        
        lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
        
        # plot mean reaches for each group
        col <- colourscheme[[group]][['S']]
        #lines(x = timepts, y = mid, col=col)
        points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      }
      
      
    }
  }
  
  #add legend
  legend(1,15,legend=c('aligned', 'rotation', 'random', 'mirror'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getAverageEarlyLateLRPComparisons <- function(){
  
  aln <- getAverageEarlyLateLRP(group='aln')
  rdm <- getAverageEarlyLateLRP(group='rdm')
  rot <- getAverageEarlyLateLRP(group='rot')
  mir <- getAverageEarlyLateLRP(group='mir')
  
  cat('t-test (Aligned vs. Random): \n')
  print(t.test(aln, rdm[1,]))
  
  cat('t-test (Aligned vs. Rotation Block 1): \n')
  print(t.test(aln, rot[1,]))
  
  cat('t-test (Aligned vs. Mirror Block 1): \n')
  print(t.test(aln, mir[1,]))
  
  cat('t-test (Rotation Block 1 vs. Mirror Block 1): \n')
  print(t.test(rot[1,], mir[1,]))
  
}


#CALCULATE SMALL/ LARGE LRPs-----
# Right(C3-C4) - Left(C3-C4)
getSmallLargeLRP <- function(groups = c('aln', 'alnrot', 'alnmir', 'rot', 'rdm', 'mir'), directions = c('right', 'left')){
  
  for(group in groups){
    #separate condition for aligned
    if(group == 'aln'){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_C3.csv', group, direction))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_C4.csv', group, direction))
        
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
      
      write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group), row.names = F)
    } else if (group == 'alnrot' | group == 'alnmir'){
      for(direction in directions){
        
        C3data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_C3.csv', group, direction))
        C4data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_C4.csv', group, direction))
        
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
      
      write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group), row.names = F)
    } else if(group == 'rot' | group == 'mir' | group == 'rdm'){
      errsizes <- c('sml', 'lrg')
      for(s in errsizes){
        for(direction in directions){
          C3data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_%s_C3.csv', group, direction, s))
          C4data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_%s_C4.csv', group, direction, s))
          
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
        write.csv(groupLRP, file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_%s.csv', group, s), row.names = F) 
        
      }
    } 
  }
}

getSmallLargeLateralizedCI <- function(groups = c('aln', 'alnrot', 'alnmir', 'rot_sml', 'rot_lrg', 'rdm_sml', 'rdm_lrg', 'mir_sml', 'mir_lrg'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
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
      
      write.csv(confidence, file=sprintf('data/SmallLarge_LRP_CI_%s.csv', group), row.names = F) 
      
    }
  }
}

plotSmallLargeLateralized <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8B_SmallLarge_Subtracted_LRP_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'rot_sml', 'rot_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:701,] #grab timepts we need
        
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getErrSizeColourScheme(err = err)
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
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small ROT', 'Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      groups = c('aln', 'rdm_sml', 'rdm_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:701,] #grab timepts we need
        
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getErrSizeColourScheme(err = err)
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
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small RDM', 'Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'mir_sml', 'mir_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
        timepts <- data$time
        timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_LRP_CI_%s.csv', group))
        groupconfidence <- groupconfidence[101:701,] #grab timepts we need
        
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        
        
        colourscheme <- getErrSizeColourScheme(err = err)
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
        if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
          err <- 'sml'
        } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
          err <- 'lrg'
        } else if (group == 'aln'){
          err <- 'aligned'
        }
        # plot mean reaches for each group
        col <- colourscheme[[err]][['S']]
        #lines(x = timepts, y = mid, col=col)
        lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
      }
      
      #add movement onset 
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small MIR', 'Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#need difference waves between each condition and aligned
getSmallLargevsALignedLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    if (ptype == 'rot'){
      groups = c('rot_sml', 'rot_lrg')
      adata <- read.csv(file='data/Blocked_LRP_DF_SmallLarge_alnrot.csv')
    } else if (ptype == 'mir'){
      groups = c('mir_sml', 'mir_lrg')
      adata <- read.csv(file='data/Blocked_LRP_DF_SmallLarge_alnmir.csv')
    } else if (ptype == 'rdm'){
      groups = c('rdm_sml', 'rdm_lrg')
      adata <- read.csv(file='data/Blocked_LRP_DF_SmallLarge_aln.csv')
    }
    
    for (group in groups){
      pdata <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
      row_idx <- adata$rowidx
      timepts <- adata$timepts
      n <- length(adata) - 1
      diffdata <- pdata[,2:n] - adata[,2:n]
      diffdata$idx <- row_idx
      diffdata$timepts <- timepts
      write.csv(diffdata, file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_vsAligned.csv', group), row.names = F) 
    }
  }
}

getSmallLargevsALignedLRPDiffWavesCI <- function(groups = c('rot_sml', 'rot_lrg', 'rdm_sml', 'rdm_lrg', 'mir_sml', 'mir_lrg'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_vsAligned.csv', group))
    timepts <- data$timepts
    n <- length(data) - 2 #remove idx and timepts cols
    data <- data[,1:n]
    data$time <- timepts
    
    data <- as.data.frame(data)
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
      
      write.csv(confidence, file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_vsAligned_CI.csv', group), row.names = F) 
      
    }
  }
}

#need difference waves between each condition and aligned, as well as early vs rot
getSmallvsLargePTypeLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    
    smalldat <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_sml_vsALigned.csv', ptype))
    largedat <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_lrg_vsALigned.csv', ptype))
    
    row_idx <- smalldat$idx
    timepts <- smalldat$timepts
    n <- length(smalldat) - 2
    diffdata <- largedat[,1:n] - smalldat[,1:n]
    diffdata$idx <- row_idx
    diffdata$timepts <- timepts
    write.csv(diffdata, file=sprintf('data/Blocked_LRP_DF_SmallvsLarge_%s.csv', ptype), row.names = F) 
    
  }
}

getSmallvsLargePTypeLRPDiffWavesCI <- function(perturbs = c('rot', 'rdm', 'mir'), type = 'b'){
  for (ptype in perturbs){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallvsLarge_%s.csv', ptype))
    timepts <- data$timepts
    n <- length(data) - 2 #remove idx and timepts cols
    data <- data[,1:n]
    data$time <- timepts
    
    data <- as.data.frame(data)
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
      
      write.csv(confidence, file=sprintf('data/Blocked_LRP_DF_SmallvsLarge_%s_CI.csv', ptype), row.names = F) 
      
    }
  }
}


#then we want to get the average LRP (uV) for every participant, around 300 ms to 0 s
getAverageSmallLargeLRP <- function(group){
  
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
    #subset for rows from -300 ms to 0
    startidx <- data$rowidx[which(data$timepts == -0.30)]
    endidx <- (startidx + 60)
    idxs <- seq(startidx, endidx, 1)
    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$rowidx == i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    ndatavgs <- ndatavgs * -1
    
  } else if(group == 'rot' | group == 'mir' | group == 'rdm'){
    errsizes <- c('sml', 'lrg')
    ndatavgs <- data.frame()
    for(s in errsizes){
      
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_%s.csv', group, s))
      #subset for rows from -300 ms to 0
      startidx <- data$rowidx[which(data$timepts == -0.30)]
      endidx <- (startidx + 60)
      idxs <- seq(startidx, endidx, 1)
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$rowidx == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      ndatavg <- ndatavg * -1
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } 
  
  return(ndatavgs)
  
}

getAverageSmallLargeLRPCI <- function(groups = c('aln', 'rot', 'rdm', 'mir'), type = 'b'){
  
  for (group in groups){
    
    data <- getAverageSmallLargeLRP(group=group)
    if(group == 'aln'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/SmallLarge_Average_LRP_CI_%s.csv', group), row.names = F) 
    } else if (group == 'rot' | group == 'mir' | group == 'rdm'){
      data <- as.data.frame(data)
      errsizes <- c(1:nrow(data))
      data1 <- as.matrix(data)
      
      confidence <- data.frame()
      
      
      for (s in errsizes){
        cireaches <- as.numeric(data1[s,])
        
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
        
        write.csv(confidence, file=sprintf('data/SmallLarge_Average_LRP_CI_%s.csv', group), row.names = F) 
        
      }
    }
  }
}

plotAverageSmallLargeLRPs <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig9B_Average_SmallLarge_LRP.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 5), ylim = c(-6, 16), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Lateralized Readiness Potentials across learning", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5), labels=c('pre-training', 'small', 'large'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-5, 0, 5, 10, 15), labels=c('5','0','-5','-10','-15'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/SmallLarge_Average_LRP_CI_%s.csv', group))
    if (group == 'aln'){
      data <- t(data)
      colourscheme <- getSubtractedLRPColourScheme(groups=group)
      #take only first, last and middle columns of file
      lower <- data[,1]
      upper <- data[,3]
      mid <- data[,2]
      
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      blockX <- 1.5
      
      
      #upper and lower bounds create a polygon
      #polygon creates it from low left to low right, then up right to up left -> use rev
      #x is just trial nnumber, y depends on values of bounds
      lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
      
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      #lines(x = timepts, y = mid, col=col)
      points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      
      
    } else if (group == 'rot' | group == 'mir' | group == 'rdm'){
      for (errsize in 1:nrow(data)){
        subdat <- data[errsize,]
        colourscheme <- getSubtractedLRPColourScheme(groups=group)
        #take only first, last and middle columns of file
        lower <- subdat[,1]
        upper <- subdat[,3]
        mid <- subdat[,2]
        
        col <- colourscheme[[group]][['T']] #use colour scheme according to group
        
        #upper and lower bounds create a polygon
        #polygon creates it from low left to low right, then up right to up left -> use rev
        #x is just trial nnumber, y depends on values of bounds
        if(group == 'rdm'){
          blockX <- errsize + 1.40
        } else if (group == 'rot'){
          blockX <- errsize + 1.50
        } else if (group == 'mir'){
          blockX <- errsize + 1.60
        } 
        
        lines(x = rep(blockX,2), y = c(lower, upper), col = col, lty = 1, lwd = 10)
        
        # plot mean reaches for each group
        col <- colourscheme[[group]][['S']]
        #lines(x = timepts, y = mid, col=col)
        points(x= blockX ,y= mid, pch=16, cex=1.5, col=col)
      }
      
      
    }
  }
  
  #add legend
  legend(1,15,legend=c('aligned', 'rotation', 'random', 'mirror'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

getAverageSmallLargeLRPComparisons <- function(){
  
  aln <- getAverageSmallLargeLRP(group='aln')
  rdm <- getAverageSmallLargeLRP(group='rdm')
  rot <- getAverageSmallLargeLRP(group='rot')
  mir <- getAverageSmallLargeLRP(group='mir')
  
  cat('t-test (Aligned vs. Random Small): \n')
  print(t.test(aln, rdm[1,]))
  
  cat('t-test (Aligned vs. Rotation Small): \n')
  print(t.test(aln, rot[1,]))
  
  cat('t-test (Aligned vs. Mirror Small): \n')
  print(t.test(aln, mir[1,]))
  
  cat('t-test (Rotation Small vs. Mirror Small): \n')
  print(t.test(rot[1,], mir[1,]))
  
}
