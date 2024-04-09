source('ana/shared.R')
source('ana/learningRates.R')

getROTErrorSizeIndices <- function(){
  
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  pdata <- as.data.frame(pdata)
  ptrialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
  ndat <- data.frame()
  for(pp in c(1:ncol(data2))){
    
    subdat2 <- data2[,pp]
    errs2 <- abs(subdat2)
    errs2 <- abs(errs2 - 30)
    #errs2mean <- mean(errs2, na.rm=T)
    #errs2sigma <- sd(errs2, na.rm=T)
    suberrs2 <- sort(errs2)
    #get lower and upper 40% of trials (36 trials each), disregard middle 20% or 18 trials
    ntrials <- 36
    sml <- head(suberrs2, ntrials)
    lrg <- tail(suberrs2, ntrials)
    
    errsize = c()
    for(i in errs2){
      if(i %in% sml){
        size <- 'sml'
      } else if (i %in% lrg){
        size <- 'lrg'
      } else{
        size <- NA
      }
      errsize <- c(errsize, size)
    }
    
    if (prod(dim(ndat)) == 0){
      ndat <- errsize
    } else {
      ndat <- cbind(ndat, errsize)
    }
  }
  ndat <- as.data.frame(ndat)
  df <- cbind(ptrialno, ndat)
  
  #return(df)
  write.csv(df, file='data/rot_ErrorSize_index.csv', row.names = F) 
}

getMIRErrorSizeIndices <- function(angles = c(15,30,45)){
  
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    ndat <- data.frame()
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      #get lower and upper 40% of trials (12 trials each), disregard middle 20% or 6 trials
      ntrials <- 12
      sml <- head(suberrs2, ntrials)
      lrg <- tail(suberrs2, ntrials)
      
      errsize = c()
      for(i in errs2){
        if(i %in% sml){
          size <- 'sml'
        } else if (i %in% lrg){
          size <- 'lrg'
        } else{
          size <- NA
        }
        errsize <- c(errsize, size)
      }
      
      if (prod(dim(ndat)) == 0){
        ndat <- errsize
      } else {
        ndat <- cbind(ndat, errsize)
      }
    }
    
    if(angle == 15){
      dat15 <- ndat
    } else if (angle == 30){
      dat30 <- ndat
    } else if (angle == 45){
      dat45 <- ndat
    }
  }
  
  dat15 <- as.data.frame(dat15)
  dat30 <- as.data.frame(dat30)
  dat45 <- as.data.frame(dat45)
  
  alldat <- data.frame()
  for(pp in c(1:ncol(data2))){
    df <- coalesce(dat15[,pp], dat30[,pp])
    df <- coalesce(df, dat45[,pp])
    
    if (prod(dim(alldat)) == 0){
      alldat <- df
    } else {
      alldat <- cbind(alldat, df)
    }
  }
  
  alldat <- as.data.frame(alldat)
  alldata <- cbind(ptrialno, alldat)
  
  #return(alldat)
  write.csv(alldata, file='data/mir_ErrorSize_index.csv', row.names = F) 
}

getRDMROTErrorSizeIndices <- function(angles = c(15,25,35)){
  
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    ndat <- data.frame()
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      #nondivisible number of trials (16), but get lower and upper ~44% of trials (~7 trials each), disregard middle ~12% or ~2 trials
      ntrials <- 7
      sml <- head(suberrs2, ntrials)
      lrg <- tail(suberrs2, ntrials)
      
      errsize = c()
      for(i in errs2){
        if(i %in% sml){
          size <- 'sml'
        } else if (i %in% lrg){
          size <- 'lrg'
        } else{
          size <- NA
        }
        errsize <- c(errsize, size)
      }
      
      if (prod(dim(ndat)) == 0){
        ndat <- errsize
      } else {
        ndat <- cbind(ndat, errsize)
      }
    }
    
    if(angle == 15){
      dat15 <- ndat
    } else if (angle == 25){
      dat25 <- ndat
    } else if (angle == 35){
      dat35 <- ndat
    }
  }
  
  dat15 <- as.data.frame(dat15)
  dat25 <- as.data.frame(dat25)
  dat35 <- as.data.frame(dat35)
  
  alldat <- data.frame()
  for(pp in c(1:ncol(data2))){
    df <- coalesce(dat15[,pp], dat25[,pp])
    df <- coalesce(df, dat35[,pp])
    
    if (prod(dim(alldat)) == 0){
      alldat <- df
    } else {
      alldat <- cbind(alldat, df)
    }
  }
  
  alldat <- as.data.frame(alldat)
  alldata <- cbind(ptrialno, alldat)
  
  #return(alldat)
  write.csv(alldata, file='data/rdmrot_ErrorSize_index.csv', row.names = F) 
}

getRDMMIRErrorSizeIndices <- function(angles = c(15,25,35)){
  
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    ndat <- data.frame()
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      #nondivisible number of trials (16), but get lower and upper ~44% of trials (~7 trials each), disregard middle ~12% or ~2 trials
      ntrials <- 7
      sml <- head(suberrs2, ntrials)
      lrg <- tail(suberrs2, ntrials)
      
      errsize = c()
      for(i in errs2){
        if(i %in% sml){
          size <- 'sml'
        } else if (i %in% lrg){
          size <- 'lrg'
        } else{
          size <- NA
        }
        errsize <- c(errsize, size)
      }
      
      if (prod(dim(ndat)) == 0){
        ndat <- errsize
      } else {
        ndat <- cbind(ndat, errsize)
      }
    }
    
    if(angle == 15){
      dat15 <- ndat
    } else if (angle == 25){
      dat25 <- ndat
    } else if (angle == 35){
      dat35 <- ndat
    }
  }
  
  dat15 <- as.data.frame(dat15)
  dat25 <- as.data.frame(dat25)
  dat35 <- as.data.frame(dat35)
  
  alldat <- data.frame()
  for(pp in c(1:ncol(data2))){
    df <- coalesce(dat15[,pp], dat25[,pp])
    df <- coalesce(df, dat35[,pp])
    
    if (prod(dim(alldat)) == 0){
      alldat <- df
    } else {
      alldat <- cbind(alldat, df)
    }
  }
  
  alldat <- as.data.frame(alldat)
  alldata <- cbind(ptrialno, alldat)
  
  #return(alldat)
  write.csv(alldata, file='data/rdmmir_ErrorSize_index.csv', row.names = F) 
}