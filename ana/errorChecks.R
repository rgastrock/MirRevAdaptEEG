source('ana/shared.R')
source('ana/learningRates.R')

# Lower and upper 40% as small and large errors----

# getGroupROTErrorIndices <- function(){
#   
#   pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
#   
#   pdata <- as.data.frame(pdata)
#   ptrialno <- pdata$trial
#   data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#   
#   for(pp in c(1:ncol(data2))){
#     
#     subdat2 <- data2[,pp]
#     errs2 <- abs(subdat2)
#     errs2 <- abs(errs2 - 30)
#     #errs2mean <- mean(errs2, na.rm=T)
#     #errs2sigma <- sd(errs2, na.rm=T)
#     suberrs2 <- sort(errs2)
#     #get lower and upper 40% of trials (36 trials each), disregard middle 20% or 18 trials
#     ntrials <- 36
#     sml <- head(suberrs2, ntrials)
#     lrg <- tail(suberrs2, ntrials)
#     
#     y = c()
#     for(i in errs2){
#       if(i %in% sml){
#         trialy = 1
#         y = c(y, trialy)
#       } else if (i %in% lrg){
#         trialy = 2
#         y = c(y, trialy)
#       } else{
#         trialy= NA
#         y = c(y, trialy)
#       }
#     }
#     
#     data2[,pp] <- y
#   }
# }

getROTErrorIndices <- function(){
  
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  pdata <- as.data.frame(pdata)
  ptrialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
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
    
    y = c()
    for(i in errs2){
      if(i %in% sml){
        trialy = 1
        y = c(y, trialy)
      } else if (i %in% lrg){
        trialy = 2
        y = c(y, trialy)
      } else{
        trialy= NA
        y = c(y, trialy)
      }
    }
    
    
    plot(ptrialno, y,
         xlab = 'Trial', ylab = 'Error size', main=sprintf('Rotation Errors across trials: Participant %d', pp-1),
         xaxt='n', yaxt='n', frame.plot=FALSE, ylim = c(0.5, 2.5), xlim=c(0,91))
    axis(1, at=c(1,20,40,60,90))
    axis(2, at=c(1,2), labels=c('small','large'))
    
    #dev.off()
  }
}

getMIRErrorIndices <- function(angles = c(15,30,45)){
  
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
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
      
      y = c()
      for(i in errs2){
        if(i %in% sml){
          trialy = 1
          y = c(y, trialy)
        } else if (i %in% lrg){
          trialy = 2
          y = c(y, trialy)
        } else{
          trialy= NA
          y = c(y, trialy)
        }
      }
      
      data2[,pp] <- y
    }
    if(angle == 15){
      dat15 <- data2
    } else if (angle == 30){
      dat30 <- data2
    } else if (angle == 45){
      dat45 <- data2
    }
  }
  dat15[is.na(dat15)] = dat30[is.na(dat15)] #combine 15 and 30
  dat15[is.na(dat15)] = dat45[is.na(dat15)] #combine with 45
  alldat <- dat15
  return(alldat)
  #write.csv(alldat, file='data/ErrorCheck.csv', row.names = F) 
}

plotMIRErrorIndices <- function(){
  data <- getMIRErrorIndices()
  ptrialno <- c(1:nrow(data))
  
  for(pp in c(1:ncol(data))){
    y <- data[,pp]
    plot(ptrialno, y,
         xlab = 'Trial', ylab = 'Error size', main=sprintf('Mirror Errors across trials: Participant %d', pp-1),
         xaxt='n', yaxt='n', frame.plot=FALSE, ylim = c(0.5, 2.5), xlim=c(0,91))
    axis(1, at=c(1,20,40,60,90))
    axis(2, at=c(1,2), labels=c('small','large'))
  }
  
}

#checking across participants-----

getGroupALROTAngularErrors <- function(){
  
  data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  data <- as.data.frame(data)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:dim(data)[2]])
  
  pdata <- as.data.frame(pdata)
  trialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
  aligned <- c()
  small_errors <- c()
  med_errors <-c()
  lrg_errors <- c()
  
  for(pp in c(1:ncol(data1))){
    
    
    
    subdat1 <- data1[,pp]
    errs1 <- abs(subdat1)
    aln <- median(errs1, na.rm=T)
    
    subdat2 <- data2[,pp]
    errs2 <- abs(subdat2)
    errs2 <- abs(errs2 - 30)
    #errs2mean <- mean(errs2, na.rm=T)
    #errs2sigma <- sd(errs2, na.rm=T)
    suberrs2 <- sort(errs2)
    samples <- length(suberrs2)/3
    samples_med_start <- samples+1
    samples_med_end <- samples*2
    samples_lrg_start <- samples_med_end+1
    samples_lrg_end <- samples*3
    sml <- suberrs2[1:samples]
    sml <- median(sml, na.rm=T)
    med <- suberrs2[samples_med_start:samples_med_end]
    med <- median(med, na.rm=T)
    lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
    lrg <- median(lrg, na.rm=T)
    
    aligned <- c(aligned, aln)
    small_errors <- c(small_errors, sml)
    med_errors <-c(med_errors, med)
    lrg_errors <- c(lrg_errors, lrg)
  }
  
  plot(aligned, small_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
  abline(lm(small_errors~aligned))
  
  # plot(aligned, med_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
  # abline(lm(med_errors~aligned))
  # 
  # plot(aligned, lrg_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
  # abline(lm(lrg_errors~aligned))
  
  print(cor.test(aligned, small_errors))
  # print(cor.test(aligned, med_errors))
  # print(cor.test(aligned, lrg_errors))
}

getGroupALMIRAngularErrors <- function(angles=c(15,30,45)){
  par(mfrow = c(1,3))
  for(angle in angles){
    
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    aligned <- c()
    small_errors <- c()
    med_errors <-c()
    lrg_errors <- c()
    
    for(pp in c(1:ncol(data1))){
      
      
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      aln <- median(errs1, na.rm=T)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - 30)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- length(suberrs2)/3
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      sml <- median(sml, na.rm=T)
      med <- suberrs2[samples_med_start:samples_med_end]
      med <- median(med, na.rm=T)
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      lrg <- median(lrg, na.rm=T)
      
      aligned <- c(aligned, aln)
      small_errors <- c(small_errors, sml)
      med_errors <-c(med_errors, med)
      lrg_errors <- c(lrg_errors, lrg)
    }
    
    plot(aligned, small_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(small_errors~aligned))
    
    # plot(aligned, med_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(med_errors~aligned))
    # 
    # plot(aligned, lrg_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(lrg_errors~aligned))
    
    print(cor.test(aligned, small_errors))
    # print(cor.test(aligned, med_errors))
    # print(cor.test(aligned, lrg_errors))
  }
}

getGroupALRDMROTAngularErrors <- function(angles=c(15,25,35)){
  par(mfrow = c(1,3))
  for(angle in angles){
    
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle))
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    aligned <- c()
    small_errors <- c()
    med_errors <-c()
    lrg_errors <- c()
    
    for(pp in c(1:ncol(data1))){
      
      
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      aln <- median(errs1, na.rm=T)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - 30)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3)
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      sml <- median(sml, na.rm=T)
      med <- suberrs2[samples_med_start:samples_med_end]
      med <- median(med, na.rm=T)
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      lrg <- median(lrg, na.rm=T)
      
      aligned <- c(aligned, aln)
      small_errors <- c(small_errors, sml)
      med_errors <-c(med_errors, med)
      lrg_errors <- c(lrg_errors, lrg)
    }
    
    plot(aligned, small_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(small_errors~aligned))
    
    # plot(aligned, med_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(med_errors~aligned))
    # 
    # plot(aligned, lrg_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(lrg_errors~aligned))
    
    print(cor.test(aligned, small_errors))
    # print(cor.test(aligned, med_errors))
    # print(cor.test(aligned, lrg_errors))
  }
}

getGroupALRDMMIRAngularErrors <- function(angles=c(15,25,35)){
  par(mfrow = c(1,3))
  for(angle in angles){
    
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle))
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    aligned <- c()
    small_errors <- c()
    med_errors <-c()
    lrg_errors <- c()
    
    for(pp in c(1:ncol(data1))){
      
      
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      aln <- median(errs1, na.rm=T)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - 30)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3)
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      sml <- median(sml, na.rm=T)
      med <- suberrs2[samples_med_start:samples_med_end]
      med <- median(med, na.rm=T)
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      lrg <- median(lrg, na.rm=T)
      
      aligned <- c(aligned, aln)
      small_errors <- c(small_errors, sml)
      med_errors <-c(med_errors, med)
      lrg_errors <- c(lrg_errors, lrg)
    }
    
    plot(aligned, small_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(small_errors~aligned))
    
    # plot(aligned, med_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(med_errors~aligned))
    # 
    # plot(aligned, lrg_errors, main=sprintf('Median absolute errors'), cex.main=2, cex.lab=2, cex.axis=2)
    # abline(lm(lrg_errors~aligned))
    
    print(cor.test(aligned, small_errors))
    # print(cor.test(aligned, med_errors))
    # print(cor.test(aligned, lrg_errors))
  }
}

#checking each individual participant----

getALROTAngularErrors <- function(){
  
  data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  data <- as.data.frame(data)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:dim(data)[2]])
  
  pdata <- as.data.frame(pdata)
  trialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
  for(pp in c(1:ncol(data1))){
    
    par(mfrow = c(1,3))
    
    subdat1 <- data1[,pp]
    errs1 <- abs(subdat1)
    
    subdat2 <- data2[,pp]
    errs2 <- abs(subdat2)
    errs2 <- abs(errs2 - 30)
    #errs2mean <- mean(errs2, na.rm=T)
    #errs2sigma <- sd(errs2, na.rm=T)
    suberrs2 <- sort(errs2)
    samples <- length(suberrs2)/3
    samples_med_start <- samples+1
    samples_med_end <- samples*2
    samples_lrg_start <- samples_med_end+1
    samples_lrg_end <- samples*3
    sml <- suberrs2[1:samples]
    med <- suberrs2[samples_med_start:samples_med_end]
    lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
    
    
    aln <- tail(errs1, samples)
    plot(aln, sml, main=sprintf('Absolute errors: Participant %d', pp-1), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(sml~aln))
    
    plot(aln, med, main=sprintf('Absolute errors: Participant %d', pp-1), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(med~aln))
    
    plot(aln,lrg, main=sprintf('Absolute errors: Participant %d', pp-1), cex.main=2, cex.lab=2, cex.axis=2)
    abline(lm(lrg~aln))
    
    print(sprintf('Participant: %d', pp-1))
    print(cor.test(aln, sml))
    print(cor.test(aln, med))
    print(cor.test(aln, lrg))
    
    #dev.off()
  }
}

getROTAngularErrorMagnitudeAcrossTrials <- function(){
  
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  pdata <- as.data.frame(pdata)
  ptrialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
  for(pp in c(1:ncol(data2))){
    
    subdat2 <- data2[,pp]
    errs2 <- abs(subdat2)
    errs2 <- abs(errs2 - 30)
    #errs2mean <- mean(errs2, na.rm=T)
    #errs2sigma <- sd(errs2, na.rm=T)
    suberrs2 <- sort(errs2)
    samples <- length(suberrs2)/3
    samples_med_start <- samples+1
    samples_med_end <- samples*2
    samples_lrg_start <- samples_med_end+1
    samples_lrg_end <- samples*3
    sml <- suberrs2[1:samples]
    med <- suberrs2[samples_med_start:samples_med_end]
    lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
    
    y = c()
    for(i in errs2){
      if(i %in% sml){
        trialy = 1
        y = c(y, trialy)
      } else if (i %in% med){
        trialy = 2
        y = c(y, trialy)
      } else if (i %in% lrg){
        trialy = 3
        y = c(y, trialy)
      }
    }

    
    plot(ptrialno, y,
         xlab = 'Trial', ylab = 'Error size', main=sprintf('Errors across trials: Participant %d', pp-1),
         xaxt='n', yaxt='n', frame.plot=FALSE)
    axis(1, at=c(1,20,40,60,90))
    axis(2, at=c(1,2,3), labels=c('sml','med','lrg'))
    
    #dev.off()
  }
}


getALMIRAngularErrors <- function(angles = c(15,30,45)){
  
  for(angle in angles){
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data1))){
      
      par(mfrow = c(1,3))
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- length(suberrs2)/3
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      
      aln <- tail(errs1, samples)
      plot(aln, sml, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(sml~aln))
      
      plot(aln, med, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(med~aln))
      
      plot(aln,lrg, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(lrg~aln))
      
      print(sprintf('Participant: %d', pp-1))
      print(cor.test(aln, sml))
      print(cor.test(aln, med))
      print(cor.test(aln, lrg))
    }
  }
}

getMIRAngularErrorMagnitudeAcrossTrials <- function(angles = c(15,30,45)){
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- length(suberrs2)/3
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      y = c()
      for(i in errs2){
        if(i %in% sml){
          trialy = 1
          y = c(y, trialy)
        } else if (i %in% med){
          trialy = 2
          y = c(y, trialy)
        } else if (i %in% lrg){
          trialy = 3
          y = c(y, trialy)
        }
      }
      
      
      plot(c(1:length(y)), y,
           xlab = 'Trial', ylab = 'Error size', main=sprintf('Errors across trials: Participant %d, %d deg', pp-1, angle),
           xaxt='n', yaxt='n', frame.plot=FALSE)
      axis(1, at=c(1,10,20,30))
      axis(2, at=c(1,2,3), labels=c('sml','med','lrg'))
    }
    
  }
}

getALRDMROTAngularErrors <- function(angles = c(15,25,35)){
  
  for(angle in angles){
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle))
    
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data1))){
      
      par(mfrow = c(1,3))
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3) #random will have 16 trials per angle, so splitting into sml, med, lrg errors will not be divisible, grab just 5 each
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      
      aln <- tail(errs1, samples)
      plot(aln, sml, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(sml~aln))
      
      plot(aln, med, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(med~aln))
      
      plot(aln,lrg, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(lrg~aln))
      
      print(sprintf('Participant: %d', pp-1))
      print(cor.test(aln, sml))
      print(cor.test(aln, med))
      print(cor.test(aln, lrg))
    }
  }
}

getRDMROTAngularErrorMagnitudeAcrossTrials <- function(angles = c(15,30,45)){
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3)
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      y = c()
      for(i in errs2){
        if(i %in% sml){
          trialy = 1
          y = c(y, trialy)
        } else if (i %in% med){
          trialy = 2
          y = c(y, trialy)
        } else if (i %in% lrg){
          trialy = 3
          y = c(y, trialy)
        }
      }
      
      
      plot(c(1:length(y)), y,
           xlab = 'Trial', ylab = 'Error size', main=sprintf('Errors across trials: Participant %d, %d deg', pp-1, angle),
           xaxt='n', yaxt='n', frame.plot=FALSE)
      axis(1, at=c(1,5,10,15))
      axis(2, at=c(1,2,3), labels=c('sml','med','lrg'))
    }
    
  }
}

getALRDMMIRAngularErrors <- function(angles = c(15,25,35)){
  
  for(angle in angles){
    data <- read.csv(file='data/ALIGNED_learningcurve_degrees.csv')
    pdata <- read.csv(file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle))
    
    data <- as.data.frame(data)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:dim(data)[2]])
    
    pdata <- as.data.frame(pdata)
    trialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data1))){
      
      par(mfrow = c(1,3))
      
      subdat1 <- data1[,pp]
      errs1 <- abs(subdat1)
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3) #random will have 16 trials per angle, so splitting into sml, med, lrg errors will not be divisible, grab just 5 each
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      
      aln <- tail(errs1, samples)
      plot(aln, sml, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(sml~aln))
      
      plot(aln, med, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(med~aln))
      
      plot(aln,lrg, main=sprintf('Absolute errors: Participant %d, %d deg', pp-1, angle), cex.main=2, cex.lab=2, cex.axis=2)
      abline(lm(lrg~aln))
      
      print(sprintf('Participant: %d', pp-1))
      print(cor.test(aln, sml))
      print(cor.test(aln, med))
      print(cor.test(aln, lrg))
    }
  }
}

getRDMMIRAngularErrorMagnitudeAcrossTrials <- function(angles = c(15,30,45)){
  for(angle in angles){
    pdata <- read.csv(file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle))
    pdata <- as.data.frame(pdata)
    ptrialno <- pdata$trial
    data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
    
    for(pp in c(1:ncol(data2))){
      
      subdat2 <- data2[,pp]
      errs2 <- abs(subdat2)
      errs2 <- abs(errs2 - angle)
      #errs2mean <- mean(errs2, na.rm=T)
      #errs2sigma <- sd(errs2, na.rm=T)
      suberrs2 <- sort(errs2)
      samples <- floor(length(suberrs2)/3)
      samples_med_start <- samples+1
      samples_med_end <- samples*2
      samples_lrg_start <- samples_med_end+1
      samples_lrg_end <- samples*3
      sml <- suberrs2[1:samples]
      med <- suberrs2[samples_med_start:samples_med_end]
      lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
      
      y = c()
      for(i in errs2){
        if(i %in% sml){
          trialy = 1
          y = c(y, trialy)
        } else if (i %in% med){
          trialy = 2
          y = c(y, trialy)
        } else if (i %in% lrg){
          trialy = 3
          y = c(y, trialy)
        }
      }
      
      
      plot(c(1:length(y)), y,
           xlab = 'Trial', ylab = 'Error size', main=sprintf('Errors across trials: Participant %d, %d deg', pp-1, angle),
           xaxt='n', yaxt='n', frame.plot=FALSE)
      axis(1, at=c(1,5,10,15))
      axis(2, at=c(1,2,3), labels=c('sml','med','lrg'))
    }
    
  }
}


# getALROTAngularErrors <- function(maxppid = 31, location = 'feedback'){
#   
#   data <- getALNGroupLearningCurves(maxppid = maxppid, location = location)
#   pdata <- getROTGroupLearningCurves(maxppid = maxppid, location = location)
#   
#   data <- as.data.frame(data)
#   trialno <- data$trial
#   data1 <- as.matrix(data[,2:dim(data)[2]])
#   
#   pdata <- as.data.frame(pdata)
#   trialno <- pdata$trial
#   data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#   
#   for(pp in c(1:ncol(data1))){
#     print(pp)
#     par(mfrow = c(2,2))
#     
#     subdat1 <- data1[,pp]
#     errs1 <- abs(subdat1)
#     errs1mean <- mean(errs1, na.rm=T)
#     errs1sigma <- sd(errs1, na.rm=T)
#     errs1clip <- errs1mean + (errs1sigma * .5) #only want a + clip since we are dealing with absolute values
#     suberrs1 <- errs1[which(errs1 < errs1clip)]
#     
#     subdat2 <- data2[,pp]
#     errs2 <- abs(subdat2)
#     errs2 <- abs(errs2 - 30)
#     #errs2mean <- mean(errs2, na.rm=T)
#     #errs2sigma <- sd(errs2, na.rm=T)
#     suberrs2 <- sort(errs2[which(errs2 > errs1clip)])
#     samples <- round(length(suberrs2)/3)
#     samples_med_start <- samples+1
#     samples_med_end <- samples*2
#     samples_lrg_start <- samples_med_end+1
#     samples_lrg_end <- samples*3
#     sml <- suberrs2[1:samples]
#     med <- suberrs2[samples_med_start:samples_med_end]
#     lrg <- suberrs2[samples_lrg_start:samples_lrg_end]
#     
#     set.seed(999)
#     aln <- sample(suberrs1, samples)
#     plot(aln, sml, main=sprintf('Absolute errors: Participant %d', pp-1))
#     abline(lm(sml~aln))
#     cor.test(aln, sml)
#     plot(aln, med, main=sprintf('Absolute errors: Participant %d', pp-1))
#     abline(lm(med~aln))
#     cor.test(aln, med)
#     plot(aln,lrg, main=sprintf('Absolute errors: Participant %d', pp-1))
#     abline(lm(lrg~aln))
#     cor.test(aln, lrg)
#     
#     hits <- sort(errs2[which(errs2 < errs1clip)])
#     aln <- sample(suberrs1, length(hits))
#     plot(aln, hits, main=sprintf('Absolute errors: Participant %d', pp-1))
#     abline(lm(hits~aln))
#     cor.test(aln, hits)
#     
#     dev.off()
#   }
# }