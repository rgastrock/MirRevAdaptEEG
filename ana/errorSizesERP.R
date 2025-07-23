source('ana/shared.R')
source('ana/learningRates.R')

#Error Size indices----
getROTErrorSizeIndices <- function(outlier_rmv = 'n'){
  
  pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
  
  pdata <- as.data.frame(pdata)
  ptrialno <- pdata$trial
  data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
  
  ndat <- data.frame()
  for(pp in c(1:ncol(data2))){
    
    subdat2 <- data2[,pp]
    errs2 <- abs(subdat2)
    errs2 <- abs(errs2 - 30)
    
    if(outlier_rmv == 'y'){
      errs2[errs2 >= 60] <- NA
    }
    
    suberrs2 <- sort(errs2)
    #get 36 lowest values for small (equivalent to 6 blocks late training) and 18 largest for large (equivalent to 3 blocks of early)
    smltrials <- 36
    sml <- head(suberrs2, smltrials)
    lrgtrials <- 18
    lrg <- tail(suberrs2, lrgtrials)
    
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

getMIRErrorSizeIndices <- function(angles = c(15,30,45), outlier_rmv = 'n'){
  
  for(angle in angles){
    data <- read.csv(sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
    data <- as.data.frame(data)
    if(angle == 15){
      trialno <- data$trial
      data2 <- as.matrix(data[,2:dim(data)[2]])
      for(pp in c(1:ncol(data2))){
        subdat2 <- data2[,pp]
        errs2 <- abs(subdat2)
        errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
        data2[,pp] <- errs2
      }
      data15 <- data.frame(trialno, data2)
    } else if (angle == 30){
      trialno <- data$trial
      data2 <- as.matrix(data[,2:dim(data)[2]])
      for(pp in c(1:ncol(data2))){
        subdat2 <- data2[,pp]
        errs2 <- abs(subdat2)
        errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
        data2[,pp] <- errs2
      }
      data30 <- data.frame(trialno, data2)
    } else if (angle == 45){
      trialno <- data$trial
      data2 <- as.matrix(data[,2:dim(data)[2]])
      for(pp in c(1:ncol(data2))){
        subdat2 <- data2[,pp]
        errs2 <- abs(subdat2)
        errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
        data2[,pp] <- errs2
      }
      data45 <- data.frame(trialno, data2)
    }
  }
  
  #combine into one df
  data15[is.na(data15)] <- data30[is.na(data15)]
  data15[is.na(data15)] <- data45[is.na(data15)]
  data <- data15
  trialno <- data$trial
  data2 <- as.matrix(data[,2:dim(data)[2]])
  if(outlier_rmv == 'y'){
    data2[data2 >= 60] <- NA
  }
  
  ndat <- data.frame()
  for(pp in c(1:ncol(data2))){
    
    subdat2 <- data2[,pp]
    suberrs2 <- sort(subdat2)
    #get 36 lowest values for small (equivalent to 6 blocks late training) and 18 largest for large (equivalent to 3 blocks of early)
    smltrials <- 36
    sml <- head(suberrs2, smltrials)
    lrgtrials <- 18
    lrgsuberrs2 <- suberrs2[1:length(suberrs2)-1] #we remove the largest error (consider as outlier)
    lrg <- tail(lrgsuberrs2, lrgtrials)
    
    errsize = c()
    for(i in subdat2){
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
  alldata <- as.data.frame(cbind(trialno, ndat))
  
  write.csv(alldata, file='data/mir_ErrorSize_index.csv', row.names = F) 
}

getRDMErrorSizeIndices <- function(perturbs = c('RDMROT', 'RDMMIR'), angles = c(15,25,35), outlier_rmv='n'){
  for(ptype in perturbs){
    for(angle in angles){
      data <- read.csv(sprintf('data/%s_learningcurve_degrees_%02d.csv', ptype, angle))
      data <- as.data.frame(data)
      if(angle == 15){
        trialno <- data$trial
        data2 <- as.matrix(data[,2:dim(data)[2]])
        for(pp in c(1:ncol(data2))){
          subdat2 <- data2[,pp]
          errs2 <- abs(subdat2)
          errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
          data2[,pp] <- errs2
        }
        data15 <- data.frame(trialno, data2)
      } else if (angle == 25){
        trialno <- data$trial
        data2 <- as.matrix(data[,2:dim(data)[2]])
        for(pp in c(1:ncol(data2))){
          subdat2 <- data2[,pp]
          errs2 <- abs(subdat2)
          errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
          data2[,pp] <- errs2
        }
        data25 <- data.frame(trialno, data2)
      } else if (angle == 35){
        trialno <- data$trial
        data2 <- as.matrix(data[,2:dim(data)[2]])
        for(pp in c(1:ncol(data2))){
          subdat2 <- data2[,pp]
          errs2 <- abs(subdat2)
          errs2 <- abs(errs2 - angle) #perturbation size, to grab errors, not hand deviations
          data2[,pp] <- errs2
        }
        data35 <- data.frame(trialno, data2)
      }
    }
    if(ptype == 'RDMROT'){
      #combine into one df
      data15[is.na(data15)] <- data25[is.na(data15)]
      data15[is.na(data15)] <- data35[is.na(data15)]
      rdmrotdat <- data15
    } else if (ptype == 'RDMMIR'){
      #combine into one df
      data15[is.na(data15)] <- data25[is.na(data15)]
      data15[is.na(data15)] <- data35[is.na(data15)]
      rdmmirdat <- data15
    }
  }
  
  rdmdat <- rbind(rdmrotdat, rdmmirdat)
  trial <- c(1:nrow(rdmdat))
  rdmdat$trialno <- trial
  data <- rdmdat
  trialno <- data$trial
  data2 <- as.matrix(data[,2:dim(data)[2]])
  
  if(outlier_rmv == 'y'){
    data2[data2 >= 60] <- NA
  }
  
  ndat <- data.frame()
  for(pp in c(1:ncol(data2))){
    
    subdat2 <- data2[,pp]
    suberrs2 <- sort(subdat2)
    #get 36 lowest values for small (equivalent to 6 blocks late training) and 18 largest for large (equivalent to 3 blocks of early)
    smltrials <- 36
    sml <- head(suberrs2, smltrials)
    lrgtrials <- 18
    lrgsuberrs2 <- suberrs2[1:length(suberrs2)-1] #we remove the largest error (consider as outlier)
    lrg <- tail(lrgsuberrs2, lrgtrials)
    
    errsize = c()
    for(i in subdat2){
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
  alldata <- as.data.frame(cbind(trialno, ndat))
  
  rdmrotdata <- alldata[1:48,]
  rdmmirdata <- alldata[49:96,]
  rdmmirdata$trialno <- c(1:nrow(rdmmirdata))
  
  write.csv(rdmrotdata, file='data/rdmrot_ErrorSize_index.csv', row.names = F) 
  write.csv(rdmmirdata, file='data/rdmmir_ErrorSize_index.csv', row.names = F) 
}

# getROTErrorSizeIndices <- function(){
#   
#   pdata <- read.csv(file='data/ROT_learningcurve_degrees.csv')
#   
#   pdata <- as.data.frame(pdata)
#   ptrialno <- pdata$trial
#   data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#   
#   ndat <- data.frame()
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
#     errsize = c()
#     for(i in errs2){
#       if(i %in% sml){
#         size <- 'sml'
#       } else if (i %in% lrg){
#         size <- 'lrg'
#       } else{
#         size <- NA
#       }
#       errsize <- c(errsize, size)
#     }
#     
#     if (prod(dim(ndat)) == 0){
#       ndat <- errsize
#     } else {
#       ndat <- cbind(ndat, errsize)
#     }
#   }
#   ndat <- as.data.frame(ndat)
#   df <- cbind(ptrialno, ndat)
#   
#   #return(df)
#   write.csv(df, file='data/rot_ErrorSize_index.csv', row.names = F) 
# }

# getMIRErrorSizeIndices <- function(angles = c(15,30,45)){
#   
#   for(angle in angles){
#     pdata <- read.csv(file=sprintf('data/MIR_learningcurve_degrees_%02d.csv', angle))
#     pdata <- as.data.frame(pdata)
#     ptrialno <- pdata$trial
#     data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#     
#     ndat <- data.frame()
#     for(pp in c(1:ncol(data2))){
#       
#       subdat2 <- data2[,pp]
#       errs2 <- abs(subdat2)
#       errs2 <- abs(errs2 - angle)
#       #errs2mean <- mean(errs2, na.rm=T)
#       #errs2sigma <- sd(errs2, na.rm=T)
#       suberrs2 <- sort(errs2)
#       #get lower and upper 40% of trials (12 trials each), disregard middle 20% or 6 trials
#       ntrials <- 12
#       sml <- head(suberrs2, ntrials)
#       lrg <- tail(suberrs2, ntrials)
#       
#       errsize = c()
#       for(i in errs2){
#         if(i %in% sml){
#           size <- 'sml'
#         } else if (i %in% lrg){
#           size <- 'lrg'
#         } else{
#           size <- NA
#         }
#         errsize <- c(errsize, size)
#       }
#       
#       if (prod(dim(ndat)) == 0){
#         ndat <- errsize
#       } else {
#         ndat <- cbind(ndat, errsize)
#       }
#     }
#     
#     if(angle == 15){
#       dat15 <- ndat
#     } else if (angle == 30){
#       dat30 <- ndat
#     } else if (angle == 45){
#       dat45 <- ndat
#     }
#   }
#   
#   dat15 <- as.data.frame(dat15)
#   dat30 <- as.data.frame(dat30)
#   dat45 <- as.data.frame(dat45)
#   
#   alldat <- data.frame()
#   for(pp in c(1:ncol(data2))){
#     df <- coalesce(dat15[,pp], dat30[,pp])
#     df <- coalesce(df, dat45[,pp])
#     
#     if (prod(dim(alldat)) == 0){
#       alldat <- df
#     } else {
#       alldat <- cbind(alldat, df)
#     }
#   }
#   
#   alldat <- as.data.frame(alldat)
#   alldata <- cbind(ptrialno, alldat)
#   
#   #return(alldat)
#   write.csv(alldata, file='data/mir_ErrorSize_index.csv', row.names = F) 
# }

# getRDMROTErrorSizeIndices <- function(angles = c(15,25,35)){
#   
#   for(angle in angles){
#     pdata <- read.csv(file=sprintf('data/RDMROT_learningcurve_degrees_%02d.csv', angle))
#     pdata <- as.data.frame(pdata)
#     ptrialno <- pdata$trial
#     data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#     
#     ndat <- data.frame()
#     for(pp in c(1:ncol(data2))){
#       
#       subdat2 <- data2[,pp]
#       errs2 <- abs(subdat2)
#       errs2 <- abs(errs2 - angle)
#       #errs2mean <- mean(errs2, na.rm=T)
#       #errs2sigma <- sd(errs2, na.rm=T)
#       suberrs2 <- sort(errs2)
#       #nondivisible number of trials (16), but get lower and upper ~44% of trials (~7 trials each), disregard middle ~12% or ~2 trials
#       ntrials <- 7
#       sml <- head(suberrs2, ntrials)
#       lrg <- tail(suberrs2, ntrials)
#       
#       errsize = c()
#       for(i in errs2){
#         if(i %in% sml){
#           size <- 'sml'
#         } else if (i %in% lrg){
#           size <- 'lrg'
#         } else{
#           size <- NA
#         }
#         errsize <- c(errsize, size)
#       }
#       
#       if (prod(dim(ndat)) == 0){
#         ndat <- errsize
#       } else {
#         ndat <- cbind(ndat, errsize)
#       }
#     }
#     
#     if(angle == 15){
#       dat15 <- ndat
#     } else if (angle == 25){
#       dat25 <- ndat
#     } else if (angle == 35){
#       dat35 <- ndat
#     }
#   }
#   
#   dat15 <- as.data.frame(dat15)
#   dat25 <- as.data.frame(dat25)
#   dat35 <- as.data.frame(dat35)
#   
#   alldat <- data.frame()
#   for(pp in c(1:ncol(data2))){
#     df <- coalesce(dat15[,pp], dat25[,pp])
#     df <- coalesce(df, dat35[,pp])
#     
#     if (prod(dim(alldat)) == 0){
#       alldat <- df
#     } else {
#       alldat <- cbind(alldat, df)
#     }
#   }
#   
#   alldat <- as.data.frame(alldat)
#   alldata <- cbind(ptrialno, alldat)
#   
#   #return(alldat)
#   write.csv(alldata, file='data/rdmrot_ErrorSize_index.csv', row.names = F) 
# }
# 
# getRDMMIRErrorSizeIndices <- function(angles = c(15,25,35)){
#   
#   for(angle in angles){
#     pdata <- read.csv(file=sprintf('data/RDMMIR_learningcurve_degrees_%02d.csv', angle))
#     pdata <- as.data.frame(pdata)
#     ptrialno <- pdata$trial
#     data2 <- as.matrix(pdata[,2:dim(pdata)[2]])
#     
#     ndat <- data.frame()
#     for(pp in c(1:ncol(data2))){
#       
#       subdat2 <- data2[,pp]
#       errs2 <- abs(subdat2)
#       errs2 <- abs(errs2 - angle)
#       #errs2mean <- mean(errs2, na.rm=T)
#       #errs2sigma <- sd(errs2, na.rm=T)
#       suberrs2 <- sort(errs2)
#       #nondivisible number of trials (16), but get lower and upper ~44% of trials (~7 trials each), disregard middle ~12% or ~2 trials
#       ntrials <- 7
#       sml <- head(suberrs2, ntrials)
#       lrg <- tail(suberrs2, ntrials)
#       
#       errsize = c()
#       for(i in errs2){
#         if(i %in% sml){
#           size <- 'sml'
#         } else if (i %in% lrg){
#           size <- 'lrg'
#         } else{
#           size <- NA
#         }
#         errsize <- c(errsize, size)
#       }
#       
#       if (prod(dim(ndat)) == 0){
#         ndat <- errsize
#       } else {
#         ndat <- cbind(ndat, errsize)
#       }
#     }
#     
#     if(angle == 15){
#       dat15 <- ndat
#     } else if (angle == 25){
#       dat25 <- ndat
#     } else if (angle == 35){
#       dat35 <- ndat
#     }
#   }
#   
#   dat15 <- as.data.frame(dat15)
#   dat25 <- as.data.frame(dat25)
#   dat35 <- as.data.frame(dat35)
#   
#   alldat <- data.frame()
#   for(pp in c(1:ncol(data2))){
#     df <- coalesce(dat15[,pp], dat25[,pp])
#     df <- coalesce(df, dat35[,pp])
#     
#     if (prod(dim(alldat)) == 0){
#       alldat <- df
#     } else {
#       alldat <- cbind(alldat, df)
#     }
#   }
#   
#   alldat <- as.data.frame(alldat)
#   alldata <- cbind(ptrialno, alldat)
#   
#   #return(alldat)
#   write.csv(alldata, file='data/rdmmir_ErrorSize_index.csv', row.names = F) 
# }


#Plotting Small/ Large EEG Data (ERN/FRN)----

getSmallLargeERPCI <- function(groups = c('aln', 'smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotSmallLargeERPs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='frn') {
      svglite(file=sprintf('doc/fig/Fig1B_FRN_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & erps=='ern') {
      svglite(file=sprintf('doc/fig/Fig10B_ERN_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'smallrot', 'largerot')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      # abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,0,legend=c('Aligned','Small ROT', 'Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      groups = c('aln', 'smallrdm', 'largerdm')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      # abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,0,legend=c('Aligned','Small RDM', 'Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'smallmir', 'largemir')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      if (erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("ERP time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      # abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if (erps == 'frn'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      }
      
      
      #add legend
      legend(0.8,0,legend=c('Aligned','Small MIR', 'Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getDiffWavesSmallLargeCI <- function(groups = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotSmallLargeDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'frn') {
      svglite(file=sprintf('doc/fig/Fig2B_FRN_DiffWaves_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & erps == 'ern') {
      svglite(file=sprintf('doc/fig/Fig11B_ERN_DiffWaves_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('smallrot', 'largerot')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c( -0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if(erps=='frn'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,0,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'rdm'){
      groups = c('smallrdm', 'largerdm')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if(erps=='frn'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,0,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      groups = c('smallmir', 'largemir')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-6, 16), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      if(erps=='frn'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
      }
      
      #add legend
      legend(0.8,0,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getPTypeDiffWavesSmallLargeCI <- function(groups = c('rot_diff', 'rdm_diff', 'mir_diff'), type = 'b', erps = 'frn'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SvL_%s_%s.csv', group, erps))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    timepts <- data$time
    data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
    data1 <- data1*-1 #multiply by -1 to change direction (large minus small is now small minus large)
    
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_SmallLarge_SvL_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

#Plotting Small/ Large EEG Data (Readiness Potential)----

getSmallLargeRPCI <- function(groups = c('aln', 'smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'rp'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotSmallLargeRP <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'rp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig41A_RP_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'smallrot', 'largerot')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("RP time-locked to go signal onset, %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = 4, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Aligned','Small ROT', 'Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      groups = c('aln', 'smallrdm', 'largerdm')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("RP time-locked to go signal onset, %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = 4, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Aligned','Small RDM', 'Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'smallmir', 'largemir')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("RP time-locked to go signal onset, %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = 4, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Aligned','Small MIR', 'Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getRPDiffWavesSmallLargeCI <- function(groups = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'rp'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotRPSmallLargeDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'rp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig41B_FRN_DiffWaves_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('smallrot', 'largerot')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
        
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'rdm'){
      groups = c('smallrdm', 'largerdm')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      groups = c('smallmir', 'largemir')
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-1.1, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      text(-1, 6, 'target onset', cex = 0.85)
      text(0, 6, 'go signal', cex = 0.85)
      axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[201:701,] #grab timepts we need
        
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
        if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
          err <- 'sml'
        } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
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
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      
      
      #add legend
      legend(0.2,-10,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

getRPPTypeDiffWavesSmallLargeCI <- function(groups = c('rot_diff', 'rdm_diff', 'mir_diff'), type = 'b', erps = 'rp'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SvL_%s_%s.csv', group, erps))
    data <- data[,2:length(data)]
    
    data <- as.data.frame(data)
    timepts <- data$time
    data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
    data1 <- data1*-1 #multiply by -1 to change direction (large minus small is now small minus large)
    
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_SmallLarge_SvL_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}


#Plotting Small/ Large LRPs----
getLRPSmallLargeCI <- function(groups = c('aln', 'smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'lrp', channels = c('C3','C4')){
  for(channel in channels){
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_%s.csv', group, erps, channel))
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
        
        write.csv(confidence, file=sprintf('data/ERP_SmallLarge_CI_%s_%s_%s.csv', group, erps, channel), row.names = F) 
        
      }
    }
  }
}

plotSmallLargeLRPs <- function(groups = c('aln','smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'lrp', channels = c('C3','C4')) {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4C_LRP_SmallLarge.svg', width=16, height=24, pointsize=14, system_fonts=list(sans="Arial"))
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
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (channel in channels){
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_%s.csv', group, erps, channel))
      timepts <- data$time
      timepts <- timepts[201:701] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in files created by getGroupConfidenceInterval in filehandling.R
      groupconfidence <- read.csv(file=sprintf('data/ERP_SmallLarge_CI_%s_%s_%s.csv', group, erps, channel))
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
    } else if (group == 'smallrot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'largerot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
    } else if (group == 'smallrdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'largerdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
    } else if (group == 'smallmir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    } else if (group == 'largemir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
    }
    colourscheme <- getSmallLargeERPColourScheme(groups = group)
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
getLRPDiffWavesSmallLargeCI <- function(groups = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'lrp'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s_C3.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s_C3.csv', group, erps), row.names = F) 
      
    }
  }
}

plotLRPDiffWavesSmallLarge <- function(groups = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'lrp') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5B_LRP_SmallLarge_DiffWaves.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s_C3.csv', group, erps))
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
    groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_CI_%s_%s_C3.csv', group, erps))
    groupconfidence <- groupconfidence[201:701,] #grab timepts we need
    
    colourscheme <- getSmallLargeERPColourScheme(groups = group)
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
      colourscheme <- getSmallLargeERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 5, pch = 20, cex = 1.5, col=col)
    } else if (group == 'largerot'){
      mo <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      colourscheme <- getSmallLargeERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(4, 4), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 4, pch = 20, cex = 1.5, col=col)
    } else if (group == 'largerdm'){
      mo <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      colourscheme <- getSmallLargeERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(3.5, 3.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 3.5, pch = 20, cex = 1.5, col=col)
    } else if (group == 'largemir'){
      mo <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      colourscheme <- getSmallLargeERPColourScheme(groups = group)
      col <- colourscheme[[group]][['T']]
      lines(x = c(mo[,1], mo[,3]), y = c(3, 3), col = col, lty = 1, lwd = 8)
      col <- colourscheme[[group]][['S']]
      points(x = mo[,2], y = 3, pch = 20, cex = 1.5, col=col)
    }
  }
  
  
  #add legend
  legend(0.8,-5,legend=c('Small ROT - Aligned', 'Large ROT - Aligned', 'Small RDM - Aligned', 'Large RDM - Aligned', 'Small MIR - Aligned', 'Large MIR - Aligned'),
         col=c(colourscheme[['smallrot']][['S']],colourscheme[['largerot']][['S']],colourscheme[['smallrdm']][['S']],colourscheme[['largerdm']][['S']],colourscheme[['smallmir']][['S']],colourscheme[['largemir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}