source('ana/shared.R')
source('ana/learningRates.R')

#plot ERP for P3----
#P3 has two components: P3a (150-280 ms post feedback onset) and P3b (280 - 500 ms post feedback onset)

#we have 5 blocks for rot and mir (18 trials each block), and 4 blocks for rdm (12 trials each)
#for simplicity we can just do 1st and last block for perturb conditions, and 1 block for aligned
getP3ConfidenceInterval <- function(groups = c('aln', 'rot_b0', 'rot_b4', 'mir_b0', 'mir_b4', 'rdm_b0', 'rdm_b3'), type = 'b', erps = 'P3'){
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
      
      write.csv(confidence, file=sprintf('data/ERP_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotP3 <- function(groups = c('aln', 'rot_b0', 'rot_b4', 'mir_b0', 'mir_b4', 'rdm_b0', 'rdm_b3'), target='inline', erps = 'P3') {
  
  
  #but we can save plot as svg file
  if(target=='svg'){
    svglite(file='doc/fig/Fig13_P3.svg', width=16, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
    timepts <- data$time
    timepts <- timepts[101:701] #remove .5 seconds before and after -1.5 and 1.5
  }
  
  #NA to create empty plot
  # could maybe use plot.new() ?
  
  plot(NA, NA, xlim = c(-1.6, 1.6), ylim = c(-16, 8), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "ERP time-locked to feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
  axis(1, at = c(-1.5, -1, -0.5, -0.25, 0, 0.15, 0.28, 0.5, 1, 1.5),labels=c('-1.50','-1.00','-0.50','-0.25','0','0.15','0.28', '0.50', '1.00', '1.50')) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
  
  for(group in groups){
    #read in files created by getGroupConfidenceInterval in filehandling.R
    groupconfidence <- read.csv(file=sprintf('data/ERP_CI_%s_%s.csv', group, erps))
    groupconfidence <- groupconfidence[101:701,] #grab timepts we need
    

    colourscheme <- getP3ColourScheme(group=group)
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
  
  
  #add legend
  legend(0.8,-5,legend=c('Aligned','Rot_first block', 'Rot_last block', 'Mir_first block', 'Mir_last block', 'Rdm_first block', 'Rdm_last block'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rot_b0']][['S']],colourscheme[['rot_b4']][['S']],colourscheme[['mir_b0']][['S']],colourscheme[['mir_b4']][['S']],colourscheme[['rdm_b0']][['S']],colourscheme[['rdm_b3']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plot Early/ Late ERP for P3----
#P3 has two components: P3a (150-280 ms post feedback onset) and P3b (280 - 500 ms post feedback onset)

#we have 2 blocks for rot and mir (45 trials each block), and 2 blocks for rdm (24 trials each)
getP3EarlyLateCI <- function(groups = c('aln', 'rot_b0', 'rot_b1', 'mir_b0', 'mir_b1', 'rdm_b0', 'rdm_b1'), type = 'b', erps = 'P3'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s.csv', group, erps))
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
      
      write.csv(confidence, file=sprintf('data/EarlyLate_ERP_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotEarlyLateP3 <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig13A_P3_EarlyLate_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'rot_b0', 'rot_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
            xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
            main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
     
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
      
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
        
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Early ROT', 'Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'rdm'){
      groups = c('aln', 'rdm_b0', 'rdm_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
      
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Early RDM', 'Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'mir_b0', 'mir_b1')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/EarlyLate_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
        if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
          err <- 'early'
        } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
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
      
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(15, 15), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 15, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(14.5, 14.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = 14.5, pch = 20, cex = 1.5, col=col)
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Early MIR', 'Late MIR'),
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
getEarlyLatevsALignedP3DiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    if (ptype == 'rot'){
      groups = c('rot_b0', 'rot_b1')
    } else if (ptype == 'mir'){
      groups = c('mir_b0', 'mir_b1')
    } else if (ptype == 'rdm'){
      groups = c('rdm_b0', 'rdm_b1')
    }
    
    for (group in groups){
      adata <- read.csv(file='data/Evoked_DF_EarlyLate_aln_P3.csv')
      pdata <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_P3.csv', group))
      row_idx <- adata[,1]
      timepts <- adata$time
      n <- length(adata) - 1
      diffdata <- pdata[,2:n] - adata[,2:n]
      diffdata$idx <- row_idx
      diffdata$timepts <- timepts
      write.csv(diffdata, file=sprintf('data/Evoked_DF_vsAligned_%s_P3.csv', group), row.names = F) 
    }
  }
}

getEarlyLatevsALignedP3DiffWavesCI <- function(groups = c('rot_b0', 'rot_b1', 'rdm_b0', 'rdm_b1', 'mir_b0', 'mir_b1'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_vsAligned_%s_P3.csv', group))
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
      
      write.csv(confidence, file=sprintf('data/Evoked_DF_vsAligned_%s_P3_CI.csv', group), row.names = F) 
      
    }
  }
}

getEarlyvsLatePTypeP3DiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    
    earlydat <- read.csv(file=sprintf('data/Evoked_DF_vsAligned_%s_b0_P3.csv', ptype))
    latedat <- read.csv(file=sprintf('data/Evoked_DF_vsAligned_%s_b1_P3.csv', ptype))
    
    row_idx <- earlydat$idx
    timepts <- earlydat$timepts
    n <- length(earlydat) - 2
    diffdata <- latedat[,1:n] - earlydat[,1:n]
    diffdata$idx <- row_idx
    diffdata$timepts <- timepts
    write.csv(diffdata, file=sprintf('data/Evoked_DF_EarlyvsLate_%s_P3.csv', ptype), row.names = F) 
    
  }
}

getEarlyvsLatePTypeP3DiffWavesCI <- function(perturbs = c('rot', 'rdm', 'mir'), type = 'b'){
  for (ptype in perturbs){
    data <- read.csv(file=sprintf('data/Evoked_DF_EarlyvsLate_%s_P3.csv', ptype))
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
      
      write.csv(confidence, file=sprintf('data/Evoked_DF_EarlyvsLate_%s_P3_CI.csv', ptype), row.names = F) 
      
    }
  }
}


#plot Small/ Large ERP for P3----
#P3 has two components: P3a (150-280 ms post feedback onset) and P3b (280 - 500 ms post feedback onset)

#we have small and large conditions for rot, mir, rdm
getP3SmallLargeCI <- function(groups = c('aln', 'rot_sml', 'rot_lrg', 'mir_sml', 'mir_lrg', 'rdm_sml', 'rdm_lrg'), type = 'b', erps = 'P3'){
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
      
      write.csv(confidence, file=sprintf('data/SmallLarge_ERP_CI_%s_%s.csv', group, erps), row.names = F) 
      
    }
  }
}

plotSmallLargeP3 <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig13B_P3_SmallLarge_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('aln', 'rot_sml', 'rot_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c( -5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
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
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Small ROT', 'Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'rdm'){
      groups = c('aln', 'rdm_sml', 'rdm_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
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
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Small RDM', 'Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'mir'){
      groups = c('aln', 'mir_sml', 'mir_lrg')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-6, 16), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3)
      axis(1, at = c(-0.25, 0, 0.15, 0.28, 0.5, 1),labels=c('-0.25','0','0.15','0.28', '0.50', '1.00')) #tick marks for x axis
      axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
      #axis(3, at = c(0.215, 0.39), labels=c('P3a', 'P3b'), tick = FALSE)
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
        timepts <- data$time
        timepts <- timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/SmallLarge_ERP_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:601,] #grab timepts we need
        
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
      
      
      #add legend
      legend(0.8,5,legend=c('Aligned','Small MIR', 'Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

#need difference waves between each condition and aligned, as well as early vs rot
getSmallLargevsALignedP3DiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    if (ptype == 'rot'){
      groups = c('rot_sml', 'rot_lrg')
    } else if (ptype == 'mir'){
      groups = c('mir_sml', 'mir_lrg')
    } else if (ptype == 'rdm'){
      groups = c('rdm_sml', 'rdm_lrg')
    }
    
    for (group in groups){
      adata <- read.csv(file='data/Evoked_DF_SmallLarge_aln_P3.csv')
      pdata <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_P3.csv', group))
      row_idx <- adata[,1]
      timepts <- adata$time
      n <- length(adata) - 1
      diffdata <- pdata[,2:n] - adata[,2:n]
      diffdata$idx <- row_idx
      diffdata$timepts <- timepts
      write.csv(diffdata, file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_P3.csv', group), row.names = F) 
    }
  }
}

getSmallLargevsALignedP3DiffWavesCI <- function(groups = c('rot_sml', 'rot_lrg', 'rdm_sml', 'rdm_lrg', 'mir_sml', 'mir_lrg'), type = 'b'){
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_P3.csv', group))
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
      
      write.csv(confidence, file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_P3_CI.csv', group), row.names = F) 
      
    }
  }
}

getSmallvsLargePTypeP3DiffWaves <- function(perturbs = c('rot', 'rdm', 'mir')) {
  
  for(ptype in perturbs){
    
    smalldat <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_sml_P3.csv', ptype))
    largedat <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_lrg_P3.csv', ptype))
    
    row_idx <- smalldat$idx
    timepts <- smalldat$timepts
    n <- length(smalldat) - 2
    diffdata <- largedat[,1:n] - smalldat[,1:n]
    diffdata$idx <- row_idx
    diffdata$timepts <- timepts
    write.csv(diffdata, file=sprintf('data/Evoked_DF_SmallvsLarge_%s_P3.csv', ptype), row.names = F) 
    
  }
}

getSmallvsLargePTypeP3DiffWavesCI <- function(perturbs = c('rot', 'rdm', 'mir'), type = 'b'){
  for (ptype in perturbs){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallvsLarge_%s_P3.csv', ptype))
    timepts <- data$timepts
    n <- length(data) - 2 #remove idx and timepts cols
    data <- data[,1:n]
    data$time <- timepts
    
    data <- as.data.frame(data)
    data1 <- as.matrix(data[,1:(dim(data)[2]-1)])
    data1 <- data1*-1
    
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
      
      write.csv(confidence, file=sprintf('data/Evoked_DF_SmallvsLarge_%s_P3_CI.csv', ptype), row.names = F) 
      
    }
  }
}



#plot P3 across learning----
getAverageBLockedP3<- function(component, group){
  #cannot detect specific time point we want
  #rowidx will be consistent across blocks anyway, so we can hard code this
  #for p3a: we want 0.15 to 0.28 | for p3b: we want 0.29 to 0.50
  if(component == 'a'){
    startidx <- 430 
    endidx <- 456 
  } else if (component == 'b'){
    startidx <- 457
    endidx <- 500
  }
  idxs <- seq(startidx, endidx, 1)
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Evoked_DF_%s_P3.csv', group))

    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$X== i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    
  } else if(group == 'rot' | group == 'mir'){
    blocks <- c(0:4)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_b%s_P3.csv', group, blockno))
      
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$X == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } else if(group == 'rdm'){
    blocks <- c(0:3)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_b%s_P3.csv', group, blockno))

      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$X == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
  }
  
  return(ndatavgs)
  
}

getAverageBlockedP3ConfidenceInterval <- function(groups = c('aln', 'rot', 'mir', 'rdm'), type = 'b', component = 'a'){
  
  for (group in groups){
    
    data <- getAverageBLockedP3(component=component, group=group)
    if(group == 'aln'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/Blocked_P3%s_CI_%s.csv', component, group), row.names = F) 
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
        
        write.csv(confidence, file=sprintf('data/Blocked_P3%s_CI_%s.csv', component, group), row.names = F) 
        
      }
    }
  }
}

plotBlockedP3 <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline', component) {
  
  #component = 'a' or 'b'
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig/Fig14_Blocked_P3%s.svg', component), width=12, height=7, pointsize=18, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 8), ylim = c(-12, 12), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("P3%s across learning", component), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5, 4.5, 5.5, 6.5), labels=c('pre-training', 'block 1', 'block 2', 'block 3', 'block 4', 'block 5'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-10, -5, -2.5, 0, 2.5, 5, 10), labels=c('-10','-5','-2.5','0','2.5','5','10'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_P3%s_CI_%s.csv', component, group))
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
        if(group == 'rot'){
          blockX <- blockno + 1.50
        } else if (group == 'mir'){
          blockX <- blockno + 1.60
        } else if (group == 'rdm'){
          blockX <- blockno + 1.40
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
  legend(4.5,-5,legend=c('aligned', 'random rotation', ' fixed rotation', 'mirror reversal'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rdm']][['S']],colourscheme[['rot']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plot Early/ Late P3 across learning----
getAverageEarlyLateP3<- function(component, group){
  #cannot detect specific time point we want
  #rowidx will be consistent across blocks anyway, so we can hard code this
  #for p3a: we want 0.15 to 0.28 | for p3b: we want 0.29 to 0.50
  if(component == 'a'){
    startidx <- 430 
    endidx <- 456 
  } else if (component == 'b'){
    startidx <- 457
    endidx <- 500
  }
  idxs <- seq(startidx, endidx, 1)
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_P3.csv', group))
    
    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$X== i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    
  } else if(group == 'rot' | group == 'mir' | group == 'rdm'){
    blocks <- c(0:1)
    ndatavgs <- data.frame()
    for(blockno in blocks){
      
      data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_b%s_P3.csv', group, blockno))
      
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$X == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } 
  
  return(ndatavgs)
  
}

getAverageEarlyLateP3ConfidenceInterval <- function(groups = c('aln', 'rot', 'mir', 'rdm'), type = 'b', component = 'a'){
  
  for (group in groups){
    
    data <- getAverageEarlyLateP3(component=component, group=group)
    if(group == 'aln'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/EarlyLate_Average_P3%s_CI_%s.csv', component, group), row.names = F) 
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
        
        write.csv(confidence, file=sprintf('data/EarlyLate_Average_P3%s_CI_%s.csv', component, group), row.names = F) 
        
      }
    }
  }
}

plotAverageEarlyLateP3 <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline', component) {
  
  #component = 'a' or 'b'
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig/Fig14A_Average_EarlyLate_P3%s.svg', component), width=12, height=7, pointsize=18, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 5), ylim = c(-12, 12), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("P3%s across learning", component), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5), labels=c('pre-training', 'early', 'late'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-10, -5, -2.5, 0, 2.5, 5, 10), labels=c('-10','-5','-2.5','0','2.5','5','10'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/EarlyLate_Average_P3%s_CI_%s.csv', component, group))
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
        if(group == 'rot'){
          blockX <- blockno + 1.50
        } else if (group == 'mir'){
          blockX <- blockno + 1.60
        } else if (group == 'rdm'){
          blockX <- blockno + 1.40
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
  legend(3.5,-5,legend=c('aligned', 'random rotation', ' fixed rotation', 'mirror reversal'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rdm']][['S']],colourscheme[['rot']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


#plot Small/ Large P3 across learning----
getAverageSmallLargeP3<- function(component, group){
  #cannot detect specific time point we want
  #rowidx will be consistent across blocks anyway, so we can hard code this
  #for p3a: we want 0.15 to 0.28 | for p3b: we want 0.29 to 0.50
  if(component == 'a'){
    startidx <- 430 
    endidx <- 456 
  } else if (component == 'b'){
    startidx <- 457
    endidx <- 500
  }
  idxs <- seq(startidx, endidx, 1)
  
  #separate condition for aligned
  if(group == 'aln'){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_P3.csv', group))
    
    ndat <- data.frame()
    for(i in idxs){
      subdat <- data[which(data$X== i),]
      
      if (prod(dim(ndat)) == 0){
        ndat <- subdat
      } else {
        ndat <- rbind(ndat, subdat)
      }
    }
    #calculate mean value for each participant
    ndat <- ndat[,2:(dim(ndat)[2]-1)]
    ndatavgs <- as.numeric(colMeans(ndat))
    
  } else if(group == 'rot' | group == 'mir' | group == 'rdm'){
    errsizes <- c('sml','lrg')
    ndatavgs <- data.frame()
    for(s in errsizes){
      
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s_P3.csv', group, s))
      
      ndat <- data.frame()
      for(i in idxs){
        subdat <- data[which(data$X == i),]
        
        if (prod(dim(ndat)) == 0){
          ndat <- subdat
        } else {
          ndat <- rbind(ndat, subdat)
        }
      }
      #calculate mean value for each participant
      ndat <- ndat[,2:(dim(ndat)[2]-1)]
      ndatavg <- as.numeric(colMeans(ndat))
      if (prod(dim(ndatavgs)) == 0){
        ndatavgs <- ndatavg
      } else {
        ndatavgs <- rbind(ndatavgs, ndatavg)
      }
    }
    
  } 
  
  return(ndatavgs)
  
}

getAverageSmallLargeP3ConfidenceInterval <- function(groups = c('aln', 'rot', 'mir', 'rdm'), type = 'b', component = 'a'){
  
  for (group in groups){
    
    data <- getAverageSmallLargeP3(component=component, group=group)
    if(group == 'aln'){
      if (type == "t"){
        data <- data[!is.na(data)]
        citrial <- t.interval(data = data, variance = var(data), conf.level = 0.95)
      } else if(type == "b"){
        citrial <- getBSConfidenceInterval(data = data, resamples = 1000)
      }
      
      confidence <- data.frame(citrial)
      write.csv(confidence, file=sprintf('data/SmallLarge_Average_P3%s_CI_%s.csv', component, group), row.names = F) 
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
        
        write.csv(confidence, file=sprintf('data/SmallLarge_Average_P3%s_CI_%s.csv', component, group), row.names = F) 
        
      }
    }
  }
}

plotAverageSmallLargeP3 <- function(groups = c('aln', 'rot', 'rdm', 'mir'), target='inline', component) {
  
  #component = 'a' or 'b'
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig/Fig14B_Average_SmallLarge_P3%s.svg', component), width=12, height=7, pointsize=18, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow = c(4,2))
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(0, 5), ylim = c(-12, 12), 
       xlab = "Training blocks", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("P3%s across learning", component), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(h = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #text(-1, 6, 'target onset', cex = 0.85)
  #text(0, 6, 'go signal', cex = 0.85)
  #axis(1, at = c(1, 2, 3, 4, 5, 6, 7, 8)) #tick marks for x axis
  axis(side=1, at=c(1.5, 2.5, 3.5), labels=c('pre-training', 'small', 'large'))
  #axis(2, at = c(-5, 0, 5, 10, 15), las=2) #tick marks for y axis
  axis(side=2, at=c(-10, -5, -2.5, 0, 2.5, 5, 10), labels=c('-10','-5','-2.5','0','2.5','5','10'), las = 2)
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/SmallLarge_Average_P3%s_CI_%s.csv', component, group))
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
        if(group == 'rot'){
          blockX <- errsize + 1.50
        } else if (group == 'mir'){
          blockX <- errsize + 1.60
        } else if (group == 'rdm'){
          blockX <- errsize + 1.40
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
  legend(3.5,-5,legend=c('aligned', 'random rotation', ' fixed rotation', 'mirror reversal'),
         col=c(colourscheme[['aln']][['S']],colourscheme[['rdm']][['S']],colourscheme[['rot']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}