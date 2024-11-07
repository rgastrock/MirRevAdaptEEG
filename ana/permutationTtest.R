source('ana/shared.R')
source('ana/learningRates.R')

# FRN Permutation tests (Early vs Late)----
plotPermTestEarlyLateERPs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='frn') {
      svglite(file=sprintf('doc/fig/Fig1C_FRN_EarlyLate_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups <- c('aln', 'earlyrot')
    } else if (ptype == 'laterot'){
      groups <- c('aln', 'laterot')
    } else if (ptype == 'earlyrdm'){
      groups <- c('aln', 'earlyrdm')
    } else if (ptype == 'laterdm'){
      groups <- c('aln', 'laterdm')
    } else if (ptype == 'earlymir'){
      groups <- c('aln', 'earlymir')
    } else if (ptype == 'latemir'){
      groups <- c('aln', 'latemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
      if(ptype == 'earlyrot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'laterot'){
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
      } else if (ptype == 'earlyrdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'laterdm'){
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
      } else if (ptype == 'earlymir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'latemir'){
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
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'earlyrot'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrot') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterot'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterot') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlyrdm'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrdm') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterdm'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterdm') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlymir'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlymir') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'latemir'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'latemir') {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'earlyrot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlyrdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlymir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'latemir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }

    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestEarlyLateDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'frn') {
      svglite(file=sprintf('doc/fig/Fig2C_FRN_DiffWaves_EarlyLate_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('earlyrot', 'laterot')
    } else if (ptype == 'rdm'){
      groups = c('earlyrdm', 'laterdm')
    } else if (ptype == 'mir'){
      groups = c('earlymir', 'latemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    if(erps == 'frn'){
      plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    } else if (erps == 'ern'){
      plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    }
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
      if(ptype == 'rot'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'rdm'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'mir'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['late']][['T']]
        } else {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,-5,legend=c('Early ROT - Aligned', 'Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,-5,legend=c('Early RDM - Aligned', 'Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
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

plotPermTestPTypeEarlyLateDiffWaves <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'frn') {
  
  #but we can save plot as svg file
  if (target=='svg' & erps == 'frn') {
    svglite(file='doc/fig/Fig2E_FRN_DiffWaves_EarlyLate_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(erps == 'frn'){
    plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 10), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = "Late-Early difference: feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_EvL_%s_diff_%s.csv', group, erps))
    full_timepts <- data$time
    timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  if(erps=='frn'){
    mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
    mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
    mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
    
    col <- colourscheme[['rot']][['T']]
    lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(10, 10), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rot']][['S']]
    points(x = mo_rot[,2], y = 10, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['rdm']][['T']]
    lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(9.5, 9.5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rdm']][['S']]
    points(x = mo_rdm[,2], y = 9.5, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['mir']][['T']]
    lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(9, 9), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['mir']][['S']]
    points(x = mo_mir[,2], y = 9, pch = 20, cex = 1.5, col=col)
    
  }
  
  #add legend
  legend(0.85,10,legend=c('Fixed rotation', 'Random rotation', 'Mirror reversal'),
         col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          # lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          # lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1.05, -15, 'rotation vs mirror', col = col, adj=c(0,0))
  text(1.05, -13, 'rotation vs random', col = col, adj=c(0,0))
  text(1.05, -11, 'mirror vs random', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


# LRP Permutation tests (Early vs Late)----
plotPermTestEarlyLateLRPs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps='lrp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8C_EarlyLate_Subtracted_LRP_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups <- c('alnrot', 'rot_b0')
    } else if (ptype == 'laterot'){
      groups <- c('alnrot', 'rot_b1')
    } else if (ptype == 'earlyrdm'){
      groups <- c('alnearlyrdm', 'rdm_b0')
    } else if (ptype == 'laterdm'){
      groups <- c('alnlaterdm', 'rdm_b1')
    } else if (ptype == 'earlymir'){
      groups <- c('alnmir', 'mir_b0')
    } else if (ptype == 'latemir'){
      groups <- c('alnmir', 'mir_b1')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyLate_%s.csv', group))
      full_timepts <- data$time
      timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/EarlyLate_LRP_CI_%s.csv', group))
      groupconfidence <- groupconfidence[201:501,] #grab timepts we need
      
      if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
        err <- 'early'
      } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
        err <- 'late'
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir' | group == 'alnearlyrdm' | group == 'alnlaterdm'){
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
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir' | group == 'alnearlyrdm' | group == 'alnlaterdm'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'earlyrot' | ptype == 'laterot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlymir' | ptype == 'latemir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlyrdm' | ptype == 'laterdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      if(nrow(subdat) == 0){
        break
      }
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'earlyrot'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrot') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterot'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterot') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlyrdm'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrdm') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterdm'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterdm') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlymir'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlymir') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'latemir'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'latemir') {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'earlyrot'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlyrdm'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterdm'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlymir'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'latemir'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  } 
}


plotPermTestEarlyLateLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps='lrp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8D_EarlyLate_LRP_DiffWaves_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('rot_b0', 'rot_b1')
    } else if (ptype == 'rdm'){
      groups = c('rdm_b0', 'rdm_b1')
    } else if (ptype == 'mir'){
      groups = c('mir_b0', 'mir_b1')
    }
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-16, 8), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c( -1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s_vsALigned.csv', group))
      full_timepts <- data$timepts
      timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/Blocked_LRP_DF_%s_vsALigned_CI.csv', group))
      groupconfidence <- groupconfidence[201:501,] #grab timepts we need
      
      if(group == 'rot_b0'|group == 'rdm_b0'|group == 'mir_b0'){
        err <- 'early'
      } else if (group == 'rot_b1'|group == 'rdm_b1'|group == 'mir_b1'){
        err <- 'late'
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'| group == 'alnearlyrdm' | group == 'alnlaterdm'){
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
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'| group == 'alnearlyrdm' | group == 'alnlaterdm'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')

      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['late']][['T']]
        } else {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    if(ptype == 'rot'){
      #add legend
      legend(-1,-5,legend=c('Early ROT - Aligned','Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(-1,-5,legend=c('Early RDM - Aligned','Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(-1,-5,legend=c('Early MIR - Aligned','Late MIR - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeEarlyLateLRPDiffWaves <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'lrp') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig8E_LRP_DiffWaves_EarlyLate_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1.1, 1.20), ylim = c(-16, 10), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Late-Early LRP difference: go signal onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyvsLate_%s.csv', group))
    full_timepts <- data$time
    timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/Blocked_LRP_DF_EarlyvsLate_%s_CI.csv', group))
    groupconfidence <- groupconfidence[201:501,] #grab timepts we need
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(0, lim[3]-1, 0.5, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
  
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(10, 10), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = 10, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(9.5, 9.5), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = 9.5, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(9, 9), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = 9, pch = 20, cex = 1.5, col=col)
  
  
  # #add legend
  # legend(0.25,-5,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(0.55, -15, 'rotation vs mirror', col = col, adj=c(0,0))
  text(0.55, -13, 'rotation vs random', col = col, adj=c(0,0))
  text(0.55, -11, 'mirror vs random', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# P3 Permutation tests (Early vs Late)----
plotPermTestEarlyLateP3 <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='P3') {
      svglite(file=sprintf('doc/fig/Fig13C_P3_EarlyLate_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups <- c('aln', 'rot_b0')
    } else if (ptype == 'laterot'){
      groups <- c('aln', 'rot_b1')
    } else if (ptype == 'earlyrdm'){
      groups <- c('aln', 'rdm_b0')
    } else if (ptype == 'laterdm'){
      groups <- c('aln', 'rdm_b1')
    } else if (ptype == 'earlymir'){
      groups <- c('aln', 'mir_b0')
    } else if (ptype == 'latemir'){
      groups <- c('aln', 'mir_b1')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_EarlyLate_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
    if (erps == 'P3'){
      if(ptype == 'earlyrot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'laterot'){
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
      } else if (ptype == 'earlyrdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'laterdm'){
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
      } else if (ptype == 'earlymir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'latemir'){
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
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'earlyrot'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrot') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterot'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterot') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlyrdm'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrdm') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterdm'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterdm') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlymir'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlymir') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'latemir'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'latemir') {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'earlyrot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlyrdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlymir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'latemir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestEarlyLateDiffWavesP3 <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'P3') {
      svglite(file=sprintf('doc/fig/Fig13D_P3_DiffWaves_EarlyLate_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('rot_b0', 'rot_b1')
    } else if (ptype == 'rdm'){
      groups = c('rdm_b0', 'rdm_b1')
    } else if (ptype == 'mir'){
      groups = c('mir_b0', 'mir_b1')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_vsAligned_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
      groupconfidence <- read.csv(file=sprintf('data/Evoked_DF_vsAligned_%s_%s_CI.csv', group, erps))
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
    if(erps=='P3'){
      if(ptype == 'rot'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'rdm'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'mir'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['late']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['late']][['S']]
        points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['late']][['T']]
        } else {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,-5,legend=c('Early ROT - Aligned', 'Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,-5,legend=c('Early RDM - Aligned', 'Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
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

plotPermTestPTypeEarlyLateDiffWavesP3 <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'P3') {
  
  #but we can save plot as svg file
  if (target=='svg' & erps == 'P3') {
    svglite(file='doc/fig/Fig13E_P3_DiffWaves_EarlyLate_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  
  plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Difference Waves time-locked to feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_EarlyvsLate_%s_%s.csv', group, erps))
    full_timepts <- data$time
    timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/Evoked_DF_EarlyvsLate_%s_%s_CI.csv', group, erps))
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  if(erps=='P3'){
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
  legend(.8,-5,legend=c('Rot', 'Rdm', 'Mir'),
         col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1, -15, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(1, -13, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(1, -11, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# FRN Permutation tests (Small vs Large)----
plotPermTestSmallLargeERPs <- function(perturbs = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='frn') {
      svglite(file=sprintf('doc/fig/Fig1D_FRN_SmallLarge_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'smallrot'){
      groups <- c('aln', 'smallrot')
    } else if (ptype == 'largerot'){
      groups <- c('aln', 'largerot')
    } else if (ptype == 'smallrdm'){
      groups <- c('aln', 'smallrdm')
    } else if (ptype == 'largerdm'){
      groups <- c('aln', 'largerdm')
    } else if (ptype == 'smallmir'){
      groups <- c('aln', 'smallmir')
    } else if (ptype == 'largemir'){
      groups <- c('aln', 'largemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
      if(ptype == 'smallrot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largerot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'smallrdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largerdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'smallmir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largemir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_SmallLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'smallrot'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrot') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerot'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerot') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallrdm'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrdm') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerdm'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerdm') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallmir'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallmir') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largemir'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largemir') {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'smallrot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallrdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallmir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largemir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestSmallLargeDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'frn') {
      svglite(file=sprintf('doc/fig/Fig2F_FRN_DiffWaves_SmallLarge_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('smallrot', 'largerot')
    } else if (ptype == 'rdm'){
      groups = c('smallrdm', 'largerdm')
    } else if (ptype == 'mir'){
      groups = c('smallmir', 'largemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    if(erps == 'frn'){
      plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    } else if (erps == 'ern'){
      plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    }
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/DiffWaves_DF_SmallLarge_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
      if(ptype == 'rot'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'rdm'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'mir'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallvsLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['lrg']][['T']]
        } else {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,-5,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,-5,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(0.8,-5,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeSmallLargeDiffWaves <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'frn') {
  
  #but we can save plot as svg file
  if (target=='svg' & erps == 'frn') {
    svglite(file='doc/fig/Fig2G_FRN_DiffWaves_SmallLarge_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(erps == 'frn'){
    plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 10), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = "Large-Small difference: feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_SvL_%s_diff_%s.csv', group, erps))
    full_timepts <- data$time
    timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/DiffWaves_SmallLarge_SvL_CI_%s_diff_%s.csv', group, erps))
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  if(erps=='frn'){
    mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
    mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
    mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
    
    col <- colourscheme[['rot']][['T']]
    lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(10, 10), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rot']][['S']]
    points(x = mo_rot[,2], y = 10, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['rdm']][['T']]
    lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(9.5, 9.5), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['rdm']][['S']]
    points(x = mo_rdm[,2], y = 9.5, pch = 20, cex = 1.5, col=col)
    
    col <- colourscheme[['mir']][['T']]
    lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(9, 9), col = col, lty = 1, lwd = 8)
    col <- colourscheme[['mir']][['S']]
    points(x = mo_mir[,2], y = 9, pch = 20, cex = 1.5, col=col)
    
  }
  
  # #add legend
  # legend(.8,-5,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallLarge_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1.05, -15, 'rotation vs mirror', col = col, adj=c(0,0))
  text(1.05, -13, 'rotation vs random', col = col, adj=c(0,0))
  text(1.05, -11, 'mirror vs random', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# LRP Permutation tests (Small vs Large)----
plotPermTestSmallLargeLRPs <- function(perturbs = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps='lrp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8F_SmallLarge_Subtracted_LRP_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'smallrot'){
      groups <- c('alnrot', 'rot_sml')
    } else if (ptype == 'largerot'){
      groups <- c('alnrot', 'rot_lrg')
    } else if (ptype == 'smallrdm'){
      groups <- c('aln', 'rdm_sml')
    } else if (ptype == 'largerdm'){
      groups <- c('aln', 'rdm_lrg')
    } else if (ptype == 'smallmir'){
      groups <- c('alnmir', 'mir_sml')
    } else if (ptype == 'largemir'){
      groups <- c('alnmir', 'mir_lrg')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s.csv', group))
      full_timepts <- data$time
      timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/SmallLarge_LRP_CI_%s.csv', group))
      groupconfidence <- groupconfidence[201:501,] #grab timepts we need
      
      if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
        err <- 'sml'
      } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
        err <- 'lrg'
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
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
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'smallrot' | ptype == 'largerot'){
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
    } else if (ptype == 'smallmir' | ptype == 'largemir'){
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
    } else if (ptype == 'smallrdm' | ptype == 'largerdm'){
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
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_SmallLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'smallrot'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrot') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerot'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerot') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallrdm'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrdm') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerdm'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerdm') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallmir'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallmir') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largemir'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largemir') {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'smallrot'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Small ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerot'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallrdm'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Small RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerdm'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallmir'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Small MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largemir'){
      #add legend
      legend(-1,-5,legend=c('Aligned','Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  } 
}

plotPermTestSmallLargeLRPDiffWaves <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps='lrp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8G_SmallLarge_LRP_DiffWaves_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('rot_sml', 'rot_lrg')
    } else if (ptype == 'rdm'){
      groups = c('rdm_sml', 'rdm_lrg')
    } else if (ptype == 'mir'){
      groups = c('mir_sml', 'mir_lrg')
    }
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("LRP time-locked to go signal onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_vsALigned.csv', group))
      full_timepts <- data$timepts
      timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallLarge_%s_vsALigned_CI.csv', group))
      groupconfidence <- groupconfidence[201:501,] #grab timepts we need
      
      if(group == 'rot_sml'|group == 'rdm_sml'|group == 'mir_sml'){
        err <- 'sml'
      } else if (group == 'rot_lrg'|group == 'rdm_lrg'|group == 'mir_lrg'){
        err <- 'lrg'
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
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
      } else if (group == 'aln' | group == 'alnrot' | group == 'alnmir'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallvsLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['lrg']][['T']]
        } else {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    if(ptype == 'rot'){
      #add legend
      legend(-1,-5,legend=c('Small ROT - Aligned','Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(-1,-5,legend=c('Small RDM - Aligned','Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(-1,-5,legend=c('Small MIR - Aligned','Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeSmallLargeLRPDiffWaves <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'lrp') {
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig8H_LRP_DiffWaves_SmallLarge_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  plot(NA, NA, xlim = c(-1.1, 1.20), ylim = c(-16, 10), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Large-Small LRP difference: go signal onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallvsLarge_%s.csv', group))
    full_timepts <- data$time
    timepts <- full_timepts[201:501] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/Blocked_LRP_DF_SmallvsLarge_%s_CI.csv', group))
    groupconfidence <- groupconfidence[201:501,] #grab timepts we need
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(0, lim[3]-1, 0.5, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
  
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(10, 10), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = 10, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(9.5, 9.5), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = 9.5, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(9, 9), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = 9, pch = 20, cex = 1.5, col=col)
  
  # #add legend
  # legend(.25,-5,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallLarge_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[201:401]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(0.55, -15, 'rotation vs mirror', col = col, adj=c(0,0))
  text(0.55, -13, 'rotation vs random', col = col, adj=c(0,0))
  text(0.55, -11, 'mirror vs random', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# P3 Permutation tests (Small vs Large)----
plotPermTestSmallLargeP3 <- function(perturbs = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='P3') {
      svglite(file=sprintf('doc/fig/Fig13F_P3_SmallLarge_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'smallrot'){
      groups <- c('aln', 'rot_sml')
    } else if (ptype == 'largerot'){
      groups <- c('aln', 'rot_lrg')
    } else if (ptype == 'smallrdm'){
      groups <- c('aln', 'rdm_sml')
    } else if (ptype == 'largerdm'){
      groups <- c('aln', 'rdm_lrg')
    } else if (ptype == 'smallmir'){
      groups <- c('aln', 'mir_sml')
    } else if (ptype == 'largemir'){
      groups <- c('aln', 'mir_lrg')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
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
    if (erps == 'P3'){
      if(ptype == 'smallrot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largerot'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'smallrdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largerdm'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'smallmir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['sml']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['sml']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'largemir'){
        mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['aligned']][['T']]
        lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['aligned']][['S']]
        points(x = mo_aln[,2], y = 5, pch = 20, cex = 1.5, col=col)
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_SmallLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'smallrot'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrot') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerot'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerot') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallrdm'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrdm') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerdm'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerdm') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallmir'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallmir') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largemir'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largemir') {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'smallrot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerot'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallrdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerdm'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallmir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Small MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largemir'){
      #add legend
      legend(0.8,-5,legend=c('Aligned','Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestSmallLargeDiffWavesP3 <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'P3') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps == 'P3') {
      svglite(file=sprintf('doc/fig/Fig13G_P3_DiffWaves_SmallLarge_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('rot_sml', 'rot_lrg')
    } else if (ptype == 'rdm'){
      groups = c('rdm_sml', 'rdm_lrg')
    } else if (ptype == 'mir'){
      groups = c('mir_sml', 'mir_lrg')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_%s.csv', group, erps))
      full_timepts <- data$time
      timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
      
      groupconfidence <- read.csv(file=sprintf('data/Evoked_DF_SmallLarge_vsAligned_%s_%s_CI.csv', group, erps))
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
    if(erps=='P3'){
      if(ptype == 'rot'){
        mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rot[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'rdm'){
        mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_rdm[,2], y = 5, pch = 20, cex = 1.5, col=col)
      } else if (ptype == 'mir'){
        mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
        
        col <- colourscheme[['lrg']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(5, 5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['lrg']][['S']]
        points(x = mo_mir[,2], y = 5, pch = 20, cex = 1.5, col=col)
      }
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallvsLarge_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['lrg']][['T']]
        } else {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        lower <- c(rep(-15, length(permtime)))
        upper <- c(rep(-14, length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,-5,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,-5,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(0.8,-5,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeSmallLargeDiffWavesP3 <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'P3') {
  
  #but we can save plot as svg file
  if (target=='svg' & erps == 'P3') {
    svglite(file='doc/fig/Fig13H_P3_DiffWaves_SmallLarge_PermTest_PTypeDiff.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  
  plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
       xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
       main = "Difference Waves time-locked to feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/Evoked_DF_SmallvsLarge_%s_%s.csv', group, erps))
    full_timepts <- data$time
    timepts <- full_timepts[351:601] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/Evoked_DF_SmallvsLarge_%s_%s_CI.csv', group, erps))
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  if(erps=='P3'){
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
  legend(.8,-5,legend=c('Rot', 'Rdm', 'Mir'),
         col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/Permutation_test_SmallLarge_PerturbTypeComp_%s.csv', erps))
    subdat <- permdat[which(permdat$condition == ptype),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        #redefine timepts
        timepts <- full_timepts[401:601]
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-15, length(permtime)))
          upper <- c(rep(-14, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-13, length(permtime)))
          upper <- c(rep(-12, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-11, length(permtime)))
          upper <- c(rep(-10, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1, -15, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(1, -13, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(1, -11, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# TFR confidence intervals for group data----

#roi = 'medfro' or 'latcen'
#erps = 'frn' or 'lrp'
getAllEarlyLateTFRCIs <- function(frequencies = c('theta', 'alpha', 'beta'), roi, erps){
  for (freqs in frequencies){
    getEarlyLateTFRCI(freqs=freqs, roi=roi, erps=erps)
    getDiffWavesEarlyLateTFRCI(freqs=freqs, roi=roi, erps=erps)
    getPTypeDiffWavesEarlyLateTFRCI(freqs=freqs, roi=roi, erps=erps)
  }
}


getEarlyLateTFRCI <- function(groups = c('earlylate_aligned', 'earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

getDiffWavesEarlyLateTFRCI <- function(groups = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_DiffWaves_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

getPTypeDiffWavesEarlyLateTFRCI <- function(groups = c('rot', 'rdm', 'mir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_EvL_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_%s_DiffWaves_EvL_CI_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

# TFR Permutation tests (Early vs Late): Feedback onset----
plotPermTestEarlyLateTFRs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'frn', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs=='alpha') {
      svglite(file=sprintf('doc/fig/Fig16A_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='beta') {
      svglite(file=sprintf('doc/fig/Fig16B_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='theta') {
      svglite(file=sprintf('doc/fig/Fig16C_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups <- c('earlylate_aligned', 'earlyrot')
    } else if (ptype == 'laterot'){
      groups <- c('earlylate_aligned', 'laterot')
    } else if (ptype == 'earlyrdm'){
      groups <- c('earlylate_aligned', 'earlyrdm')
    } else if (ptype == 'laterdm'){
      groups <- c('earlylate_aligned', 'laterdm')
    } else if (ptype == 'earlymir'){
      groups <- c('earlylate_aligned', 'earlymir')
    } else if (ptype == 'latemir'){
      groups <- c('earlylate_aligned', 'latemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-yval -10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s power time-locked to feedback onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      # full_timepts <- data$time
      # timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
      timepts <- data$time
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/TFR_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
        err <- 'early'
      } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
        err <- 'late'
      } else if (group == 'earlylate_aligned'){
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
      } else if (group == 'earlylate_aligned'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset 
    if(ptype == 'earlyrot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['early']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval - 10, yval - 10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['early']][['S']]
      points(x = mo_rot[,2], y = yval - 10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'laterot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlyrdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['early']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-10,yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['early']][['S']]
      points(x = mo_rdm[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'laterdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlymir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['early']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['early']][['S']]
      points(x = mo_mir[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'latemir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_vsAligned_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s', freqs, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'earlyrot'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrot') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterot'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterot') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlyrdm'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrdm') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterdm'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterdm') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlymir'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlymir') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'latemir'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'latemir') {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'earlyrot'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlyrdm'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterdm'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlymir'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'latemir'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}


plotPermTestEarlyLateDiffWavesTFRs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs == 'alpha') {
      svglite(file=sprintf('doc/fig/Fig17A_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'beta') {
      svglite(file=sprintf('doc/fig/Fig17B_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'theta') {
      svglite(file=sprintf('doc/fig/Fig17C_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('earlyrot', 'laterot')
    } else if (ptype == 'rdm'){
      groups = c('earlyrdm', 'laterdm')
    } else if (ptype == 'mir'){
      groups = c('earlymir', 'latemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-yval - 10, yval +10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s time-locked to feedback onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
      timepts <- data$time
      
      groupconfidence <- read.csv(file=sprintf('data/TFR_DiffWaves_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
        err <- 'early'
      } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
        err <- 'late'
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
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset 
    
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = yval, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_EarlyvsLate_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['late']][['T']]
        } else {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,yval,legend=c('Early ROT - Aligned', 'Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,yval,legend=c('Early RDM - Aligned', 'Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(0.8,yval,legend=c('Early MIR - Aligned', 'Late MIR - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeEarlyLateDiffWavesTFRs <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'frn', freqs, roi) {
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'alpha') {
    svglite(file=sprintf('doc/fig/Fig18A_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig18B_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'theta') {
    svglite(file=sprintf('doc/fig/Fig18C_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(roi == 'medfro'){
    reg <- 'Medial frontal'
  } else if (roi == 'latcen'){
    reg <- 'Lateral central'
  }
  if(erps == 'frn'){
    plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-yval - 10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s %s\nLate-Early difference: feedback onset", reg, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  if (freqs == 'alpha'){
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
  } else if (freqs == 'beta'){
    axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
  } else if (freqs == 'theta'){
    axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
  }
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_EvL_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    
    groupconfidence <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_EvL_CI_%s_%s_%s.csv', roi, freqs, group, erps))
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
  
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- yval - (.13*yval)
  scal2 <- yval - (.26*yval)

    
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
    
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(scal, scal), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = scal, pch = 20, cex = 1.5, col=col)
    
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(scal2, scal2), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = scal2, pch = 20, cex = 1.5, col=col)
    
  
  
  #add legend
  if(roi == 'medfro'){
    legend(.95,yval,legend=c('Fixed rotation', 'Random rotation', 'Mirror reversal'),
           col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  }
  
  
  #add in permutation clusters and any significant results
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- 0.13*yval
  scal2 <- 0.26*yval
  mult <- 0.05 #multiplier to keep size consistent
  
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_PerturbTypeComp_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1.05, -yval, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(1.05, -yval + scal, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(1.05, -yval + scal2, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAllEarlyLateTFRs <- function(frequencies = c('theta', 'alpha', 'beta'), roi='medfro'){
  for (freqs in frequencies){
    plotPermTestEarlyLateTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotPermTestEarlyLateDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotPermTestPTypeEarlyLateDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
  }
}

# TFR Permutation tests (Early vs Late): GO onset----
plotGoOnsetPermTestEarlyLateTFRs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'lrp', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs=='alpha') {
      svglite(file=sprintf('doc/fig/Fig19A_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='beta') {
      svglite(file=sprintf('doc/fig/Fig19B_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='theta') {
      svglite(file=sprintf('doc/fig/Fig19C_TFR_EarlyLate_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups <- c('earlylate_aligned', 'earlyrot')
    } else if (ptype == 'laterot'){
      groups <- c('earlylate_aligned', 'laterot')
    } else if (ptype == 'earlyrdm'){
      groups <- c('earlylate_aligned', 'earlyrdm')
    } else if (ptype == 'laterdm'){
      groups <- c('earlylate_aligned', 'laterdm')
    } else if (ptype == 'earlymir'){
      groups <- c('earlylate_aligned', 'earlymir')
    } else if (ptype == 'latemir'){
      groups <- c('earlylate_aligned', 'latemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval -10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s power time-locked to go signal onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      # full_timepts <- data$time
      # timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
      timepts <- data$time
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/TFR_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
        err <- 'early'
      } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
        err <- 'late'
      } else if (group == 'earlylate_aligned'){
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
      } else if (group == 'earlylate_aligned'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'earlyrot' | ptype == 'laterot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlymir' | ptype == 'latemir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'earlyrdm' | ptype == 'laterdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_vsAligned_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s', freqs, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'earlyrot'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrot') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterot'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterot') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlyrdm'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlyrdm') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'laterdm'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'laterdm') {
          col <- colourscheme[['late']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'earlymir'){
          col <- colourscheme[['early']][['T']]
        } else if (p_clust < 0.05 & ptype == 'earlymir') {
          col <- colourscheme[['early']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'latemir'){
          col <- colourscheme[['late']][['T']]
        } else if (p_clust < 0.05 & ptype == 'latemir') {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'earlyrot'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlyrdm'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterdm'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'earlymir'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'latemir'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotGoOnsetPermTestEarlyLateDiffWavesTFRs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'lrp', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs == 'alpha') {
      svglite(file=sprintf('doc/fig/Fig20A_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'beta') {
      svglite(file=sprintf('doc/fig/Fig20B_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'theta') {
      svglite(file=sprintf('doc/fig/Fig20C_TFR_DiffWaves_EarlyLate_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('earlyrot', 'laterot')
    } else if (ptype == 'rdm'){
      groups = c('earlyrdm', 'laterdm')
    } else if (ptype == 'mir'){
      groups = c('earlymir', 'latemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval - 10, yval +10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s time-locked to go signal onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
      timepts <- data$time
      
      groupconfidence <- read.csv(file=sprintf('data/TFR_DiffWaves_EarlyLate_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'earlyrot'|group == 'earlyrdm'|group == 'earlymir'){
        err <- 'early'
      } else if (group == 'laterot'|group == 'laterdm'|group == 'latemir'){
        err <- 'late'
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
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_mir[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['late']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['late']][['S']]
      points(x = mo_rdm[,2], y = yval, pch = 20, cex = 1.5, col=col)
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_EarlyvsLate_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['late']][['T']]
        } else {
          col <- colourscheme[['late']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(-1,yval,legend=c('Early ROT - Aligned', 'Late ROT - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(-1,yval,legend=c('Early RDM - Aligned', 'Late RDM - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(-1,yval,legend=c('Early MIR - Aligned', 'Late MIR - Aligned'),
             col=c(colourscheme[['early']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'lrp', freqs, roi) {
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'alpha') {
    svglite(file=sprintf('doc/fig/Fig21A_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig21B_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'theta') {
    svglite(file=sprintf('doc/fig/Fig21C_TFR_DiffWaves_EarlyLate_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(roi == 'medfro'){
    reg <- 'Medial frontal'
  } else if (roi == 'latcen'){
    reg <- 'Lateral central'
  }
  plot(NA, NA, xlim = c(-1.1, 1.20), ylim = c(-yval - 10, yval + 10), 
       xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("%s %s\nLate-Early difference: go signal onset", reg, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  if (freqs == 'alpha'){
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
  } else if (freqs == 'beta'){
    axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
  } else if (freqs == 'theta'){
    axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
  }
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_EvL_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    
    groupconfidence <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_EvL_CI_%s_%s_%s.csv', roi, freqs, group, erps))
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(0, lim[3]-1, 0.5, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  # #add legend
  # legend(-1,yval,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add movement onset
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
  
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- yval - (.13*yval)
  scal2 <- yval - (.26*yval)
  
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(scal, scal), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = scal, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(scal2, scal2), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = scal2, pch = 20, cex = 1.5, col=col)
  
  #add in permutation clusters and any significant results
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- 0.13*yval
  scal2 <- 0.26*yval
  mult <- 0.05 #multiplier to keep size consistent
  
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_PerturbTypeComp_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(.55, -yval, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(.55, -yval + scal, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(.55, -yval + scal2, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotGoOnsetAllEarlyLateTFRs <- function(frequencies = c('theta', 'alpha', 'beta'), roi='medfro'){
  for (freqs in frequencies){
    plotGoOnsetPermTestEarlyLateTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotGoOnsetPermTestEarlyLateDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
  }
}

# TFR SMALL VS LARGE: Confidence intervals for group data----

#roi = 'medfro' or 'latcen'
#erps = 'frn' or 'lrp'
getAllSmallLargeTFRCIs <- function(frequencies = c('theta', 'alpha', 'beta'), roi, erps){
  for (freqs in frequencies){
    getSmallLargeTFRCI(freqs=freqs, roi=roi, erps=erps)
    getDiffWavesSmallLargeTFRCI(freqs=freqs, roi=roi, erps=erps)
    getPTypeDiffWavesSmallLargeTFRCI(freqs=freqs, roi=roi, erps=erps)
  }
}


getSmallLargeTFRCI <- function(groups = c('smalllarge_aligned', 'smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

getDiffWavesSmallLargeTFRCI <- function(groups = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_DiffWaves_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

getPTypeDiffWavesSmallLargeTFRCI <- function(groups = c('rot', 'rdm', 'mir'), type = 'b', erps = 'frn', freqs, roi){
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_%s_%s_%s.csv', roi, freqs, group, erps))
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
      
      write.csv(confidence, file=sprintf('data/TFR_%s_DiffWaves_SvL_CI_%s_%s_%s.csv', roi, freqs, group, erps), row.names = F) 
      
    }
  }
}

# TFR Permutation tests (Small vs Large): Feedback onset----
plotPermTestSmallLargeTFRs <- function(perturbs = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'frn', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs=='alpha') {
      svglite(file=sprintf('doc/fig/Fig22A_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='beta') {
      svglite(file=sprintf('doc/fig/Fig22B_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='theta') {
      svglite(file=sprintf('doc/fig/Fig22C_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'smallrot'){
      groups <- c('smalllarge_aligned', 'smallrot')
    } else if (ptype == 'largerot'){
      groups <- c('smalllarge_aligned', 'largerot')
    } else if (ptype == 'smallrdm'){
      groups <- c('smalllarge_aligned', 'smallrdm')
    } else if (ptype == 'largerdm'){
      groups <- c('smalllarge_aligned', 'largerdm')
    } else if (ptype == 'smallmir'){
      groups <- c('smalllarge_aligned', 'smallmir')
    } else if (ptype == 'largemir'){
      groups <- c('smalllarge_aligned', 'largemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-yval -10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s power time-locked to feedback onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      # full_timepts <- data$time
      # timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
      timepts <- data$time
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/TFR_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
        err <- 'sml'
      } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
        err <- 'lrg'
      } else if (group == 'smalllarge_aligned'){
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
      } else if (group == 'smalllarge_aligned'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset 
    if(ptype == 'smallrot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['sml']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval - 10, yval - 10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['sml']][['S']]
      points(x = mo_rot[,2], y = yval - 10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'largerot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'smallrdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['sml']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-10,yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['sml']][['S']]
      points(x = mo_rdm[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'largerdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'smallmir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['sml']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['sml']][['S']]
      points(x = mo_mir[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'largemir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval,yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-10, yval-10), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = yval-10, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_ErrorSize_vsAligned_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s', freqs, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'smallrot'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrot') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerot'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerot') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallrdm'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrdm') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerdm'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerdm') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallmir'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallmir') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largemir'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largemir') {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'smallrot'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Small ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerot'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallrdm'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Small RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerdm'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallmir'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Small MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largemir'){
      #add legend
      legend(0.8,yval,legend=c('Aligned','Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}


plotPermTestSmallLargeDiffWavesTFRs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'frn', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs == 'alpha') {
      svglite(file=sprintf('doc/fig/Fig23A_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'beta') {
      svglite(file=sprintf('doc/fig/Fig23B_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'theta') {
      svglite(file=sprintf('doc/fig/Fig23C_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('smallrot', 'largerot')
    } else if (ptype == 'rdm'){
      groups = c('smallrdm', 'largerdm')
    } else if (ptype == 'mir'){
      groups = c('smallmir', 'largemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-0.35, 1.1), ylim = c(-yval - 10, yval +10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s time-locked to feedback onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
      timepts <- data$time
      
      groupconfidence <- read.csv(file=sprintf('data/TFR_DiffWaves_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
        err <- 'sml'
      } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
        err <- 'lrg'
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
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset 
    
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = yval, pch = 20, cex = 1.5, col=col)
    }
    
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_SmallvsLarge_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['lrg']][['T']]
        } else {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(0.8,yval,legend=c('Small ROT - Aligned', 'Large ROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(0.8,yval,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(0.8,yval,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotPermTestPTypeSmallLargeDiffWavesTFRs <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'frn', freqs, roi) {
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'alpha') {
    svglite(file=sprintf('doc/fig/Fig24A_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig24B_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'theta') {
    svglite(file=sprintf('doc/fig/Fig24C_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(roi == 'medfro'){
    reg <- 'Medial frontal'
  } else if (roi == 'latcen'){
    reg <- 'Lateral central'
  }
  if(erps == 'frn'){
    plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-yval - 10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("%s %s\nLarge-Small difference: feedback onset", reg, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1)) #tick marks for x axis
  if (freqs == 'alpha'){
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
  } else if (freqs == 'beta'){
    axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
  } else if (freqs == 'theta'){
    axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
  }
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    
    groupconfidence <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_CI_%s_%s_%s.csv', roi, freqs, group, erps))
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(-0.25, lim[3]-1, 0, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  #add movement onset 
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir.csv')
  
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- yval - (.13*yval)
  scal2 <- yval - (.26*yval)
  
  
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(scal, scal), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = scal, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(scal2, scal2), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = scal2, pch = 20, cex = 1.5, col=col)
  
  
  
  # #add legend
  # legend(.8,yval,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add in permutation clusters and any significant results
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- 0.13*yval
  scal2 <- 0.26*yval
  mult <- 0.05 #multiplier to keep size consistent
  
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_ErrorSize_PerturbTypeComp_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1.05, -yval, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(1.05, -yval + scal, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(1.05, -yval + scal2, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAllSmallLargeTFRs <- function(frequencies = c('theta', 'alpha', 'beta'), roi='medfro'){
  for (freqs in frequencies){
    plotPermTestSmallLargeTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotPermTestSmallLargeDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotPermTestPTypeSmallLargeDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
  }
}

# TFR Permutation tests (Small vs Large): GO onset----
plotGoOnsetPermTestSmallLargeTFRs <- function(perturbs = c('smallrot', 'largerot', 'smallrdm', 'largerdm', 'smallmir', 'largemir'), target='inline', erps = 'lrp', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs=='alpha') {
      svglite(file=sprintf('doc/fig/Fig25A_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='beta') {
      svglite(file=sprintf('doc/fig/Fig25B_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs=='theta') {
      svglite(file=sprintf('doc/fig/Fig25C_TFR_SmallLarge_PermTest_%s_%s_%s.svg', roi, freqs, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'smallrot'){
      groups <- c('smalllarge_aligned', 'smallrot')
    } else if (ptype == 'largerot'){
      groups <- c('smalllarge_aligned', 'largerot')
    } else if (ptype == 'smallrdm'){
      groups <- c('smalllarge_aligned', 'smallrdm')
    } else if (ptype == 'largerdm'){
      groups <- c('smalllarge_aligned', 'largerdm')
    } else if (ptype == 'smallmir'){
      groups <- c('smalllarge_aligned', 'smallmir')
    } else if (ptype == 'largemir'){
      groups <- c('smalllarge_aligned', 'largemir')
    }
    
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval -10, yval + 10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s power time-locked to go signal onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      # full_timepts <- data$time
      # timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
      timepts <- data$time
      
      #read in CI files created
      groupconfidence <- read.csv(file=sprintf('data/TFR_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
        err <- 'sml'
      } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
        err <- 'lrg'
      } else if (group == 'smalllarge_aligned'){
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
      } else if (group == 'smalllarge_aligned'){
        err <- 'aligned'
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'smallrot' | ptype == 'largerot'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'smallmir' | ptype == 'largemir'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'smallrdm' | ptype == 'largerdm'){
      mo_aln <- read.csv(file='data/MovementOnset_CI_aln_lrp.csv')
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['aligned']][['T']]
      lines(x = c(mo_aln[,1], mo_aln[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['aligned']][['S']]
      points(x = mo_aln[,2], y = yval, pch = 20, cex = 1.5, col=col)
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval-20, yval-20), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = yval-20, pch = 20, cex = 1.5, col=col)
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_ErrorSize_vsAligned_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s', freqs, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'smallrot'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrot') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerot'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerot') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallrdm'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallrdm') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largerdm'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largerdm') {
          col <- colourscheme[['lrg']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'smallmir'){
          col <- colourscheme[['sml']][['T']]
        } else if (p_clust < 0.05 & ptype == 'smallmir') {
          col <- colourscheme[['sml']][['S']]
        } else if(p_clust >= 0.05 & ptype == 'largemir'){
          col <- colourscheme[['lrg']][['T']]
        } else if (p_clust < 0.05 & ptype == 'largemir') {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'smallrot'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Small ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerot'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Large ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallrdm'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Small RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largerdm'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Large RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'smallmir'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Small MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['sml']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'largemir'){
      #add legend
      legend(-1,yval,legend=c('Aligned','Large MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotGoOnsetPermTestSmallLargeDiffWavesTFRs <- function(perturbs = c('rot', 'rdm', 'mir'), target='inline', erps = 'lrp', freqs, roi) {
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & freqs == 'alpha') {
      svglite(file=sprintf('doc/fig/Fig26A_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'beta') {
      svglite(file=sprintf('doc/fig/Fig26B_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    } else if (target=='svg' & freqs == 'theta') {
      svglite(file=sprintf('doc/fig/Fig26C_TFR_DiffWaves_SmallLarge_PermTest_%s_%s_%s.svg', freqs, roi, ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'rot'){
      groups = c('smallrot', 'largerot')
    } else if (ptype == 'rdm'){
      groups = c('smallrdm', 'largerdm')
    } else if (ptype == 'mir'){
      groups = c('smallmir', 'largemir')
    }
    
    # create plot
    meanGroupReaches <- list() #empty list so that it plots the means last
    #NA to create empty plot
    # could maybe use plot.new() ?
    
    plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval - 10, yval +10), 
         xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Mean %s %s time-locked to go signal onset: %s", roi, freqs, ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    
    abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
    axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
    if (freqs == 'alpha'){
      axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
    } else if (freqs == 'beta'){
      axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
    } else if (freqs == 'theta'){
      axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
    }
    
    for (group in groups){
      data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_%s_%s_%s.csv', roi, freqs, group, erps))
      timepts <- data$time
      
      groupconfidence <- read.csv(file=sprintf('data/TFR_DiffWaves_SmallLarge_CI_%s_%s_%s_%s.csv', roi, freqs, group, erps))
      
      if(group == 'smallrot'|group == 'smallrdm'|group == 'smallmir'){
        err <- 'sml'
      } else if (group == 'largerot'|group == 'largerdm'|group == 'largemir'){
        err <- 'lrg'
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
      }
      # plot mean reaches for each group
      col <- colourscheme[[err]][['S']]
      #lines(x = timepts, y = mid, col=col)
      lines(x = timepts, y = meanGroupReaches[[group]], col = col, lty = 1, lwd = 2)
    }
    
    #add movement onset
    if(ptype == 'rot'){
      mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'mir'){
      mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_mir[,2], y = yval, pch = 20, cex = 1.5, col=col)
    } else if (ptype == 'rdm'){
      mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
      
      col <- colourscheme[['lrg']][['T']]
      lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
      col <- colourscheme[['lrg']][['S']]
      points(x = mo_rdm[,2], y = yval, pch = 20, cex = 1.5, col=col)
    }
    
    #add in permutation clusters and any significant results
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_SmallvsLarge_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05){
          col <- colourscheme[['lrg']][['T']]
        } else {
          col <- colourscheme[['lrg']][['S']]
        }
        #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
        mult <- 0.05
        lower <- c(rep(-yval, length(permtime)))
        upper <- c(rep(-yval + (yval*mult), length(permtime)))
        polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
      }
    }
    
    if(ptype == 'rot'){
      #add legend
      legend(-1,yval,legend=c('Small ROT - Aligned', 'LargeROT - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'rdm'){
      #add legend
      legend(-1,yval,legend=c('Small RDM - Aligned', 'Large RDM - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'mir'){
      #add legend
      legend(-1,yval,legend=c('Small MIR - Aligned', 'Large MIR - Aligned'),
             col=c(colourscheme[['sml']][['S']],colourscheme[['lrg']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}

plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs <- function(groups = c('rot', 'rdm', 'mir'), perturbs = c('rotvmir', 'rotvrdm', 'mirvrdm'), target='inline', erps = 'lrp', freqs, roi) {
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'alpha') {
    svglite(file=sprintf('doc/fig/Fig27A_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig27B_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  } else if (target=='svg' & freqs == 'theta') {
    svglite(file=sprintf('doc/fig/Fig27C_TFR_DiffWaves_SmallLarge_PermTest_PTypeDiff_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  if (freqs == 'alpha'){
    yval <- 200
  } else if (freqs == 'beta'){
    yval <- 100
  } else if (freqs == 'theta'){
    yval <- 300
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  if(roi == 'medfro'){
    reg <- 'Medial frontal'
  } else if (roi == 'latcen'){
    reg <- 'Lateral central'
  }
  
  plot(NA, NA, xlim = c(-1.1, 1.20), ylim = c(-yval - 10, yval + 10), 
       xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("%s %s\nLarge-Small difference: go signal onset", reg, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  if (freqs == 'alpha'){
    axis(2, at = c(-200, -150, -100, -50, 0, 50, 100, 150, 200), las=2) #tick marks for y axis
  } else if (freqs == 'beta'){
    axis(2, at = c(-100, -50, 0, 50, 100), las=2) #tick marks for y axis
  } else if (freqs == 'theta'){
    axis(2, at = c(-300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), las=2) #tick marks for y axis
  }
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    
    groupconfidence <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_CI_%s_%s_%s.csv', roi, freqs, group, erps))
    
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
  
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(0, lim[3]-1, 0.5, lim[4]+1, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  # #add legend
  # legend(-1,yval,legend=c('Rot', 'Rdm', 'Mir'),
  #        col=c(colourscheme[['rot']][['S']],colourscheme[['rdm']][['S']],colourscheme[['mir']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #add movement onset 
  mo_rot <- read.csv(file='data/MovementOnset_CI_rot_lrp.csv')
  mo_rdm <- read.csv(file='data/MovementOnset_CI_rdm_lrp.csv')
  mo_mir <- read.csv(file='data/MovementOnset_CI_mir_lrp.csv')
  
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- yval - (.13*yval)
  scal2 <- yval - (.26*yval)
  
  
  col <- colourscheme[['rot']][['T']]
  lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(yval, yval), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rot']][['S']]
  points(x = mo_rot[,2], y = yval, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['rdm']][['T']]
  lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(scal, scal), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['rdm']][['S']]
  points(x = mo_rdm[,2], y = scal, pch = 20, cex = 1.5, col=col)
  
  col <- colourscheme[['mir']][['T']]
  lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(scal2, scal2), col = col, lty = 1, lwd = 8)
  col <- colourscheme[['mir']][['S']]
  points(x = mo_mir[,2], y = scal2, pch = 20, cex = 1.5, col=col)
  
  #add in permutation clusters and any significant results
  #scal is 13% of yval, scal2 is 26% of yval
  scal <- 0.13*yval
  scal2 <- 0.26*yval
  mult <- 0.05 #multiplier to keep size consistent
  
  for(ptype in perturbs){
    colourscheme <- getPermTestColourScheme()
    permdat <- read.csv(file=sprintf('data/TFR_Permutation_test_ErrorSize_PerturbTypeComp_%s_%s.csv', erps, roi))
    cond <- sprintf('%s_%s_%s', freqs, roi, ptype)
    subdat <- permdat[which(permdat$condition == cond),]
    for(i in c(1:nrow(subdat))){
      start <- subdat$clust_idx_start[i] + 1
      end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
      
      if(is.na(start) & is.na(end)){
        next
      } else {
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(-yval, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval, length(permtime)))
          upper <- c(rep(-yval + (yval*mult), length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          #lines(x = c(permtime), y = c(rep(scal2, length(permtime))), col = col, lty = 1, lwd = 8)
          lower <- c(rep(-yval + scal2, length(permtime)))
          upper <- c(rep((-yval + (yval*mult)) + scal2, length(permtime)))
          polygon(x = c(permtime, rev(permtime)), y = c(lower, rev(upper)), border=NA, col=col)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(.55, -yval, 'Fixed rotation vs Mirror reversal', col = col, adj=c(0,0))
  text(.55, -yval + scal, 'Fixed rotation vs Random rotation', col = col, adj=c(0,0))
  text(.55, -yval + scal2, 'Mirror reversal vs Random rotation', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotGoOnsetAllSmallLargeTFRs <- function(frequencies = c('theta', 'alpha', 'beta'), roi='medfro'){
  for (freqs in frequencies){
    plotGoOnsetPermTestSmallLargeTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotGoOnsetPermTestSmallLargeDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
    plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(target = 'svg', freqs = freqs, roi = roi)
  }
}

# functions to check data----
getOutlierSignal <- function(target= 'inline', groups = c('rot'), roi = 'latcen', freqs = 'beta', erps='lrp'){
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig28_TFR_SmallLarge_GoSignal_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  yval <- 2000
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  
  plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval - 10, yval + 10), 
       xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Mean %s %s time-locked to go signal onset", roi, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  axis(2, at = c(-1000, -800, -600, -300, -200, -100, -50, 0, 50, 100, 200, 300, 600, 800, 1000), las=2) #tick marks for y axis
  
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_%s_DiffWaves_SvL_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    subdata <- as.matrix(data[,2:(dim(data)[2]-1)])
    for (i in c(1:ncol(subdata))){
      colourscheme <- getPTypeDiffWavesColourScheme(groups = group)
      col <- colourscheme[[group]][['T']] #use colour scheme according to group
      if(i == 18){ #participant ID 17
        col <- colourscheme[[group]][['S']]
      }
      lines(x = timepts, y = subdata[,i], col = col, lty = 1, lwd = 2)
    }
    col <- colourscheme[[group]][['S']]
    text(.05, -yval, 'Outlier Participant ID# 17', col = col, adj=c(0,0))
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
}

getOutlierTrialSignal<- function(target= 'inline', groups = c('smallrot'), roi = 'latcen', freqs = 'beta', erps='lrp'){
  
  #but we can save plot as svg file
  if (target=='svg' & freqs == 'beta') {
    svglite(file=sprintf('doc/fig/Fig28A_TFR_SmallLarge_GoSignal_TrialPower_%s_%s.svg', roi, freqs), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  yval <- 2000
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  #NA to create empty plot
  # could maybe use plot.new() ?
  
  plot(NA, NA, xlim = c(-1.1, 0.5), ylim = c(-yval - 10, yval + 10), 
       xlab = "Time (s)", ylab = "Power (µV²)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Mean %s %s time-locked to go signal onset", roi, freqs), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  
  abline(h = c(0), v = c(-1, 0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-1, -0.5, -0.25, 0, 0.25, 0.5)) #tick marks for x axis
  axis(2, at = c(-1000, -800, -600, -300, -200, -100, -50, 0, 50, 100, 200, 300, 600, 800, 1000), las=2) #tick marks for y axis
  
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/TFR_trialpower_%s_%s_%s_%s.csv', roi, freqs, group, erps))
    timepts <- data$time
    subdata <- as.matrix(data[,2:(dim(data)[2]-1)])
    for (i in c(1:ncol(subdata))){
      err <- 'sml'
      colourscheme <- getErrSizeColourScheme(err=err)
      col <- colourscheme[[err]][['T']] #use colour scheme according to group
      if(i == 3){ #trial number 2
        col <- colourscheme[['lrg']][['S']]
      }
      lines(x = timepts, y = subdata[,i], col = col, lty = 1, lwd = 2)
    }
    col <- colourscheme[['lrg']][['S']]
    text(.25, -yval, 'Outlier Trial # 2', col = col, adj=c(0,0))
    
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
  
}

#Plotting summary plots----
plotERPResults <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig35_ERP_Results.svg', width=10, height=8, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,2,3,3,4,4), 2, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1,1))
  
  # # # # # # # # # #
  # panel A: PTYPE DIFFWAVES LRP - Early vs Late
  plotPermTestPTypeEarlyLateLRPDiffWaves()
  mtext('a', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: PTYPE DIFFWAVES FRN - Early vs Late
  plotPermTestPTypeEarlyLateDiffWaves()
  mtext('b', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  # # # # # # # # # #
  # panel C: PTYPE DIFFWAVES LRP - Small vs Large
  plotPermTestPTypeSmallLargeLRPDiffWaves()
  mtext('c', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: PTYPE DIFFWAVES FRN - Small vs Large
  plotPermTestPTypeSmallLargeDiffWaves()
  mtext('d', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotTFRThetaResults <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig36_TFR_Theta_Results.svg', width=10, height=11, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,2,
                  3,3,4,4,
                  5,5,6,6,
                  7,7,8,8),
                4, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1.5,1.5,1.5,1.5))
  
  # # # # # # # # # #
  # panel A: TFR Go onset, theta, medfro - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'theta', roi = 'medfro')
  mtext('a', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: TFR feedback onset, theta, medfro - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'theta', roi = 'medfro')
  mtext('b', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: TFR Go onset, theta, latcen - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'theta', roi = 'latcen')
  mtext('c', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: TFR feedback onset, theta, latcen - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'theta', roi = 'latcen')
  mtext('d', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel E: TFR Go onset, theta, medfro - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'theta', roi = 'medfro')
  mtext('e', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel F: TFR feedback onset, theta, medfro - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'theta', roi = 'medfro')
  mtext('f', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel G: TFR Go onset, theta, latcen - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'theta', roi = 'latcen')
  mtext('g', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel H: TFR feedback onset, theta, latcen - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'theta', roi = 'latcen')
  mtext('h', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotTFRAlphaResults <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig37_TFR_Alpha_Results.svg', width=10, height=11, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,2,
                  3,3,4,4,
                  5,5,6,6,
                  7,7,8,8),
                4, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1.5,1.5,1.5,1.5))
  
  # # # # # # # # # #
  # panel A: TFR Go onset, alpha, medfro - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'alpha', roi = 'medfro')
  mtext('a', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: TFR feedback onset, alpha, medfro - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'alpha', roi = 'medfro')
  mtext('b', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: TFR Go onset, alpha, latcen - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'alpha', roi = 'latcen')
  mtext('c', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: TFR feedback onset, alpha, latcen - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'alpha', roi = 'latcen')
  mtext('d', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel E: TFR Go onset, alpha, medfro - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'alpha', roi = 'medfro')
  mtext('e', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel F: TFR feedback onset, alpha, medfro - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'alpha', roi = 'medfro')
  mtext('f', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel G: TFR Go onset, alpha, latcen - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'alpha', roi = 'latcen')
  mtext('g', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel H: TFR feedback onset, alpha, latcen - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'alpha', roi = 'latcen')
  mtext('h', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotTFRBetaResults <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig38_TFR_Beta_Results.svg', width=10, height=11, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,1,2,2,
                  3,3,4,4,
                  5,5,6,6,
                  7,7,8,8),
                4, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1.5,1.5,1.5,1.5))
  
  # # # # # # # # # #
  # panel A: TFR Go onset, beta, medfro - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'beta', roi = 'medfro')
  mtext('a', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: TFR feedback onset, beta, medfro - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'beta', roi = 'medfro')
  mtext('b', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: TFR Go onset, beta, latcen - Early vs Late
  plotGoOnsetPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'beta', roi = 'latcen')
  mtext('c', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: TFR feedback onset, beta, latcen - Early vs Late
  plotPermTestPTypeEarlyLateDiffWavesTFRs(freqs = 'beta', roi = 'latcen')
  mtext('d', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel E: TFR Go onset, beta, medfro - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'beta', roi = 'medfro')
  mtext('e', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel F: TFR feedback onset, beta, medfro - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'beta', roi = 'medfro')
  mtext('f', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel G: TFR Go onset, beta, latcen - Small vs Large
  plotGoOnsetPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'beta', roi = 'latcen')
  mtext('g', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel H: TFR feedback onset, beta, latcen - Small vs Large
  plotPermTestPTypeSmallLargeDiffWavesTFRs(freqs = 'beta', roi = 'latcen')
  mtext('h', side=3, outer=F, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


# functions to read in TFR plots-----

# getTFRplots <- function(roi = 'medfro', erps = 'frn'){
#   
#   img <- image_read_svg(sprintf('doc/fig/tfr_mne/%s/%s_early_late_aligned_power-tfr.svg', erps, roi))
#   print(img)
#   img <- image_read_svg(sprintf('doc/fig/tfr_mne/%s/%s_early_rot_power-tfr.svg', erps, roi))
#   print(img)
#   img <- image_read_svg(sprintf('doc/fig/tfr_mne/%s/%s_late_rot_power-tfr.svg', erps, roi))
#   print(img)
#    
#   
# }














#example using permutation tests
# set.seed(999)
# g1 <- sample(1:100, 30, replace = F)
# g2 <- sample(50:150, 30, replace = F)
# 
# 
# hist(g1)
# hist(g2)
# 
# 
# perm_out <- twoSamplePermutationTestLocation(
#   x = g1,
#   y = g2,
#   fcn = 'mean',
#   paired = T,
#   n.permutations = 1000,
#   seed = 999
# )
# 
# perm_out
# 
# diffs <- g1-g2
# perm_out <- oneSamplePermutationTest(
#   x = diffs,
#   alternative = 'two.sided',
#   n.permutations = 1000,
#   seed = 999
# )
# 
# perm_out
# plot(perm_out)

# getIndexAroundPeak <- function(group, erps = 'frn', starttime = -0.5, endtime = 0.5){
#   
#   data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
#   data <- subset(data, subset = time >= starttime & time <= endtime)
#   data <- as.matrix(data[,2:(dim(data)[2]-1)])
#   
#   data <- as.data.frame(data)
# 
#   return(data)
# 
# }
# 
# testPeakAmplitude <- function(g1, g2){
#   
#   group1 <- getIndexAroundPeak(group = g1)
#   ppidx <- ncol(group1)
#   peaks1 <- c()
#   
#   for(pp in c(1:ppidx)){
#     subdat1 <- as.numeric(group1[,pp])
#     peakneg <- min(subdat1)
#     peaks1 <- c(peaks1, peakneg)
#   }
#   
#   group2 <- getIndexAroundPeak(group = g2)
#   ppidx <- ncol(group2)
#   peaks2 <- c()
#   
#   for(pp in c(1:ppidx)){
#     subdat1 <- as.numeric(group2[,pp])
#     peakneg <- min(subdat1)
#     peaks2 <- c(peaks2, peakneg)
#   }
#   
#   cat(sprintf('Bayesian t-test %s vs. %s peak negativity:\n', g1, g2))
#   print(ttestBF(peaks1, peaks2, paired = TRUE))
# }

# testPermTtest <- function(){
#   
#   group1 <- getIndexAroundPeak(group = 'aln')
#   group2 <- getIndexAroundPeak(group = 'earlyrot')
#   
#   nidx <- nrow(group1)
#   pvals <- c()
#   for (i in c(1:nidx)){
#     subdat1 <- as.numeric(group1[i,])
#     subdat2 <- as.numeric(group2[i,])
#     
#     perm_out <- twoSamplePermutationTestLocation(
#       x = subdat1,
#       y = subdat2,
#       fcn = 'mean',
#       paired = T,
#       n.permutations = 1000,
#       seed = 999
#     )
#     
#     pval <- perm_out$p.value
#     pvals <- c(pvals, pval)
#   }
#   
#   padj <- p.adjust(pvals, method = 'fdr')
#   
# }
#  does not survive pval adjustment


