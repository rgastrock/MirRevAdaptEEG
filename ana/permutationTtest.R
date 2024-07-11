source('ana/shared.R')
source('ana/learningRates.R')

# FRN Permutation tests----
plotPermTestEarlyLateERPs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps = 'frn') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg' & erps=='frn') {
      svglite(file=sprintf('doc/fig/Fig1C_FRN_EarlyLate_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups = c('aln', 'earlyrot')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
            xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
            main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rot[,1], mo_rot[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rot[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
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
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (ptype == 'laterot'){
      groups = c('aln', 'laterot')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'earlyrdm'){
      groups = c('aln', 'earlyrdm')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_rdm[,1], mo_rdm[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_rdm[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
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
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'laterdm'){
      groups = c('aln', 'laterdm')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'earlymir'){
      groups = c('aln', 'earlymir')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
        
        col <- colourscheme[['early']][['T']]
        lines(x = c(mo_mir[,1], mo_mir[,3]), y = c(4.5, 4.5), col = col, lty = 1, lwd = 8)
        col <- colourscheme[['early']][['S']]
        points(x = mo_mir[,2], y = 4.5, pch = 20, cex = 1.5, col=col)
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
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
      #add legend
      legend(0.8,-5,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'latemir'){
      groups = c('aln', 'latemir')
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      
      #NA to create empty plot
      # could maybe use plot.new() ?
      
      plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
           xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("ERP time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      #abline(v = c(0.15, 0.28, 0.5), col = 8, lty = 3) #include P3 in same plot
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        #read in CI files created
        groupconfidence <- read.csv(file=sprintf('data/ERP_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
      }
      
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
      
      # create plot
      meanGroupReaches <- list() #empty list so that it plots the means last
      #NA to create empty plot
      # could maybe use plot.new() ?
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
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
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
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
      if(erps == 'frn'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to feedback onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      } else if (erps == 'ern'){
        plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
             xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
             main = sprintf("Difference Waves time-locked to movement onset: %s", ptype), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      }
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
      axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
      axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
      
      for (group in groups){
        data <- read.csv(file=sprintf('data/DiffWaves_DF_%s_%s.csv', group, erps))
        full_timepts <- data$time
        timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
        
        groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_CI_%s_%s.csv', group, erps))
        groupconfidence <- groupconfidence[351:701,] #grab timepts we need
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_EarlyvsLate_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
        # if(start %in% c(101:701) & end %in% c(101:701)){
        #   permtime <- full_timepts[start:end]
        # } else {
        #   next
        # }
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
    plot(NA, NA, xlim = c(-0.35, 1.6), ylim = c(-16, 6), 
         xlab = "Time (s)", ylab = "µV", frame.plot = FALSE, #frame.plot takes away borders
         main = "Difference Waves time-locked to feedback onset", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  }
  
  abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0
  axis(1, at = c(-0.25, 0, 0.25, 0.5, 1, 1.5)) #tick marks for x axis
  axis(2, at = c(-15, -10, -5, 0, 5), las=2) #tick marks for y axis
  
  for (group in groups){
    data <- read.csv(file=sprintf('data/DiffWaves_DF_EvL_%s_diff_%s.csv', group, erps))
    full_timepts <- data$time
    timepts <- full_timepts[351:701] #remove .5 seconds before and after -1.5 and 1.5
    
    groupconfidence <- read.csv(file=sprintf('data/DiffWaves_EarlyLate_EvL_CI_%s_diff_%s.csv', group, erps))
    groupconfidence <- groupconfidence[351:701,] #grab timepts we need
    
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
  legend(1.11,-5,legend=c('Rot', 'Rdm', 'Mir'),
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
        permtime <- timepts[start:end]
        
        p_clust <- subdat$p_values[i]
        if(p_clust >= 0.05 & ptype == 'rotvmir'){
          col <- colourscheme[['T']]
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        } else if(p_clust >= 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['T']]
          lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
        } else if(p_clust >= 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['T']]
          lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
        } else if(p_clust < 0.05 & ptype == 'rotvmir') {
          col <- colourscheme[['S']]
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        } else if(p_clust < 0.05 & ptype == 'rotvrdm') {
          col <- colourscheme[['S']]
          lines(x = c(permtime), y = c(rep(-13, length(permtime))), col = col, lty = 1, lwd = 8)
        } else if(p_clust < 0.05 & ptype == 'mirvrdm') {
          col <- colourscheme[['S']]
          lines(x = c(permtime), y = c(rep(-11, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
    }
  }
  
  #add permutation results labels
  col <- colourscheme[['S']]
  text(1.25, -15, 'Rot vs Mir', col = col, adj=c(0,0))
  text(1.25, -13, 'Rot vs Rdm', col = col, adj=c(0,0))
  text(1.25, -11, 'Mir vs Rdm', col = col, adj=c(0,0))
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


# LRP Permutation tests----
plotPermTestEarlyLateLRPs <- function(perturbs = c('earlyrot', 'laterot', 'earlyrdm', 'laterdm', 'earlymir', 'latemir'), target='inline', erps='lrp') {
  
  for(ptype in perturbs){
    #but we can save plot as svg file
    if (target=='svg') {
      svglite(file=sprintf('doc/fig/Fig8C_EarlyLate_Subtracted_LRP_PermTest_%s.svg', ptype), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
    }
    
    if(ptype == 'earlyrot'){
      groups = c('alnrot', 'rot_b0')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
      for(i in c(1:nrow(subdat))){
        start <- subdat$clust_idx_start[i] + 1
        end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
        
        if(is.na(start) & is.na(end)){
          next
        } else {
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'laterot'){
      groups = c('alnrot', 'rot_b1')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Late ROT'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'earlyrdm'){
      groups = c('aln', 'rdm_b0')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
      for(i in c(1:nrow(subdat))){
        start <- subdat$clust_idx_start[i] + 1
        end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
        
        if(is.na(start) & is.na(end)){
          next
        } else {
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'laterdm'){
      groups = c('aln', 'rdm_b1')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Late RDM'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'earlymir'){
      groups = c('alnmir', 'mir_b0')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
      for(i in c(1:nrow(subdat))){
        start <- subdat$clust_idx_start[i] + 1
        end <- subdat$clust_idx_end[i] #nothing to add or subtract: due to python indexing and should not include last digit in python sequence
        
        if(is.na(start) & is.na(end)){
          next
        } else {
          permtime <- timepts[start:end]
          
          p_clust <- subdat$p_values[i]
          if(p_clust >= 0.05){
            col <- colourscheme[['early']][['T']]
          } else {
            col <- colourscheme[['early']][['S']]
          }
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Early MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['early']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    } else if (ptype == 'latemir'){
      groups = c('alnmir', 'mir_b1')
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
        full_timepts <- data$time
        timepts <- full_timepts[101:401] #remove .5 seconds before and after -1.5 and 1.5
        
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
      
      #add in permutation clusters and any significant results
      permdat <- read.csv(file=sprintf('data/Permutation_test_vsAligned_%s.csv', erps))
      subdat <- permdat[which(permdat$condition == ptype),]
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
          lines(x = c(permtime), y = c(rep(-15, length(permtime))), col = col, lty = 1, lwd = 8)
        }
      }
      
      #add legend
      legend(-1.5,-5,legend=c('Aligned','Late MIR'),
             col=c(colourscheme[['aligned']][['S']],colourscheme[['late']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
      
    }
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
  }
}


#need difference waves between each condition and aligned, as well as early vs rot
























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


