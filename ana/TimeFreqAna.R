source('ana/shared.R')

# define ranges of frequencies (as in Gentili et al., 2015)
# low_alpha = 8-10 Hz
# high_alpha = 11-13 Hz
# low_beta = 14-22 Hz
# high_beta = 23-35 Hz

getEarlyLateAverageFreqBands <- function(group, band, erps){
  
  avgs <- c()
  
  data <- read.csv(file=sprintf('data/EL_%s_power_%s.csv', group, erps))
  freqs <- unique(data$freq)
  time <- unique(data$time)
  
  if(band == 'low_alpha'){
    subfreqs <- freqs[between(freqs, 8, 11)] #exceed limit by 1 to get values just above the limit
  } else if (band == 'high_alpha'){
    subfreqs <- freqs[between(freqs, 11, 14)]
  } else if (band == 'low_beta'){
    subfreqs <- freqs[between(freqs, 14, 23)]
  } else if (band == 'high_beta'){
    subfreqs <- freqs[between(freqs, 23, 35)]
  }
  
  subdata <- data[which(data$freq %in% subfreqs),]
  df <- data.frame()
  for (f in subfreqs){
    subdat <- subdata[which(subdata$freq == f),]
    ndat <- subdat[,4:ncol(subdat)]
    ndat <- as.numeric(rowMeans(ndat)) #get Mean across channels
    
    #combine into data frame with freqs as columns
    if (prod(dim(df)) == 0){
      df <- ndat
    } else {
      df <- cbind(df, ndat)
    }
  }
  
  df <- as.numeric(rowMeans(df))
  
  outputdata <- data.frame(time, df)
  return(outputdata)
}

plotEarlyLateAverageFreqBands <- function(target='inline', bands = c('low_alpha', 'high_alpha', 'low_beta', 'high_beta'), groups = c('rot', 'mir', 'rdm'), erps){
  
  for(band in bands){
    
    # create plot
    #NA to create empty plot
    if (erps == 'lrp'){
      
      if (target=='svg') {
        svglite(file=sprintf('doc/fig/Fig15_AvgTFR_%s_%s.svg', band, erps), width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      plot(NA, NA, xlim = c(-1.51, 0.01), ylim = c(-0.06,0.06), 
           xlab = "Time (s)", ylab = "Power", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Average Time-Frequency time-locked to go signal onset: %s", band), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1.0, -0.5, -0.25, 0)) #tick marks for x axis
      axis(2, at = c(-0.05, -0.025, 0, 0.025, 0.05), las=2) #tick marks for y axis
    } else if (erps == 'frn'){
      
      if (target=='svg') {
        svglite(file=sprintf('doc/fig/Fig15_AvgTFR_%s_%s.svg', band, erps), width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      plot(NA, NA, xlim = c(-0.01, 1.51), ylim = c(-0.06,0.06), 
           xlab = "Time (s)", ylab = "Power", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Average Time-Frequency time-locked to feedback onset: %s", band), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 0.25, 0.5, 1.0, 1.5)) #tick marks for x axis
      axis(2, at = c(-0.05, -0.025, 0, 0.025, 0.05), las=2) #tick marks for y axis
    }

    for(group in groups){
      data <- getEarlyLateAverageFreqBands(group = group, band = band, erps = erps)
      timepts <- data$time
      if(erps == 'lrp'){
        timepts <- timepts[101:401] #-1.5 sec to 0
      } else if (erps == 'frn'){
        timepts <- timepts[401:701] #0 to 1.5 sec
      }
      
      
      
      colourscheme <- getPerturbationColourScheme(p = group)
      subdat <- data[which(data$time %in% timepts),]
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(x = timepts, y = subdat$df, col = col, lty = 1, lwd = 2)
    }
    
    #add legend
    if(erps == 'lrp'){
      legend(-0.5,0.05,legend=c('rotation','mirror', 'random'),
             col=c(colourscheme[['rot']][['S']],colourscheme[['mir']][['S']],colourscheme[['rdm']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (erps == 'frn'){
      legend(1,-0.025,legend=c('rotation','mirror', 'random'),
             col=c(colourscheme[['rot']][['S']],colourscheme[['mir']][['S']],colourscheme[['rdm']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }

    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }

}


getSmallLargeAverageFreqBands <- function(group, band, erps){
  
  avgs <- c()
  
  data <- read.csv(file=sprintf('data/SL_%s_power_%s.csv', group, erps))
  freqs <- unique(data$freq)
  time <- unique(data$time)
  
  if(band == 'low_alpha'){
    subfreqs <- freqs[between(freqs, 8, 11)] #exceed limit by 1 to get values just above the limit
  } else if (band == 'high_alpha'){
    subfreqs <- freqs[between(freqs, 11, 14)]
  } else if (band == 'low_beta'){
    subfreqs <- freqs[between(freqs, 14, 23)]
  } else if (band == 'high_beta'){
    subfreqs <- freqs[between(freqs, 23, 35)]
  }
  
  subdata <- data[which(data$freq %in% subfreqs),]
  df <- data.frame()
  for (f in subfreqs){
    subdat <- subdata[which(subdata$freq == f),]
    ndat <- subdat[,4:ncol(subdat)]
    ndat <- as.numeric(rowMeans(ndat)) #get Mean across channels
    
    #combine into data frame with freqs as columns
    if (prod(dim(df)) == 0){
      df <- ndat
    } else {
      df <- cbind(df, ndat)
    }
  }
  
  df <- as.numeric(rowMeans(df))
  
  outputdata <- data.frame(time, df)
  return(outputdata)
}

plotSmallLargeAverageFreqBands <- function(target='inline', bands = c('low_alpha', 'high_alpha', 'low_beta', 'high_beta'), groups = c('rot', 'mir', 'rdm'), erps){
  
  for(band in bands){
    
    # create plot
    #NA to create empty plot
    if (erps == 'lrp'){
      
      if (target=='svg') {
        svglite(file=sprintf('doc/fig/Fig15A_AvgTFR_SL_%s_%s.svg', band, erps), width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      plot(NA, NA, xlim = c(-1.51, 0.01), ylim = c(-0.06,0.06), 
           xlab = "Time (s)", ylab = "Power", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Average Time-Frequency time-locked to go signal onset: %s", band), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(-1.5, -1.0, -0.5, -0.25, 0)) #tick marks for x axis
      axis(2, at = c(-0.05, -0.025, 0, 0.025, 0.05), las=2) #tick marks for y axis
    } else if (erps == 'frn'){
      
      if (target=='svg') {
        svglite(file=sprintf('doc/fig/Fig15A_AvgTFR_SL_%s_%s.svg', band, erps), width=6, height=6, pointsize=14, system_fonts=list(sans="Arial"))
      }
      
      plot(NA, NA, xlim = c(-0.01, 1.51), ylim = c(-0.06,0.06), 
           xlab = "Time (s)", ylab = "Power", frame.plot = FALSE, #frame.plot takes away borders
           main = sprintf("Average Time-Frequency time-locked to feedback onset: %s", band), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
      
      
      abline(h = c(0), v = c(0), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
      axis(1, at = c(0, 0.25, 0.5, 1.0, 1.5)) #tick marks for x axis
      axis(2, at = c(-0.05, -0.025, 0, 0.025, 0.05), las=2) #tick marks for y axis
    }
    
    for(group in groups){
      data <- getSmallLargeAverageFreqBands(group = group, band = band, erps = erps)
      timepts <- data$time
      if(erps == 'lrp'){
        timepts <- timepts[101:401] #-1.5 sec to 0
      } else if (erps == 'frn'){
        timepts <- timepts[401:701] #0 to 1.5 sec
      }
      
      
      
      colourscheme <- getPerturbationColourScheme(p = group)
      subdat <- data[which(data$time %in% timepts),]
      # plot mean reaches for each group
      col <- colourscheme[[group]][['S']]
      lines(x = timepts, y = subdat$df, col = col, lty = 1, lwd = 2)
    }
    
    #add legend
    if(erps == 'lrp'){
      legend(-0.5,0.05,legend=c('rotation','mirror', 'random'),
             col=c(colourscheme[['rot']][['S']],colourscheme[['mir']][['S']],colourscheme[['rdm']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    } else if (erps == 'frn'){
      legend(1,-0.025,legend=c('rotation','mirror', 'random'),
             col=c(colourscheme[['rot']][['S']],colourscheme[['mir']][['S']],colourscheme[['rdm']][['S']]),
             lty=1,bty='n',cex=1,lwd=2)
    }
    
    
    #close everything if you saved plot as svg
    if (target=='svg') {
      dev.off()
    }
    
  }
  
}

