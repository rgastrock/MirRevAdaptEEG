source('ana/shared.R')

# download data from OSF----
getBehaviorData <- function(){
  
  Reach::downloadOSFdata(repository = 'q5n6j',
                         filelist = list('data/' =c('behavior.zip')),
                         folder = 'data/',
                         overwrite = TRUE,
                         unzip = TRUE,
                         removezips = TRUE)
}

getEEGData <- function(){
  
  Reach::downloadOSFdata(repository = 'q5n6j',
                         filelist = list('data/eeg/' =c('p000.zip', 'p001.zip', 'p002.zip', 'p003.zip', 'p004.zip', 'p005.zip', 'p006.zip', 'p007.zip',
                                                        'p008.zip', 'p009.zip', 'p010.zip', 'p011.zip', 'p012.zip', 'p013.zip', 'p014.zip', 'p015.zip',
                                                        'p016.zip', 'p017.zip', 'p018.zip', 'p019.zip', 'p020.zip', 'p021.zip', 'p022.zip', 'p023.zip',
                                                        'p024.zip', 'p025.zip', 'p026.zip', 'p027.zip', 'p028.zip', 'p029.zip', 'p030.zip', 'p031.zip',
                                                        'tfr_ana.zip')),
                         folder = 'data/eeg/',
                         overwrite = TRUE,
                         unzip = TRUE,
                         removezips = TRUE)
}

getFamiliarizationEEGData <- function(){
  
  Reach::downloadOSFdata(repository = 'q5n6j',
                         filelist = list('data/eeg/' =c('familiarization.zip')),
                         folder = 'data/test/',
                         overwrite = TRUE,
                         unzip = TRUE,
                         removezips = TRUE)
}