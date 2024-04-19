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

getIndexAroundPeak <- function(group, erps = 'frn', starttime = -0.5, endtime = 0.5){
  
  data <- read.csv(file=sprintf('data/Evoked_DF_%s_%s.csv', group, erps))
  data <- subset(data, subset = time >= starttime & time <= endtime)
  data <- as.matrix(data[,2:(dim(data)[2]-1)])
  
  data <- as.data.frame(data)

  return(data)

}

testPeakAmplitude <- function(g1, g2){
  
  group1 <- getIndexAroundPeak(group = g1)
  ppidx <- ncol(group1)
  peaks1 <- c()
  
  for(pp in c(1:ppidx)){
    subdat1 <- as.numeric(group1[,pp])
    peakneg <- min(subdat1)
    peaks1 <- c(peaks1, peakneg)
  }
  
  group2 <- getIndexAroundPeak(group = g2)
  ppidx <- ncol(group2)
  peaks2 <- c()
  
  for(pp in c(1:ppidx)){
    subdat1 <- as.numeric(group2[,pp])
    peakneg <- min(subdat1)
    peaks2 <- c(peaks2, peakneg)
  }
  
  cat(sprintf('Bayesian t-test %s vs. %s peak negativity:\n', g1, g2))
  print(ttestBF(peaks1, peaks2, paired = TRUE))
}

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


