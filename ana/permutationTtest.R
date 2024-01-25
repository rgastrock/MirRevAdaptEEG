library(EnvStats)
library(coin)


set.seed(999)
g1 <- sample(1:100, 30, replace = F)
g2 <- sample(50:150, 30, replace = F)


hist(g1)
hist(g2)


perm_out <- twoSamplePermutationTestLocation(
  x = g1,
  y = g2,
  fcn = 'mean',
  paired = T,
  n.permutations = 1000,
  seed = 999
)

perm_out

diffs <- g1-g2
perm_out <- oneSamplePermutationTest(
  x = diffs,
  alternative = 'two.sided',
  n.permutations = 1000,
  seed = 999
)

perm_out
plot(perm_out)
