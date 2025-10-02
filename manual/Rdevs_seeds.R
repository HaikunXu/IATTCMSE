# simulate and save recruitment devs for 100 iterations and 200 quarters
set.seed(123)
seeds <- sample(1:1e3, size = 100, replace = FALSE)  # Sample 5 elements without replacement
write.csv(seeds, file = paste0(pdir,"seeds.csv"), row.names = FALSE)

R_devs <- matrix(NA, nrow = 200, ncol = 100)
for (i in 1:100) {
  set.seed(seeds[i])
  R_devs[,i] <- rnorm(n = 200, mean = 0, sd = 0.6) - 0.6 ^ 2 / 2
}

write.csv(R_devs, file = paste0(pdir,"R_devs.csv"), row.names = FALSE)