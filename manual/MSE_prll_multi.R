library(IATTCMSE)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)

#Specify path of parent directory
pdir <- "D:/OneDrive - IATTC/IATTC/2025/MSE/Test/"

#Specify the path of conditioned initial OM
sdir <- "D:/OneDrive - IATTC/IATTC/2025/SAC16/BET F30/"

#Dimensions
# Nfisheries <- 22
niterations <- 5
nyears <- 10
nquarters <- nyears * 4
Mcycle <- 10
nsteps <- nyears / Mcycle
endquarter <- 196
startquarter <- 17
n_extra_R <- 2 #number of assessment period recruitment in the projection

# # simulate and save recruitment devs
# set.seed(123)
# seeds <- sample(1:1e3, size = niterations, replace = FALSE)  # Sample 5 elements without replacement
# write.csv(seeds, file = paste0(pdir,"seeds.csv"), row.names = FALSE)
# 
# R_devs <- matrix(NA, nrow = nquarters, ncol = niterations)
# for (i in 1:niterations) {
#   set.seed(seeds[i])
#   R_devs[,i] <- rnorm(n = nquarters, mean = 0, sd = 0.6) - 0.6 ^ 2 / 2
# }
# 
# write.csv(R_devs, file = paste0(pdir,"R_devs.csv"), row.names = FALSE)

OM_name <- "Fix-1-1"
# Set the harvest strategy
HSnum <- 1
HS <- paste0("HS", HSnum, "/")

# Set the HCR
HCRnum <- 1
HCR <- paste0("HCR", HCRnum, "/")

# Set the scenario
OMnum <- 1
OM <- paste0(OM_name[OMnum], "/")

# create a folder for all iterations
dir.create(paste0(pdir, HS)) # for that harvest strategy
dir.create(paste0(pdir, HS, HCR)) # for that harvest control rule
dir.create(paste0(pdir, HS, HCR, OM)) # for that OM

# BET_MSE(pdir, sdir, HS, HCR, OM, itrnum = 1, nquarters, Mcycle, n_extra_R)

#Calculate the numbers of cores 
no_cores = 5 # detectCores() - 2
#Initiate cluster
cl = makeCluster(no_cores)
registerDoParallel(cl)


foreach(itrnum = 1:niterations) %dopar% { IATTCMSE::BET_MSE(pdir, sdir, HS, HCR, OM, itrnum, nquarters, Mcycle, n_extra_R, startquarter, endquarter) }

stopCluster(cl)
