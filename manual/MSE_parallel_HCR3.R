library(IATTCMSE)
library(foreach)
library(doParallel)

# Specify path of parent directory
pdir <- "D:/OneDrive - IATTC/IATTC/2025/MSE/Test/"

# Specify the path of conditioned initial OM
sdir <- "D:/OneDrive - IATTC/IATTC/2025/SAC16/BET F30/"

# Dimensions
niterations <- 5
nyears <- 15
nquarters <- nyears * 4
Mcycle <- 3
nsteps <- nyears / Mcycle
endquarter <- 196
startquarter <- 17
n_extra_R <- 2 # number of assessment period recruitment in the projection
EM_comp_fleet <- NA # c(4, 23) # add simulated LF to the fleets with comps in ASPM Rdevs+

# Calculate the numbers of cores 
no_cores = 10 # detectCores() - 2
# Initiate cluster
cl = makeCluster(no_cores)
registerDoParallel(cl)


OM_name <- c("Fix-1-1", "Sel-1-1", "Gro-1-1", "Mrt-1-1")
OM <- paste0(OM_name, "/")
HCR_name <- c("HCR_staff", "HCR_staff_0", "HCR_staff_0_Fscaler")

# Set the harvest strategy
HSnum <- 1
HS <- paste0("HS", HSnum, "/")
dir.create(paste0(pdir, HS)) # for that harvest strategy

# Set the HCR
HCRnum <- 3
HCR <- paste0(HCR_name[HCRnum], "/")
dir.create(paste0(pdir, HS, HCR)) # for that harvest control rule

# specify the run list 
runs <- data.frame(expand.grid(run_om = OM, run_itr = 1:niterations))

for (OMnum in 1:4) {
  # create a folder for all iterations
  unlink(paste0(pdir, HS, HCR, OM[OMnum]), recursive = TRUE)
  dir.create(paste0(pdir, HS, HCR, OM[OMnum])) # for that OM
}

# BET_MSE(pdir, sdir, HS, HCR, runs[i,1], runs[i,2], nquarters, Mcycle, n_extra_R, startquarter, endquarter, EM_comp_fleet, clean = TRUE)

foreach(i = 1:nrow(runs)) %dopar% { IATTCMSE::BET_MSE(pdir, sdir, HS, HCR, runs[i,1], runs[i,2], nquarters, Mcycle, n_extra_R, startquarter, endquarter, EM_comp_fleet, clean = TRUE) }
  
stopCluster(cl)
