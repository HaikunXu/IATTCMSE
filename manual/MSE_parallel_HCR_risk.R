library(IATTCMSE)
library(foreach)
library(doParallel)

# Specify path of parent directory
pdir <- "D:/OneDrive - IATTC/IATTC/2025/MSE/Test/"

# Specify the path of conditioned initial OM
sdir <- "D:/OneDrive - IATTC/IATTC/2025/SAC16/BET F30/"

# Dimensions
niterations <- 3
nyears <- 6
nquarters <- nyears * 4
Mcycle <- 3
nsteps <- nyears / Mcycle
endquarter <- 196
startquarter <- 17
n_extra_R <- 2 # number of assessment period recruitment in the projection
EM_comp_fleet <- c(4, 23) # fleets with comps in ASPM Rdevs+
dat_name <- "BET-EPO.dat"
ctl_name <- "BET-EPO.ctl"
ss_name <- "ss.exe"

# Calculate the numbers of cores 
no_cores = 12 # detectCores() - 2
# Initiate cluster
cl = makeCluster(no_cores)
registerDoParallel(cl)

OM_name <- c("Fix-1-1", "Sel-1-1", "Gro-1-1", "Mrt-1-1")
OM <- paste0(OM_name, "/")
HCR_name <- "HCR_staff_risk"
HCR <- paste0(HCR_name, "/")

# Set the harvest strategy
HSnum <- 2
HS <- paste0("HS", HSnum, "/")
dir.create(paste0(pdir, HS)) # for that harvest strategy

for (HCRnum in 1:length(HCR)) {
  # unlink(paste0(pdir, HS, HCR[HCRnum]), recursive = TRUE)
  dir.create(paste0(pdir, HS, HCR[HCRnum])) # for that harvest control rule
  
  for (OMnum in 1:length(OM)) {
    # create a folder for all iterations
    unlink(paste0(pdir, HS, HCR[HCRnum], OM[OMnum]), recursive = TRUE)
    dir.create(paste0(pdir, HS, HCR[HCRnum], OM[OMnum])) # for that OM
  }
}

# specify the run list 
runs <- data.frame(expand.grid(run_hcr = HCR, run_om = OM, run_itr = 1:niterations))

# i = 1;  HCR= runs[i,1]; OM = runs[i,2]; itrnum= runs[i,3]
# BET_MSE_risk(pdir,sdir,HS,runs[i, 1],runs[i, 2],runs[i, 3],nquarters,Mcycle,n_extra_R,startquarter,endquarter,EM_comp_fleet,dat_name,ctl_name,ss_name,clean = FALSE)

foreach(i = 1:nrow(runs)) %dopar% {
  IATTCMSE::BET_MSE_risk(
    pdir,
    sdir,
    HS,
    HCR = runs[i, 1],
    OM = runs[i, 2],
    itr = runs[i, 3],
    nquarters,
    Mcycle,
    n_extra_R,
    startquarter,
    endquarter,
    EM_comp_fleet,
    dat_name,
    ctl_name,
    ss_name,
    clean = TRUE
  )
}

stopCluster(cl)

# for (i in 1:nrow(runs)) {
#   Clean(pdir, sdir, HS, HCR = runs[i, 1], OM = runs[i, 2], itr = runs[i, 3], nquarters, Mcycle)
# }
