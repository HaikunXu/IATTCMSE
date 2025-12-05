library(IATTCMSE)
library(dplyr)
library(foreach)
library(doParallel)

# Specify path of parent directory
pdir <- "D:/OneDrive - IATTC/IATTC/2025/MSE/Test/"

# Specify the path of conditioned initial OM
sdir <- "D:/OneDrive - IATTC/IATTC/2025/Update_Assessment/F30/"

# Dimensions
niterations <- 10
nyears <- 12
nquarters <- nyears * 4
Mcycle <- 3
nsteps <- nyears / Mcycle
endquarter <- 200
startquarter <- 17
n_extra_R <- 2 # number of assessment period recruitment in the projection
EM_comp_fleet <- c(4, 23) # fleets with comps in ASPM Rdevs+
dat_name <- "BET-EPO.dat"
ctl_name <- "BET-EPO.ctl"
ss_name <- "ss.exe"

model = c("Fix", "Gro", "Sel", "Mrt")
catchability = c(1, 1.01, 1.02)
steepness = c(1, 0.9, 0.8)
converge <- array(1, dim = c(length(model), length(catchability), length(steepness)))
# converge[1, 3, 3] <- 0
# converge[4, 2, 2:3] <- 0

OM_list <- data.frame(expand.grid(
  Model = model,
  Catchability = catchability,
  Steepness = steepness
))
OM_list$converge <- converge[1:36]

OM_list <- OM_list %>% filter(converge == 1)

OM_name <- paste0(OM_list$Model, "-", OM_list$Catchability, "-", OM_list$Steepness)
OM <- paste0(OM_name, "/")
HCR_name <- "HCR_staff_risk"
HCR <- paste0(HCR_name, "/")

# Set the harvest strategy
HSnum <- 2
HS <- paste0("HS", HSnum, "/")
dir.create(paste0(pdir, HS)) # for that harvest strategy

# unlink(paste0(pdir, HS, HCR[HCRnum]), recursive = TRUE)
dir.create(paste0(pdir, HS, HCR)) # for that harvest control rule

for (OMnum in 1:length(OM)) {
  # create a folder for all iterations
  unlink(paste0(pdir, HS, HCR, OM[OMnum]), recursive = TRUE)
  dir.create(paste0(pdir, HS, HCR, OM[OMnum])) # for that OM
}

Weight_M <- data.frame("Model" = model, "Weight_M" =  rep(1 / length(model), length(model)))
Weight_Q <- data.frame("Catchability" = catchability, "Weight_Q" = rep(1 / length(catchability), length(catchability)))
Weight_S <- data.frame("Steepness" = steepness, "Weight_S" = c(0.46, 0.32, 0.22))

# specify the run list 
runs <- data.frame(expand.grid(
  Model = model,
  Catchability = catchability,
  itr = 1:niterations
))
runs$Steepness <- NA

Weight_tot <- left_join(left_join(left_join(OM_list,Weight_M),Weight_Q),Weight_S) %>%
  group_by(Model, Catchability) %>%
  mutate(Weight_S2 = Weight_S / sum(Weight_S) * niterations)

for (m in 1:length(model)) {
  for (q in 1:length(catchability)) {
    
    Weight_1 <- Weight_tot %>% filter(Model == model[m], Catchability == catchability[q], Steepness == 1)
    Weight_0.9 <- Weight_tot %>% filter(Model == model[m], Catchability == catchability[q], Steepness == 0.9)
    Weight_0.8 <- Weight_tot %>% filter(Model == model[m], Catchability == catchability[q], Steepness == 0.8)
    
    n_1 <- round(sum(Weight_1$Weight_S2, na.rm = TRUE))
    n_0.9 <- round(sum(Weight_0.9$Weight_S2, na.rm = TRUE))

    runs$Steepness[which(runs$Model==model[m]&runs$Catchability==catchability[q])][1:n_1] <- 1
    if(n_1 < niterations) runs$Steepness[which(runs$Model==model[m]&runs$Catchability==catchability[q])][(n_1+1):(n_1+n_0.9)] <- 0.9
    if(n_1 + n_0.9 < niterations) runs$Steepness[which(runs$Model==model[m]&runs$Catchability==catchability[q])][(n_1+n_0.9+1):niterations] <- 0.8
    
  }
}

counts <- runs %>% group_by(Model, Catchability, Steepness) %>% summarise(n=n())

runs$OM <- paste0(runs$Model, "-", runs$Catchability, "-", runs$Steepness, "/")

# runs <- runs[1, ]

# Calculate the numbers of cores 
no_cores = 12 # detectCores() - 2
# Initiate cluster
cl = makeCluster(no_cores)
registerDoParallel(cl)

# *************************************************************************************
# step 1: initialize the OM by copying from the benchmark assessment model
# *************************************************************************************
foreach(i = 1:length(OM_name)) %dopar% {
  IATTCMSE::Initialize_OM(pdir, sdir, HS, HCR, OM[i], dat_name, ctl_name, ss_name)
}

# run the MSE

# i = 1; OM = runs[i,5]; itrnum = runs[i,3]
# BET_MSE_risk(pdir,sdir,HS,HCR,runs[i, 5],runs[i, 3],nquarters,Mcycle,n_extra_R,startquarter,endquarter,EM_comp_fleet,dat_name,ctl_name,ss_name,
#         clean = TRUE,
#         plot = FALSE)

foreach(i = 1:nrow(runs)) %dopar% {
  IATTCMSE::BET_MSE_risk(
    pdir,
    sdir,
    HS,
    HCR = HCR,
    OM = runs[i, 5],
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
    clean = TRUE,
    plot = FALSE
  )
}

stopCluster(cl)