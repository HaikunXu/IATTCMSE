library(dplyr)
library(tidyr)
library(ggplot2)

#Specify path of parent directory
pdir = "D:/OneDrive - IATTC/IATTC/2025/MSE/Test/"

#Specify the path of conditioned initial OM
sdir = "D:/OneDrive - IATTC/IATTC/2025/SAC16/BET F30/"

#Dimensions
# Nfisheries <- 22
niterations <- 100
nyears <- 15
nquarters <- nyears * 4
Mcycle <- 3
nsteps <- nyears/Mcycle
# endquarter <- 196
# startquarter <- 17
n_extra_R <- 2 #number of assessment period recruitment in the projection

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

# extract saved management output
for (itrnum in 1:2) {
  itr = paste0("itr", itrnum, "/")
  Record <- read.csv(paste0(paste0(pdir, HS, HCR, OM, itr, "Closure_days.csv")))
  Record$Step <- 1:nsteps
  Record$OM <- OM_name[OMnum]
  Record$itr <- itr
  
  if(itrnum == 1) Record_all <- Record
  else Record_all <- rbind(Record_all, Record)
}

ggplot(data = Record_all) +
  geom_line(aes(x = Step, y = x, color = itr)) +
  geom_point(aes(x = Step, y = x, color = itr))
