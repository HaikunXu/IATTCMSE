#' Runs the BET MSE framework for the specified number of iterations
#'
#' @param pdir parent directory path
#' @param sdir benchmark assessment model directory path
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM the name of the operating model
#' @param itrnum iteration number
#' @param nquarters total number of new quarters to be simulated in the MSE
#' @param Mcycle the number of years within a management cycle
#' @param n_extra_R the number of recruitment devs after the main R and before the forecast R
#' @param startquarter the first quarter of the assessment model
#' @param EM_comp_fleet The fleets with comp data in the EM
#' @param clean if TRUE, all intermediate folders of the MSE simulation will be deleted to save storage space
#' 
#' @author Haikun Xu 
#' @export

BET_MSE = function(pdir, sdir, HS, HCR, OM, itrnum, nquarters, Mcycle, n_extra_R, startquarter, endquarter, EM_comp_fleet, clean = FALSE) { 
  
  itr = paste0("itr", itrnum, "/")
  
  # create and set directory for each iteration (i.e. different recruitment)
  dir_itr <- paste0(pdir, HS, HCR, OM, itr)
  dir.create(dir_itr)
  
  nsteps <- nquarters / 4 / Mcycle
  R_devs <- read.csv(paste0(pdir,"R_devs.csv"))[, itrnum] # R devs for the itr iteration
  seed <- read.csv(paste0(pdir,"seeds.csv"))[itrnum, 1]
  
  SBR_d_ts <- rep(NA, nsteps)
  max_gradient_ts <- rep(NA, nsteps)
  Closure_ts <- rep(NA, nsteps)
  Fadjust_ts <- rep(NA, nsteps)
  F30_ts <- rep(NA,  nsteps)
  Time_ts <- rep(NA, nsteps)
  SB_ts <- rep(NA, nsteps)
  
  # *************************************************************************************
  # step 1: initialize the OM by copying from the benchmark assessment model
  # *************************************************************************************
  step1 <- IATTCMSE::Initialize_OM(pdir, sdir, HS, HCR, OM)
  Flag <- 1 # mark whether the loop is running without an EM with a large gradient
  
  for (istep in 1:nsteps){
    
    # print(paste0(pdir, HS, HCR, OM, itr, ": istep = ",istep))
    
    # specify the previous OM and EM directories
    if(istep == 1) {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, "itr0/")
      dir_EM_previous <- paste0(pdir, HS, "EM/")
      CurrentClosure <- 72
    }
    else {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/OM_Boot/")
      dir_EM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/EM/")
    }
    
    # *************************************************************************************
    # step 2: Compute the F for the new management cycle
    # *************************************************************************************
    dir_EM_HCR <- ifelse(istep == 1,
                         paste0(pdir, HS, "EM/"),
                         paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/", "EM/"))
    
    if(HCR == "HCR_staff/") step2 <- IATTCMSE::HCR_staff(dir_EM = dir_EM_HCR, istep, CurrentClosure)
    if(HCR == "HCR_staff_0/") step2 <- IATTCMSE::HCR_staff_0(dir_EM = dir_EM_HCR, istep, CurrentClosure)
    if(HCR == "HCR_staff_0_Fscaler/") step2 <- IATTCMSE::HCR_staff_0_Fscaler(OM, dir_EM = dir_EM_HCR, istep, CurrentClosure)
    if(HCR == "HCR_staff_Fscaler/") step2 <- IATTCMSE::HCR_staff_Fscaler(OM, dir_EM = dir_EM_HCR, istep, CurrentClosure)
    
    if(step2$max_gradient > 0.1) { # large gradient - the model does not converge
      max_gradient_ts[istep] <- step2$max_gradient # record the gradient
      Flag <- 0 # mark the flag
      break
    }
    
    # save some management quantities
    SBR_d_ts[istep] <- step2$SBR_d
    CurrentClosure <- step2$NewClosure
    Closure_ts[istep] <- step2$NewClosure
    max_gradient_ts[istep] <- step2$max_gradient
    Fadjust_ts[istep] <- step2$Fadjust
    F30_ts[istep] <- step2$Fscale / step2$Fadjust
    SB_ts[istep] <- step2$SB
    
    # *************************************************************************************
    # step 3: make projection using simulated R devs and HCR F
    # *************************************************************************************
    step3 <- IATTCMSE::Projection_OM(pdir, HS, HCR, OM, itr, istep, step2$Fscale, dir_OM_previous, dir_EM_previous, R_devs, n_extra_R, Mcycle)
    
    dir_istep <- step3$dir_istep
    dir_OM <- step3$dir_OM
    
    # *************************************************************************************
    # Step 4: Change the data files of the updated OM to run bootstrap
    # *************************************************************************************
    step4 <- IATTCMSE::Bootstrap_OM(dir_istep, istep, dir_OM, Mcycle, EM_comp_fleet, seed, endquarter)
    dir_OM_Boot <- step4
    
    # *************************************************************************************
    # Step 5: Update the OM with simulated data without error
    # *************************************************************************************
    step5 <- IATTCMSE::Update_OM(dir_OM, dir_OM_Boot, Mcycle)
    
    # *************************************************************************************
    # Step 6: Estimation model
    # *************************************************************************************
    
    # time stamp
    Time_ts[istep] <- Sys.time()
    
    if(istep < nsteps) step6 <- IATTCMSE::Estimationn_EM(dir_istep, step1$R0, dir_EM_previous, dir_OM_Boot, Mcycle)
    
  }
  
  if(Flag == 1) { # the loop is finished with all EM converged
    # *************************************************************************************
    # Step 7: Run the OM one last time to produce MSE time series outputs
    # *************************************************************************************
    step7 <- IATTCMSE::Final_OM(pdir, dir_itr, istep, dir_OM, dir_OM_Boot, Mcycle, endquarter, clean)
    dir_OM_Final <- step7
    
    # *************************************************************************************
    # Step 8: Extract OM_final's results
    # *************************************************************************************
    step8 <- IATTCMSE::Extract_OM(dir_OM_Final, startquarter)
    write.csv(step8, file = paste0(dir_itr, "Output.csv"), row.names = FALSE)
    
    # *************************************************************************************
    # Step 9: clean unnecessary folders to save space
    # *************************************************************************************
    if (clean == TRUE) {
      for (istep in 1:nsteps) {
        unlink(paste0(pdir, HS, HCR, OM, itr, "step", istep), recursive = TRUE)
      }
    }
  }

  # *************************************************************************************
  # Step 10: save HCR-related quantities
  # *************************************************************************************
  Record <- data.frame("SBR_d" = SBR_d_ts,
                       "max_gradient" = max_gradient_ts,
                       "closure" = Closure_ts,
                       "Fadjust" = Fadjust_ts,
                       "F30" = F30_ts,
                       "Time_Stamp" = Time_ts,
                       "SB" = SB_ts)
  
  write.csv(Record, file = paste0(dir_itr, "Record.csv"), row.names = FALSE)
}