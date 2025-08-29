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
#' 
#' @author Haikun Xu 
#' @export

BET_MSE = function(pdir, sdir, HS, HCR, OM, itrnum, nquarters, Mcycle, n_extra_R, startquarter, endquarter, EM_comp_fleet) { 
  
  itr = paste0("itr", itrnum, "/")
  
  # create and set directory for each iteration (i.e. different recruitment)
  dir.create(paste0(pdir, HS, HCR, OM, itr))
  
  nsteps <- nquarters / 4 / Mcycle
  R_devs <- read.csv(paste0(pdir,"R_devs.csv"))[, itrnum] # R devs for the itr iteration
  seed <- read.csv(paste0(pdir,"seeds.csv"))[itrnum, 1]
  
  SBR_d_ts <- rep(NA, nsteps)
  max_gradient_ts <- rep(NA, nsteps)
  Closure_ts <- rep(NA, nsteps)
  Fmult_ts <- rep(NA, nsteps)
  
  # *************************************************************************************
  # step 1: initialize the OM by copying from the benchmark assessment model
  # *************************************************************************************
  step1 <- IATTCMSE::Initialize_OM(pdir, sdir, HS, HCR, OM)
  
  for (istep in 1:nsteps){
    
    print(paste0(pdir, HS, HCR, OM, itr, ": istep = ",istep))
    
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
    
    step2 <- IATTCMSE::HCR_staff(dir_EM = dir_EM_HCR, istep, CurrentClosure)
    
    if(step2$max_gradient > 0.1) { # large gradient - the model does not converge
      # read the report file from the OM projection
      # om_out = r4ss::SS_output(dir = dir_OM_previous, covar = F, verbose = FALSE, printstats = FALSE)
      # plot the latest OM to examine why the EM does not converge
      # r4ss::SS_plots(replist=om_out, forecastplot=T, uncertainty=F, datplot=T, plot = c(3, 7), verbose = FALSE)
      break
    }
    
    # save some management quantities
    SBR_d_ts[istep] <- step2$SBR_d
    CurrentClosure <- step2$NewClosure
    Closure_ts[istep] <- step2$NewClosure
    Fscale <- step2$Fscale
    max_gradient_ts[istep] <- step2$max_gradient
    Fmult_ts[istep] <- step2$Fmult
    
    # *************************************************************************************
    # step 3: make projection using simulated R devs and HCR F
    # *************************************************************************************
    step3 <- IATTCMSE::Projection_OM(pdir, HS, HCR, OM, itr, istep, Fscale, dir_OM_previous, dir_EM_previous, R_devs, n_extra_R, Mcycle)
    
    dir_istep <- step3$dir_istep
    dir_OM <- step3$dir_OM
    
    # *************************************************************************************
    # Step 4: Change the data files of the updated OM to run bootstrap
    # *************************************************************************************
    step4 <- IATTCMSE::Bootstrap_OM(dir_istep, istep, dir_OM, Mcycle, seed, endquarter)
    dir_OM_Boot <- step4
    
    # *************************************************************************************
    # Step 5: Estimation model
    # *************************************************************************************
    if(istep < nsteps) 
      IATTCMSE::Estimationn_EM(dir_istep, step1$R0, dir_OM_previous, dir_EM_previous, dir_OM_Boot, Mcycle, EM_comp_fleet)
    else
      IATTCMSE::Estimationn_EM(dir_istep, step1$R0, dir_OM_previous, dir_EM_previous, dir_OM_Boot, Mcycle, EM_comp_fleet, plot = TRUE)
  }
  
  # *************************************************************************************
  # Step 6: Run the OM one last time to produce MSE time series outputs
  # *************************************************************************************
  step6 <- IATTCMSE::Final_OM(dir_istep, istep, dir_OM, Mcycle, endquarter)
  dir_OM_Final <- step6
  
  # *************************************************************************************
  # Step 7: Extract OM_final's results
  # *************************************************************************************
  step7 <- IATTCMSE::Extract_OM(dir_OM_Final, startquarter)
  write.csv(step7, file = paste0(pdir, HS, HCR, OM, itr, "Output.csv"), row.names = FALSE)
  
  Record <- data.frame("SBR_d" = SBR_d_ts,
                       "max_gradient" = max_gradient_ts,
                       "closure" = Closure_ts,
                       "Fmult" = Fmult_ts)
  write.csv(Record, file = paste0(pdir, HS, HCR, OM, itr, "Record.csv"), row.names = FALSE)
}