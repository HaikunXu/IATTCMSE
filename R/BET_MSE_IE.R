#' Runs the BET MSE framework for the specified number of iterations
#'
#' @param pdir parent directory path
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM the name of the operating model
#' @param itrnum iteration number
#' @param nquarters total number of new quarters to be simulated in the MSE
#' @param Mcycle the number of years within a management cycle
#' @param n_extra_R the number of recruitment devs after the main R and before the forecast R
#' @param startquarter the first quarter of the assessment model
#' @param endquarter the last quarter of the assessment model
#' @param EM_comp_fleet The fleets with comp data in the EM
#' @param dat_name name of the SS data file
#' @param ctl_name name of the SS control file
#' @param ss_name name of the SS exe file
#' @param clean if TRUE, all intermediate folders of the MSE simulation will be deleted to save storage space
#'
#' @author Haikun Xu
#' @export

BET_MSE_IE = function(pdir,
                   HS,
                   HCR,
                   OM,
                   itrnum,
                   nquarters,
                   Mcycle,
                   n_extra_R,
                   startquarter,
                   endquarter,
                   EM_comp_fleet,
                   dat_name,
                   ctl_name,
                   ss_name,
                   Scontrol = 0.2,
                   clean = FALSE,
                   plot = FALSE,
                   MSY = FALSE) {
  
  itr = paste0("itr", itrnum, "/")
  
  # create and set directory for each iteration (i.e. different recruitment)
  dir_itr <- paste0(pdir, HS, HCR, OM, itr)
  dir.create(dir_itr)
  
  nsteps <- nquarters / 4 / Mcycle
  R_devs <- read.csv(paste0(pdir, "R_devs.csv"))[, itrnum] # R devs for the itr iteration
  seed <- read.csv(paste0(pdir, "seeds.csv"))[itrnum, 1]
  
  set.seed(seed)
  IE_ts <- rnorm(nsteps, -0.01/2, 0.1) # implementation error
  
  SBR_d_ts <- rep(NA, nsteps)
  max_gradient_ts <- rep(NA, nsteps)
  Closure_ts <- rep(NA, nsteps)
  Closure_diff_ts <- rep(NA, nsteps)
  Fratio_ts <- rep(NA, nsteps)
  F30_EM_ts <- rep(NA, nsteps)
  F30_ts <- rep(NA, nsteps)
  Fcurrent_EM_ts <- rep(NA, nsteps)
  Fcurrent_ts <- rep(NA, nsteps)
  Time_ts <- rep(NA, nsteps)
  SB_ts <- rep(NA, nsteps)
  FFMSY_ts <- rep(NA, nsteps)

  Flag <- 1 # mark whether the loop is running without an EM with a large gradient
  
  for (istep in 1:nsteps) {
    # print(paste0(pdir, HS, HCR, OM, itr, ": istep = ",istep))
    
    # specify the previous OM and EM directories
    if (istep == 1) {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, "itr0/")
      dir_EM_previous <- paste0(pdir, HS, HCR, "EM/")
      CurrentClosure <- 72
    }
    else {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/OM_Final/")
      dir_EM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/EM/")
    }
    
    # *************************************************************************************
    # step 2: Compute the F for the new management cycle
    # *************************************************************************************

    if (HCR == "HCR_staff/")
      step2 <- IATTCMSE::HCR_staff(dir_EM = dir_EM_previous, istep, CurrentClosure)
    if (HCR != "HCR_staff/")
      step2 <- IATTCMSE::HCR_others(dir_EM = dir_EM_previous, istep, CurrentClosure, Scontrol)

    if ((step2$max_gradient > 0.1) |
        (step2$SBR_d > 0.99) |
        (step2$SBR_d < 0.01)) {
      # large gradient - the model does not converge
      max_gradient_ts[istep] <- step2$max_gradient # record the gradient
      SBR_d_ts[istep] <- step2$SBR_d
      Flag <- 0 # mark the flag
      break
    }
    
    # add implementation error
    step2$Fratio <- step2$Fratio
    
    # update closure days
    Closure_diff_ts[istep] <- step2$NewClosure - CurrentClosure
    CurrentClosure <- step2$NewClosure
    
    # save some management quantities from the EM
    SBR_d_ts[istep] <- step2$SBR_d
    Closure_ts[istep] <- step2$NewClosure
    max_gradient_ts[istep] <- step2$max_gradient
    Fratio_ts[istep] <- step2$Fratio * exp(IE_ts[istep])
    SB_ts[istep] <- step2$SB
    Fcurrent_EM_ts[istep] <- step2$Fcurrent
    F30_EM_ts[istep] <- step2$F30
    
    # *************************************************************************************
    # step 3: make projection using simulated R devs and HCR F
    # *************************************************************************************
    step3 <- IATTCMSE::Projection_OM(
      pdir,
      HS,
      HCR,
      OM,
      itr,
      istep,
      step2$Fratio * exp(IE_ts[istep]), # add implementation error
      dir_OM_previous,
      dir_EM_previous,
      R_devs,
      n_extra_R,
      Mcycle,
      dat_name,
      ctl_name,
      ss_name,
      plot = plot
    )
    
    dir_istep <- step3$dir_istep
    dir_OM <- step3$dir_OM
    Fcurrent_ts[istep] <- step3$Fcurrent
    F30_ts[istep] <- step3$F30
    
    # *************************************************************************************
    # Step 4: Change the data files of the updated OM to run bootstrap
    # *************************************************************************************
    step4 <- IATTCMSE::Bootstrap_OM(
      dir_istep,
      istep,
      dir_OM,
      Mcycle,
      EM_comp_fleet,
      seed,
      endquarter,
      dat_name,
      ctl_name,
      ss_name
    )
    dir_OM_Boot <- step4
    
    # *************************************************************************************
    # Step 5: Update the OM with simulated data without error
    # *************************************************************************************
    
    step5 <- IATTCMSE::Update_OM(
      istep,
      dir_OM,
      dir_OM_Boot,
      paste0(dir_istep, "OM_Final/"),
      Mcycle,
      endquarter,
      dat_name,
      ss_name,
      ctl_name
    )
    
    if(MSY == TRUE) {

      # *************************************************************************************
      # Step 5 (optional): Update the OM with FMSY related quantities
      # *************************************************************************************

      step5_plus <- IATTCMSE::Update_OM_FMSY(
        istep,
        paste0(dir_istep, "OM_Final/"),
        paste0(dir_istep, "OM_FMSY/"),
        dat_name,
        ss_name,
        ctl_name
      )

      FFMSY_ts[istep] <- step5_plus
    }
    
    # *************************************************************************************
    # Step 6: Estimation model
    # *************************************************************************************
    
    # time stamp
    Time_ts[istep] <- Sys.time()
    
    # q_hypothesis <- stringr::str_split(OM, "-", simplify = TRUE)[2]
    R0 <- step5 + 0.2 # + 50 * (as.numeric(q_hypothesis) - 1) # to make the model easy to converge
    
    if (istep < nsteps)
      step6 <- IATTCMSE::Estimation_EM(
        dir_istep,
        dir_EM_previous,
        dir_OM_Boot,
        R0,
        Mcycle,
        dat_name,
        ctl_name,
        ss_name,
        plot = plot
      )
  }
  
  if (Flag == 1) {
    # the loop is finished with all EM converged
    # *************************************************************************************
    # Step 7: Run the OM one last time to produce MSE time series outputs
    # *************************************************************************************
    step7 <- IATTCMSE::Final_OM(
      pdir,
      dir_itr,
      istep,
      dir_OM,
      dir_OM_Boot,
      Mcycle,
      endquarter,
      dat_name,
      ctl_name,
      ss_name
    )
    dir_OM_Final <- step7
    
    # *************************************************************************************
    # Step 8: Extract OM_final's results
    # *************************************************************************************
    step8 <- IATTCMSE::Extract_OM(dir_OM_Final, startquarter, clean = clean, plot = plot)
    
    if(MSY == TRUE) {
      
      # *************************************************************************************
      # Step 7 (optional): Update the OM with SMSY related quantities
      # *************************************************************************************
      
      step7_plus <- IATTCMSE::Update_OM_SMSY(
        istep,
        paste0(dir_istep, "OM_Final/"),
        paste0(dir_istep, "OM_SMSY/"),
        dat_name,
        ss_name,
        ctl_name
      )
      
      step8$SMSY <- step7_plus$SMSY
    }
    
    write.csv(step8,
              file = paste0(dir_itr, "Output.csv"),
              row.names = FALSE)
    
    # *************************************************************************************
    # Step 9: clean unnecessary folders to save space
    # *************************************************************************************
    if (clean == TRUE) {
      for (istep in 1:nsteps) {
        unlink(paste0(pdir, HS, HCR, OM, itr, "step", istep), recursive = TRUE)
      }
      # unlink(dir_OM_Final, recursive = TRUE)
    }
  }
  
  # *************************************************************************************
  # Step 10: save HCR-related quantities
  # *************************************************************************************
  Record <- data.frame(
    "SBR_d" = SBR_d_ts,
    "max_gradient" = max_gradient_ts,
    "closure" = Closure_ts,
    "closure_diff" = Closure_diff_ts,
    "F30" = F30_ts,
    "F30_EM" = F30_EM_ts,
    "Fcurrent_EM" = Fcurrent_EM_ts,
    "Fcurrent" = Fcurrent_ts,
    "Time_Stamp" = Time_ts,
    "Fratio" = Fratio_ts,
    "SB" = SB_ts,
    "FFMSY" = FFMSY_ts,
    "Implementation_Error" = IE_ts
  )
  
  write.csv(Record,
            file = paste0(dir_itr, "Record.csv"),
            row.names = FALSE)
}