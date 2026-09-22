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

BET_MSE_LL = function(pdir,
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
                   MSY = FALSE,
                   IE_CV = 0.1,
                   Fscaler = 0.828065333,
                   Sscaler = 1.163170077,
                   LL_catch_limit = 55131) {
  
  itr = paste0("itr", itrnum, "/")
  
  # create and set directory for each iteration (i.e. different recruitment)
  dir_itr <- paste0(pdir, HS, HCR, OM, itr)
  dir.create(dir_itr)
  
  nsteps <- nquarters / 4 / Mcycle
  R_devs <- read.csv(paste0(pdir, "R_devs.csv"))[, itrnum] # R devs for the itr iteration
  seed <- read.csv(paste0(pdir, "seeds.csv"))[itrnum, 1]
  
  set.seed(seed)
  IE_ts <- rnorm(nsteps, -IE_CV^2/2, IE_CV) # implementation error
  
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
      
      # get the current F vector
      ForeRepName <- paste(dir_OM_previous, "Forecast-report.SSO", sep = "")
      ForeRepStart <- grep("Management_report", readLines(ForeRepName))
      ForeRepEnd <- grep("THIS FORECAST IS FOR PURPOSES", readLines(ForeRepName))[1]
      ForeDat <- read.table(file = ForeRepName, col.names = c(seq(1, 10, by = 1)), fill = T, quote = "", colClasses = "character", 
                            nrows = ForeRepEnd - ForeRepStart, skip = ForeRepStart - 1)
      ForeDat <- as.data.frame(ForeDat)
      FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
      Fvector <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
      Fvector <- as.numeric(Fvector[3:length(Fvector)])
    }
    else {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/OM/3Final/")
      dir_EM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/EM/")
    }
    
    # *************************************************************************************
    # step 2: Compute the F for the new management cycle
    # *************************************************************************************

    if (HCR == "HCR_staff/")
      step2 <- IATTCMSE::HCR_staff(dir_EM = dir_EM_previous, istep, CurrentClosure)
    if (HCR != "HCR_staff/")
      step2 <- IATTCMSE::HCR_others(dir_EM = dir_EM_previous, istep, CurrentClosure, Scontrol, Fscaler, Sscaler)

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
    # step2$Fratio <- step2$Fratio
    
    # update closure days
    Closure_diff_ts[istep] <- step2$NewClosure - CurrentClosure
    CurrentClosure <- step2$NewClosure
    
    # save some management quantities from the EM
    SBR_d_ts[istep] <- step2$SBR_d
    Closure_ts[istep] <- step2$NewClosure
    max_gradient_ts[istep] <- step2$max_gradient
    Fratio_ts[istep] <- step2$Fratio * exp(IE_ts[istep]) # add implementation error
    SB_ts[istep] <- step2$SB
    Fcurrent_EM_ts[istep] <- step2$Fcurrent
    F30_EM_ts[istep] <- step2$F30
    

    # create directory for new time step where the new dat file will be saved
    dir_istep <- paste0(pdir, HS, HCR, OM, itr, "step", istep, "/")
    dir.create(dir_istep)
    
    # step 1: create a new folder for the OM
    dir_OM_root <- paste0(dir_istep, "OM/")
    dir.create(dir_OM_root)
    
    # update the F vector for the new management cycle
    Fvector <- c(Fvector[1:14], Fvector[15:22] * step2$Fratio) * exp(IE_ts[istep])
    
    for (cycle in 1:3) {
      
      # *************************************************************************************
      # step 3: make projection using simulated R devs and HCR F
      # *************************************************************************************
      
    if (cycle > 1) {
      dir_OM_previous <- paste0(dir_istep, "OM/", cycle - 1, "Final/")
    }
      
    dir_OM <- paste0(dir_OM_root, cycle,"/")
    dir.create(dir_OM)
    
    files = c(
      paste0(dir_OM_previous, dat_name),
      paste0(dir_OM_previous, ctl_name),
      paste0(pdir, ss_name),
      paste0(dir_OM_previous, "go_nohess.bat")
    )
    file.copy(from = files, to = dir_OM, overwrite = TRUE)
    
    
    # step 2: change par file
    ParDir <- paste0(dir_OM_previous, "ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    
    Line <- match("# Fcast_recruitments:", ParFile)
    
    R_forecast <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line
    )
    
    if(n_extra_R == 0)
      R_forecast_new <- c(R_devs[((istep-1)*Mcycle*4+1+(cycle-1)*4):((istep-1)*Mcycle*4+cycle*4)], 0)
    else 
      R_forecast_new <- c(as.vector(t(R_forecast[1,1:n_extra_R])), 
                          R_devs[((istep-1)*Mcycle*4+1+(cycle-1)*4):((istep-1)*Mcycle*4+cycle*4)],
                          0)
    
    ParFile[Line + 1] <- gsub(",", "", toString(R_forecast_new))
    writeLines(ParFile, paste0(dir_OM, "/ss3.par"))
    
    
    # step 3: change forecast file
    Forecast <- r4ss::SS_readforecast(paste0(dir_OM_previous, "forecast.ss"), verbose = FALSE)
    
    Forecast$Nforecastyrs <- 1 + 4 # number of forecast years
    
    if (istep + cycle == 2) years <- 201:205 # the first prediction
    else years <- unique(Forecast$ForeCatch$year) + 4
    
    Forecast$ForeCatch <- data.frame(
      year = rep(years, each = 22),
      seas = 1,
      fleet = 1:22,
      catch_or_F = Fvector,
      basis = 99
    )

    r4ss::SS_writeforecast(Forecast, dir_OM, verbose = FALSE, overwrite = TRUE)
    
    
    # change starter file
    starter <- r4ss::SS_readstarter(paste0(dir_EM_previous, "/starter.ss"), verbose = FALSE)
    
    #specify to use the ss3.par as parameters
    starter$init_values_src = 1
    #turn off estimation of parameters 
    starter$last_estimation_phase = 0
    
    #write new starter file
    r4ss::SS_writestarter(starter, dir_OM, verbose = FALSE, overwrite = TRUE)
    
    
    # step 5: run ss
    # setwd(dir_OM)
    command <- paste("cd", dir_OM, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    # step 6: check LL catch
    om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)

    TS <- om_out$timeseries
    col_id <- which(names(TS) %in% paste0("dead(B):_", 1:14))
    
    # total predicted longline catch for the predicted year
    Catch_LL <- sum(TS[(nrow(TS) - 4):(nrow(TS) - 1), col_id]) 
    # calculate whether longline catch needs to be reduced to the limit
    LL_catch_scaler <- ifelse(Catch_LL < LL_catch_limit, 1, LL_catch_limit / Catch_LL)
    
    #################################################### Boot_OM ####################################################
    # dir_OM_Final <- paste0(dir_OM, "LL")
    # dir.create(dir_OM_Final)
    
    # step 1: create a new folder for the OM bootstrap
    dir_OM_Boot <- paste0(dir_OM_root, cycle, "Boot/")
    dir.create(dir_OM_Boot)
    
    # copy files to the new folder
    files = c(
      paste0(dir_OM, "starter.ss"),
      paste0(dir_OM, ss_name),
      paste0(dir_OM, "go_nohess.bat")
    )
    file.copy(from = files, to = dir_OM_Boot, overwrite = TRUE)
    
    # read the report file from the OM projection
    om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)
    
    # read projected catch
    Catch_projection <- r4ss::SS_ForeCatch(om_out,
                                           yrs = ((istep - 1) * Mcycle * 4 + endquarter + 1 + (cycle - 1) * 4):((istep - 1) * Mcycle * 4 + endquarter + cycle * 4),
                                           zeros = TRUE)
    
    # LL catch limit
    Catch_projection$`dead(B)`[which(Catch_projection$Fleet < 15)] <- Catch_projection$`dead(B)`[which(Catch_projection$Fleet < 15)] * LL_catch_scaler
    
    # read catch file
    dat <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, dat_name), verbose = FALSE)
    Catch <- dat$catch
    
    Catch_projection_new <- data.frame("year" = Catch_projection$`#Year`,
                                       "seas" = Catch_projection$Seas,
                                       "fleet" = Catch_projection$Fleet,
                                       "catch" = Catch_projection$`dead(B)`,
                                       "catch_se" = 0.01)
    
    Catch_new <- dplyr::arrange(rbind(Catch, Catch_projection_new), fleet, year)
    
    dat$catch <- Catch_new
    dat$endyr <- dat$endyr + 4
    
    # add dummy CPUE data
    CPUE_new <- dat$CPUE[(nrow(dat$CPUE) - 3):nrow(dat$CPUE),]
    CPUE_new$year <- CPUE_new$year + 4
    CPUE_new$se_log <- mean(dat$CPUE$se_log[(nrow(dat$CPUE)-11):nrow(dat$CPUE)])
    dat$CPUE <- rbind(dat$CPUE, CPUE_new)
    
    # add dummy LF data
    if(sum(is.na(EM_comp_fleet)) == FALSE) {
      LF <- dat$sizefreq_data_list[[1]] # [which(dat$sizefreq_data_list$)]
      LF_new <- LF[which((LF$year %in% 177:180) & LF$fleet %in% EM_comp_fleet),] # no LL LF during COVID years so use pre-COVID samples
      LF_new$year <- CPUE_new$year # 4 COVID-years (2020-2023)
      dat$sizefreq_data_list[[1]] <- rbind(LF, LF_new)
      dat$Nobs_per_method <- nrow(dat$sizefreq_data_list[[1]])
    }
    r4ss::SS_writedat_3.30(dat, paste0(dir_OM_Boot, dat_name), verbose = FALSE, overwrite = TRUE)
    
    
    # change recruitment period in the control file
    ctl <- r4ss::SS_readctl_3.30(
      file = paste0(dir_OM, ctl_name),
      verbose = FALSE,
      datlist = dat,
      use_datlist = TRUE
    )
    ctl$MainRdevYrLast <- ctl$MainRdevYrLast + 4 # increase the main recruitment last year
    
    r4ss::SS_writectl_3.30(
      ctl,
      outfile = paste0(dir_OM_Boot, ctl_name),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    
    # change forecast file
    Forecast <- r4ss::SS_readforecast(paste0(dir_OM, "forecast.ss"), verbose = FALSE)
    Forecast$Nforecastyrs <- 1
    r4ss::SS_writeforecast(Forecast, dir_OM_Boot, verbose = FALSE, overwrite = TRUE)
    
    
    # change recruitment in the par file
    ParDir <- paste0(dir_OM, "ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    
    Line_main <- match("# recdev2:", ParFile)
    R_main <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line_main
    )
    
    Line_forecast <- match("# Fcast_recruitments:", ParFile)
    R_forecast <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line_forecast
    )
    
    R_main_new <- cbind(R_main, R_forecast[1,1:4])
    R_forecast_new <- R_forecast[1,5:ncol(R_forecast)]
    
    ParFile[Line_main + 1] <- gsub(",", "", toString(R_main_new))
    ParFile[Line_forecast + 1] <- gsub(",", "", toString(R_forecast_new))
    
    writeLines(ParFile, paste0(dir_OM_Boot, "/ss3.par"))
    
    # set up bootstrap
    starter_boot <- r4ss::SS_readstarter(paste0(dir_OM_Boot, "starter.ss"), verbose = FALSE)
    
    # specify to use the ss3.par as parameters
    starter_boot$init_values_src = 1
    # turn off estimation of parameters 
    starter_boot$last_estimation_phase = 0
    # add 1 data bootstrap file
    starter_boot$N_bootstraps = 3
    # add the seed
    starter_boot$seed <- seed
    
    # write new starter file
    r4ss::SS_writestarter(starter_boot, dir_OM_Boot, verbose = FALSE, overwrite = TRUE)
    
    
    # run the bootstrap model
    command <- paste("cd", dir_OM_Boot, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    

    
    #################################################### update_OM ####################################################
    
    dir_OM_Final <- paste0(dir_OM_root, cycle, "Final/")
    dir.create(dir_OM_Final)
    
    # copy files to the new folder
    files = c(
      paste0(dir_OM, "starter.ss"),
      paste0(dir_OM, ss_name),
      paste0(dir_OM, "go_nohess.bat")
    )
    file.copy(from = files, to = dir_OM_Final, overwrite = TRUE)
    
    # change data file
    dat_OM <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, dat_name), verbose = FALSE)
    dat_OM_Boot <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, dat_name), verbose = FALSE)
    data_true <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, "data_expval.ss"), verbose = FALSE)
    
    # add new CPUE
    CPUE <- dat_OM$CPUE
    CPUE_true <- data_true$CPUE[(nrow(CPUE)+1):nrow(data_true$CPUE),]
    CPUE_new <- rbind(CPUE, CPUE_true)
    
    # add new LF
    LF <- dat_OM$sizefreq_data_list[[1]]
    LF_true <- dplyr::filter(data_true$sizefreq_data_list[[1]], year > max(LF$year))
    LF_new <- dplyr::arrange(rbind(LF, LF_true), fleet, year)
    
    # add new catch
    catch <- dat_OM$catch
    max_year <- max(catch$year)
    catch_true <- dplyr::filter(data_true$catch, year > max_year)
    catch_new <- dplyr::arrange(rbind(catch, catch_true), fleet, year)
    
    # save data file
    dat_OM$catch <- catch_new
    dat_OM$CPUE <- CPUE_new
    dat_OM$sizefreq_data_list[[1]] <- LF_new
    dat_OM$Nobs_per_method <- nrow(LF_new)
    dat_OM$endyr <- dat_OM$endyr + 4
    r4ss::SS_writedat_3.30(dat_OM, paste0(dir_OM_Final, dat_name), verbose = FALSE, overwrite = TRUE)
    
    # read data file
    # dat <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Final, dat_name), verbose = FALSE)
    
    # change recruitment period in the control file
    ctl <- r4ss::SS_readctl_3.30(
      file = paste0(dir_OM, ctl_name),
      verbose = FALSE,
      datlist = dat_OM,
      use_datlist = TRUE
    )
    ctl$MainRdevYrLast <- ctl$MainRdevYrLast + 4 # increase the main recruitment last year
    
    r4ss::SS_writectl_3.30(
      ctl,
      outfile = paste0(dir_OM_Final, ctl_name),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    # change forecast file
    Forecast <- r4ss::SS_readforecast(paste0(dir_OM, "forecast.ss"), verbose = FALSE)
    Forecast$Nforecastyrs <- 1
    r4ss::SS_writeforecast(Forecast, dir_OM_Final, verbose = FALSE, overwrite = TRUE)
    
    # change recruitment in the par file
    ParDir <- paste0(dir_OM, "ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    
    Line_main <- match("# recdev2:", ParFile)
    R_main <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line_main
    )
    
    Line_forecast <- match("# Fcast_recruitments:", ParFile)
    R_forecast <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line_forecast
    )
    
    R_main_new <- cbind(R_main, R_forecast[1,1:4])
    R_forecast_new <- R_forecast[1,5:ncol(R_forecast)]
    
    ParFile[Line_main + 1] <- gsub(",", "", toString(R_main_new))
    ParFile[Line_forecast + 1] <- gsub(",", "", toString(R_forecast_new))
    
    writeLines(ParFile, paste0(dir_OM_Final, "/ss3.par"))
    
    # starter file
    starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
    
    #specify to use the ss3.par as parameters
    starter$init_values_src = 1
    #turn off estimation of parameters 
    starter$last_estimation_phase = 0
    #
    starter$maxyr_sdreport <- starter$maxyr_sdreport + 4
    
    #write new starter file
    r4ss::SS_writestarter(starter, dir_OM_Final, verbose = FALSE, overwrite = TRUE)
    
    # run the OM
    command <- paste("cd", dir_OM_Final, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    if (cycle == 3) {
      ParDir <- paste0(dir_OM_Final, "ss3.par")
      ParFile <- readLines(ParDir, warn = F)
      
      Line_R0 <- match("# SRparm[1]:", ParFile)
      step5 <- as.numeric(ParFile[Line_R0 + 1])
    }

    
    }
    
    
    # dir_istep <- step3$dir_istep
    # dir_OM <- step3$dir_OM
    Fcurrent_ts[istep] <- NA # step3$Fcurrent
    F30_ts[istep] <- NA # step3$F30
    
    if(MSY == TRUE & cycle == 3) {

      # *************************************************************************************
      # Step 5 (optional): Update the OM with FMSY related quantities
      # *************************************************************************************
      dir_OM_MSY <- paste0(dir_istep, "OM_FMSY/")
      dir.create(dir_OM_MSY)

      files = c(
        paste0(dir_OM_Final, ctl_name),
        paste0(dir_OM_Final, dat_name),
        paste0(dir_OM_Final, ss_name),
        paste0(dir_OM_Final, "go_nohess.bat"),
        paste0(dir_OM_Final, "ss3.par")
      )
      file.copy(from = files, to = dir_OM_MSY, overwrite = TRUE)

      # change forecast file
      Forecast <- r4ss::SS_readforecast(paste0(dir_OM_Final, "forecast.ss"), verbose = FALSE)
      Forecast$MSY <- 2 # calculate FMSY
      r4ss::SS_writeforecast(Forecast, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)

      # change starter file
      starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
      starter$F_std_basis <- 122
      r4ss::SS_writestarter(starter, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)

      # run the OM
      command <- paste("cd", dir_OM_MSY, "& go_nohess.bat", sep = " ")
      ss <- shell(cmd = command, intern = T, wait = T)

      # FMSY
      ForeRepName <- paste(dir_OM_MSY, "Forecast-report.SSO", sep = "")

      # Get management report
      ForeRepStart <- grep("Management_report", readLines(ForeRepName))
      ForeRepEnd <- grep("THIS FORECAST IS FOR PURPOSES", readLines(ForeRepName))[1]

      # ForeDat <- read.table(file=ForeRepName,col.names=c(seq(1,10,by=1)),fill=T,quote='',colClasses='character',
      # nrows=45, skip = ForeRepStart-1)
      ForeDat <- read.table(file = ForeRepName, col.names = c(seq(1, 10, by = 1)), fill = T, quote = "", colClasses = "character",
                            nrows = ForeRepEnd - ForeRepStart, skip = ForeRepStart - 1)
      ForeDat <- as.data.frame(ForeDat)

      FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
      Fvector_FMSY <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
      Fvector_FMSY <- Fvector_FMSY[3:length(Fvector_FMSY)]
      FmultScale <- sum(Fvector_FMSY) # F

      # Fmultiplier
      Fmult <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3] # FMSY
      FFMSY <- FmultScale/Fmult

      FFMSY_ts[istep] <- FFMSY
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
    # Step 8: Extract OM_final's results
    # *************************************************************************************
    step8 <- IATTCMSE::Extract_OM(dir_OM_Final, startquarter, clean = clean, plot = plot)
    
    if(MSY == TRUE) {
      
      # *************************************************************************************
      # Step 7 (optional): Update the OM with SMSY related quantities
      # *************************************************************************************
      
      dir_OM_MSY <- paste0(dir_istep, "OM_SMSY/")
      dir.create(dir_OM_MSY)
      
      files = c(
        paste0(dir_OM_Final, ctl_name),
        paste0(dir_OM_Final, dat_name),
        paste0(pdir, ss_name),
        paste0(dir_OM_Final, "go_nohess.bat"),
        paste0(dir_OM_Final, "ss3.par")
      )
      file.copy(from = files, to = dir_OM_MSY, overwrite = TRUE)
      
      # change par file
      ParDir <- paste0(dir_OM_MSY, "ss3.par")
      ParFile <- readLines(ParDir, warn = F)
      
      Rep <- r4ss::SS_output(
        dir = dir_OM_Final,
        covar = F,
        printstats = F,
        verbose = FALSE
      )
      
      Recruit <- Rep$recruit$dev[which(Rep$recruit$era %in% c("Main", "Late"))]
      bias_adjust <- -Rep$recruit$biasadjuster[which(Rep$recruit$era %in% c("Main", "Late"))] * 0.6 ^ 2 / 2
      Recruit_late <- Rep$recruit$dev[which(Rep$recruit$era == "Late")]
      
      Recruit_forecast <- c(Recruit_late, Recruit + bias_adjust, 0)
      
      Line <- match("# Fcast_recruitments:", ParFile)
      # Line_error <- match("# Fcast_impl_error:", ParFile)
      ParFile[Line + 1] <- gsub(",", "", toString(Recruit_forecast))
      # ParFile[Line_error + 1] <- gsub(",", "", toString(Recruit_forecast * 0.0))
      writeLines(ParFile, paste0(dir_OM_MSY, "/ss3.par"))
      
      # change forecast file
      Forecast <- r4ss::SS_readforecast(paste0(dir_OM_Final, "forecast.ss"), verbose = FALSE)
      Forecast$MSY <- 2 # calculate FMSY
      Forecast$Nforecastyrs <- length(Recruit) + 1 # change forecast No. of recruitment
      Forecast$ControlRuleMethod <- 0 # Harvest control rule method
      Forecast$First_forecast_loop_with_stochastic_recruitment <- 1
      Forecast$Forecast <- 2 # Fforecast = MSY
      
      r4ss::SS_writeforecast(Forecast, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)
      
      # change starter file
      starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
      starter$F_std_basis <- 122
      r4ss::SS_writestarter(starter, dir_OM_MSY, verbose = FALSE, overwrite = TRUE)
      
      # run the OM
      command <- paste("cd", dir_OM_MSY, "& go_nohess.bat", sep = " ")
      ss <- shell(cmd = command, intern = T, wait = T)
      
      om_out = r4ss::SS_output(dir = dir_OM_MSY, covar = F, verbose = FALSE, printstats = FALSE)
      TS <- om_out$timeseries[which(om_out$timeseries$Era == "FORE"), ]
      SMSY <- data.frame("Yr" = TS$Yr, "SMSY" = TS$SpawnBio)
      
      step8$SMSY <- SMSY$SMSY
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
    # "Time_Stamp" = Time_ts,
    "Fratio" = Fratio_ts,
    "SB" = SB_ts,
    "FFMSY" = FFMSY_ts,
    "Implementation_Error" = IE_ts
  )
  
  write.csv(Record,
            file = paste0(dir_itr, "Record.csv"),
            row.names = FALSE)
}