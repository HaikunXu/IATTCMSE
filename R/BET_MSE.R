#' Runs the BET MSE framework for the specified number of iterations
#'
#' @param pdir parent directory path
#' @param sdir benchmark assessment model directory path
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM the name of the operating model
#' @param itr iteration number
#' @param nquarters total number of new quarters to be simulated in the MSE
#' @param Mcycle the number of years within a management cycle
#' @param nextra the number of recruitment devs after the main R and before the forecast R 
#' 
#' @author Haikun Xu
#' @export

BET_MSE = function(pdir, sidr, HS, HCR, OM, itr, nquarters, Mcycle, n_extra_R) { 

  #Specify vectors where to save output (output is from OM unless otherwise specified) for the future simulation years
  
  #set working directory
  dir.create(paste0(pdir, HS, HCR, OM))
  setwd(paste0(pdir, HS, HCR, OM))
  
  # *************************************************************************************
  # Step 0: get the OM from the 2024 assessment
  # *************************************************************************************
  
  # first get the OM from the 2024 benchmark assessment with F30% reference points 
  # dir_OM_benchmark <- "D:/OneDrive - IATTC/IATTC/2024/SAC15/Assessment/Model/2023/Fix-1-1"
  # dir_EM_MSE <- paste0(pdir, HS, HCR, OM, "itr0")
  # dir.create(dir_EM_MSE)
  
  # change to F30% estimates in forecast and starter files
  # change relative year range in the control file
  # run the model again to get new files
  
  # create and set directory for each iteration (i.e. different recruitment)
  dir.create(paste0(pdir, HS, HCR, OM, itr))

  nsteps <- nquarters / 4 / Mcycle
  R_devs <- read.csv(paste0(pdir,"R_devs.csv"))[, itrnum] # R devs for the itr iteration
  CurrentClosure <- 72
  
  Fadjust_ts <- rep(NA, nsteps)
  SBR_d_ts <- rep(NA, nsteps)
  max_gradient_ts <- rep(NA, nsteps)
  Closure_ts <- rep(NA, nsteps)
  
  for (istep in 1:nsteps){
    
    print(paste0("istep = ",istep))
    # create directory for new time step where the new dat file will be saved
    dir_istep <- paste0(pdir, HS, HCR, OM, itr, "step", istep, "/")
    dir.create(dir_istep)
    
    # *************************************************************************************
    # Step 1: Project the OM forward for 12 quarters
    # *************************************************************************************
    
    #### OM_projection
    
    # step 1: create a new folder for the OM
    dir_OM <- paste0(dir_istep, "OM/")
    dir.create(dir_OM)
    
    if(istep == 1) {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, "itr0/")
      dir_EM_previous <- paste0(pdir, HS, "EM/")
    }
    else {
      dir_OM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/OM_Boot/")
      dir_EM_previous <- paste0(pdir, HS, HCR, OM, itr, "step", istep - 1, "/EM/")
    }
    
    files = c(
      paste0(dir_OM_previous, "BET-EPO.ctl"),
      paste0(dir_EM_previous, "BET-EPO_all.dat"),
      paste0(dir_OM_previous, "ss.exe"),
      paste0(dir_OM_previous, "go_nohess.bat")
    )
    file.copy(from = files, to = dir_OM, overwrite = TRUE)
    
    file.rename(from = paste0(dir_OM, "BET-EPO_all.dat"), to = paste0(dir_OM, "BET-EPO.dat")) # load the full dataset
    
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
      R_forecast_new <- c(R_devs[((istep-1)*Mcycle*4+1):(istep*Mcycle*4)], 0)
    else 
      R_forecast_new <- c(as.vector(t(R_forecast[1,1:n_extra_R])), 
                          R_devs[((istep-1)*Mcycle*4+1):(istep*Mcycle*4)],
                          0)
    
    ParFile[Line + 1] <- gsub(",", "", toString(R_forecast_new)) # do R bias adjustment???
    writeLines(ParFile, paste0(dir_OM, "/ss3.par"))
    
    # step 3: change forecast file
    ForecastDir <- paste0(dir_OM_previous, "forecast.ss")
    ForecastFile <- readLines(ForecastDir, warn = F)
    
    if(istep == 1) {
      results <- HCR_staff(dir_EM = paste0(pdir, HS, "EM/"), CurrentClosure)
      
      SBR_d_ts[1] <- results$SBR_d
      Fadjust_ts[1] <- results$Fadjust
      CurrentClosure <- results$NewClosure
      Closure_ts[1] <- results$NewClosure
      Fscale <- results$Fscale
    }
    
    ForecastFile[12] <- 5 # use F scaler
    ForecastFile[14] <- Fscale # input F scaler
    ForecastFile[13] <- 1 + Mcycle * 4 # number of forecast years
    
    writeLines(ForecastFile, paste0(dir_OM, "/forecast.ss"))
    
    # step 4: change starter file
    StarterDir <- paste0(dir_EM_previous, "/starter.ss")
    StarterFile <- readLines(StarterDir, warn = F)
    StarterFile[15] <- 0 # do not estimate
    StarterFile[6] <- 1 # 0=use init values in control file; 1=use ss.par
    
    writeLines(StarterFile, paste0(dir_OM, "/starter.ss"))
    
    # step 5: run ss
    # setwd(dir_OM)
    command <- paste("cd", dir_OM, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    
    # *************************************************************************************
    # Step 2: Change the data files of the updated OM to run bootstrap
    # *************************************************************************************
    
    # step 1: create a new folder for the OM bootstrap
    dir_OM_Boot <- paste0(dir_istep, "OM_Boot/")
    dir.create(dir_OM_Boot)
    
    # copy files to the new folder
    files = c(
      paste0(dir_OM, "starter.ss"),
      paste0(dir_OM, "ss.exe"),
      paste0(dir_OM, "go_nohess.bat")
    )
    file.copy(from = files, to = dir_OM_Boot, overwrite = TRUE)
    
    # read the report file from the OM projection
    om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)
    r4ss::SS_plots(replist=om_out, forecastplot=T, uncertainty=F, datplot=T, plot = c(3, 4, 11), verbose = FALSE)
    
    # read projected catch
    Catch_projection <- r4ss::SS_ForeCatch(om_out,
                                           yrs = ((istep - 1) * Mcycle * 4 + 197):(istep * Mcycle * 4 + 196),
                                           zeros = TRUE)
    
    # read catch file
    dat <- r4ss::SS_readdat_3.30(file = paste0(dir_OM, "BET-EPO.dat"), verbose = FALSE)
    Catch <- dat$catch
    
    Catch_projection_new <- data.frame("year" = Catch_projection$`#Year`,
                                       "seas" = Catch_projection$Seas,
                                       "fleet" = Catch_projection$Fleet,
                                       "catch" = Catch_projection$`dead(B)`,
                                       "catch_se" = 0.01)
    
    Catch_new <- dplyr::arrange(rbind(Catch, Catch_projection_new), fleet, year)
    
    dat$catch <- Catch_new
    dat$endyr <- istep * Mcycle * 4 + 196
    
    # add dummy CPUE data
    CPUE_new <- dat$CPUE[(nrow(dat$CPUE) - Mcycle * 4 + 1):nrow(dat$CPUE),]
    CPUE_new$year <- CPUE_new$year + Mcycle * 4
    CPUE_new$se_log <- dat$CPUE$se_log[nrow(dat$CPUE)]
    dat$CPUE <- rbind(dat$CPUE, CPUE_new)
    
    # add dummy LF data
    # LF <- dat$sizefreq_data_list[[1]] # [which(dat$sizefreq_data_list$)]
    
    # LF_survey <- LF[which(LF$fleet==23),]
    
    r4ss::SS_writedat_3.30(dat, paste0(dir_OM_Boot, "BET-EPO.dat"), verbose = FALSE, overwrite = TRUE)
    
    # change recruitment period in the control file
    ctl <- r4ss::SS_readctl_3.30(
      file = paste0(dir_OM, "/BET-EPO.ctl"),
      verbose = FALSE,
      datlist = dat,
      use_datlist = TRUE
    )
    ctl$MainRdevYrLast <- ctl$MainRdevYrLast + Mcycle * 4 # increase the main recruitment last year
    
    r4ss::SS_writectl_3.30(
      ctl,
      outfile = paste0(dir_OM_Boot, "/BET-EPO.ctl"),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    
    # change forecast file
    ForecastDir <- paste0(dir_OM, "forecast.ss")
    ForecastFile <- readLines(ForecastDir, warn = F)
    
    ForecastFile[13] <- 1 # 1 year-quarters
    
    writeLines(ForecastFile, paste0(dir_OM_Boot, "/forecast.ss"))
    
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
    
    R_main_new <- cbind(R_main, R_forecast[1,1:(Mcycle*4)])
    R_forecast_new <- R_forecast[1,(Mcycle*4+1):ncol(R_forecast)]
    
    ParFile[Line_main + 1] <- gsub(",", "", toString(R_main_new))
    ParFile[Line_forecast + 1] <- gsub(",", "", toString(R_forecast_new))
    
    writeLines(ParFile, paste0(dir_OM_Boot, "/ss3.par"))
    
    # set up bootstrap
    starter_boot <- r4ss::SS_readstarter(paste0(dir_OM_Boot, "starter.ss"), verbose = FALSE)
    
    #specify to use the ss3.par as parameters
    starter_boot$init_values_src = 1
    #turn off estimation of parameters 
    starter_boot$last_estimation_phase = 0
    #add 1 data bootstrap file
    starter_boot$N_bootstraps = 3
    
    #write new starter file
    r4ss::SS_writestarter(starter_boot, dir_OM_Boot, verbose = FALSE, overwrite = TRUE)
    
    # run the bootstrap model
    
    # !!! need to add a seed later
    command <- paste("cd", dir_OM_Boot, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    
    # *************************************************************************************
    # Step 3: Estimation model
    # *************************************************************************************
    
    # step 1: create a new folder for the OM bootstrap
    dir_EM <- paste0(dir_istep, "EM/")
    dir.create(dir_EM)
    
    # copy files to the new folder
    files = c(
      paste0(dir_EM_previous, "starter.ss"),
      paste0(dir_EM_previous, "ss.exe"),
      paste0(dir_EM_previous, "go_nohess.bat"),
      paste0(dir_EM_previous, "forecast.ss")
    )
    file.copy(from = files, to = dir_EM, overwrite = TRUE)
    
    # change data file
    dat_EM_previous <- r4ss::SS_readdat_3.30(file = paste0(dir_EM_previous, "BET-EPO.dat"), verbose = FALSE)
    dat_EM_previous_all <- r4ss::SS_readdat_3.30(file = paste0(dir_EM_previous, "BET-EPO_all.dat"), verbose = FALSE)
    data_boot <- r4ss::SS_readdat_3.30(file = paste0(dir_OM_Boot, "data_boot_001.ss"), verbose = FALSE)
    
    # add new CPUE
    CPUE <- dat_EM_previous$CPUE
    CPUE_boot <- data_boot$CPUE[(nrow(CPUE)+1):nrow(data_boot$CPUE),]
    CPUE_new <- rbind(CPUE, CPUE_boot)
    
    # add new catch
    catch <- dat_EM_previous$catch
    catch_boot <- data_boot$catch[which(data_boot$catch$year>max(catch$year)),]
    catch_new <- dplyr::arrange(rbind(catch, catch_boot), fleet, year)
    
    # save data file
    dat_EM_previous$catch <- catch_new
    dat_EM_previous$CPUE <- CPUE_new
    dat_EM_previous$endyr <- dat_EM_previous$endyr + Mcycle * 4
    r4ss::SS_writedat_3.30(dat_EM_previous, paste0(dir_EM, "BET-EPO.dat"), verbose = FALSE, overwrite = TRUE)
    
    dat_EM_previous_all$catch <- catch_new
    dat_EM_previous_all$CPUE <- CPUE_new
    dat_EM_previous_all$endyr <- dat_EM_previous_all$endyr + Mcycle * 4
    r4ss::SS_writedat_3.30(dat_EM_previous_all, paste0(dir_EM, "BET-EPO_all.dat"), verbose = FALSE, overwrite = TRUE)
    
    # change control file
    ctl <- r4ss::SS_readctl_3.30(
      file = paste0(dir_EM_previous, "/BET-EPO.ctl"),
      verbose = FALSE,
      datlist = dat,
      use_datlist = TRUE
    )
    ctl$MainRdevYrLast <- ctl$MainRdevYrLast + Mcycle * 4 # increase the main recruitment last year
    
    r4ss::SS_writectl_3.30(
      ctl,
      outfile = paste0(dir_EM, "/BET-EPO.ctl"),
      overwrite = TRUE,
      verbose = FALSE
    )
    
    # change par file by adding 12 more main R devs
    ParDir <- paste0(dir_EM_previous, "ss3.par")
    ParFile <- readLines(ParDir, warn = F)
    
    Line_main <- match("# recdev2:", ParFile)
    R_main <- read.table(
      file = ParDir,
      nrows = 1,
      skip = Line_main
    )
    
    R_main_new <- c(R_main, rep(0, Mcycle * 4))
    ParFile[Line_main + 1] <- gsub(",", "", toString(R_main_new))
    
    writeLines(ParFile, paste0(dir_EM, "/ss3.par"))
    
    # run the estimation model
    command <- paste("cd", dir_EM, "& go_nohess.bat", sep = " ")
    ss <- shell(cmd = command, intern = T, wait = T)
    
    
    #****************************************************************************
    #Step 5: Compute dynamic depletion using EM model output
    
    results <- HCR_staff(dir_EM = dir_EM, CurrentClosure, plot = c(3, 4, 11))
    
    SBR_d_ts[istep] <- results$SBR_d
    Fadjust_ts[istep] <- results$Fadjust
    CurrentClosure <- results$NewClosure
    Closure_ts[istep + 1] <- results$NewClosure
    Fscale <- results$Fscale
    max_gradient_ts[istep] <- results$max_gradient
    if(istep != nsteps) Fadjust_ts[istep + 1] <- results$Fadjust
    if(results$max_gradient > 0.1) break # large gradient - the model does not converge
    
  }
  
  #
  
  # #****************************************************************************
  # #Step 6: Run the OM on the last time step with the data with no error
  # 
  # Path = paste(pdir, hs, hcr, scn, itr, "/", istep,"/OM/", sep="")
  # filename_om  <-paste(Path,"ssnohess.bat",sep="")
  # batchtext_om = paste(pwin,"SS_model//ss3.exe -nohess -cbs 500000000",sep="")
  # writeLines(batchtext_om,filename_om)
  # 
  # setwd(Path)
  # command_run_om="ssnohess.bat"
  # shell(cmd= command_run_om)
  # 
  # #****************************************************************************
  # #Step 7: Extract from the OM data relevant to calculation of performance metrics
  # 
  # out_dir = paste(pdir, hs, hcr, scn, itr,"/",istep, "/OM/", sep="")
  # om_out = SS_output(out_dir, covar = FALSE, ncols = 250)
  # 
  # #Extract spr series quantities from the SS output
  # derquant = om_out$sprseries
  # 
  # #start of simulation
  # st=length(1993:2015)+1
  # #extract from spr series quantities relevant to performance metrics
  # Rdat[1:(tasmt*istep)] = derquant$Recruits[st:length(derquant$Recruits)]
  # SPBdat[1:(tasmt*istep)] = derquant$SSB[st:length(derquant$SSB)]
  # Ddat[1:(tasmt*istep)] = derquant$Deplete[st:length(derquant$Deplete)]
  # SPRdat[1:(tasmt*istep)] = derquant$SPR[st:length(derquant$SPR)]
  # Btot[1:(tasmt*istep)] = derquant[st:length(derquant$SPR),15] #Bio_Smry
  # Tdat[1:(tasmt*istep)] = derquant$Retain_Catch[st:length(derquant$SPR)]
  # Ftgt_om [1:(tasmt*istep)] = rep(om_out$derived_quants$Value[which(om_out$derived_quants$Label == "Fstd_SPRtgt")],(tasmt*istep))
  # 
  # #extract the catch by fleet and season and year 
  # catch = om_out$catage
  # catch[,27] = rowSums(catch[,11:26])
  # 
  # #extract the terminal year
  # yr_end = om_out$endyr
  # 
  # #select the catch from 2015 onwards
  # catch_lat = catch %>% filter(Yr> 2015) 
  # 
  # #sum catch across seasons
  # catch_sum = as.data.frame(catch_lat %>% group_by(Yr,Fleet) %>% summarise(cat=sum(V27)))
  # 
  # #set to wide format to match other data, so that year is in rows and column is the catch by fishery
  # catch_wide <- reshape2::dcast(catch_sum, Yr ~ Fleet, value.var="cat")
  # 
  # Cdat[1:(tasmt*istep),] = as.matrix(catch_wide)
  # 
  # #extract dynamic bzero data
  # DBzero = om_out$Dynamic_Bzero
  # DBzero_lat = DBzero %>% filter(Yr>2015)
  # B0dat_om[1:(tasmt*istep)] = DBzero_lat$SSB_nofishing
  # B0t_om[1:(tasmt*istep)] = (om_out$derived_quants %>% filter(Label == "SmryBio_Unfished"))$Value
  # 
  # 
  # #Combine all output into a list
  # outmat = data.frame(Year = 2016:2045, R = Rdat, Rem = R_em, SSB = SPBdat, SSBem = SPB_em, Depletion = Ddat, Dem = D_em, SPR = SPRdat, SPRem = SPR_em, Catch = Tdat, Cem = C_em, TAC = as.numeric(TACdt), TACi = as.numeric(TACi), LRPem = B0dat, LRPom = B0dat_om, Ftgtom = Ftgt_om, Ftgtem = Ftgt_em,B0tem=B0t,B0tom=B0t_om,Btot=Btot, F1 = Cdat[,2], F10 = Cdat[,11],F11 = Cdat[,12], F12 = Cdat[,13], F13 = Cdat[,14], F14 = Cdat[,15], F15 = Cdat[,16], F16 = Cdat[,17], F17 = Cdat[,18], F18 = Cdat[,19], F19 = Cdat[,20], F2 = Cdat[,3], F20 = Cdat[,21], F21 = Cdat[,22], F22 = Cdat[,23],F23 = Cdat[,24], F24 = Cdat[,25], F25 = Cdat[,26], F26 = Cdat[, 27], F27 = Cdat[, 28], F28 = Cdat[, 29], F29 = Cdat[, nquarters], F3 = Cdat[,4], F4 = Cdat[, 5], F5 = Cdat[, 6], F6 = Cdat[, 7], F7=Cdat[, 8], F8 = Cdat[, 9], F9 = Cdat[, 10])
  # outlist= list(outmat=outmat)
  # 
  # #save output to file
  # write.table(outlist, paste(pdir,hs, hcr,scn, itr,"/outlist.txt", sep =""))
  # 
  # return(outmat)
}



