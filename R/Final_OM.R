#' Make the final OM for extracting MSE results
#'
#' @param dir_istep the directory of step i
#' @param istep step number
#' @param dir_OM the directory of OM
#' @param Mcycle the number of years within a management cycle
#' 
#' @author Haikun Xu 
#' @export

Final_OM = function(dir_istep, istep, dir_OM, Mcycle) {
  
  # step 1: create a new folder for the OM bootstrap
  dir_OM_Final <- paste0(dir_istep, "OM_Final/")
  dir.create(dir_OM_Final)
  
  # copy files to the new folder
  files = c(
    paste0(dir_OM, "starter.ss"),
    paste0(dir_OM, "ss.exe"),
    paste0(dir_OM, "go_nohess.bat")
  )
  file.copy(from = files, to = dir_OM_Final, overwrite = TRUE)
  
  # read the report file from the OM projection
  om_out = r4ss::SS_output(dir = dir_OM, covar = F, verbose = FALSE, printstats = FALSE)
  
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
  
  r4ss::SS_writedat_3.30(dat, paste0(dir_OM_Final, "BET-EPO.dat"), verbose = FALSE, overwrite = TRUE)
  
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
    outfile = paste0(dir_OM_Final, "/BET-EPO.ctl"),
    overwrite = TRUE,
    verbose = FALSE
  )
  
  
  # change forecast file
  ForecastDir <- paste0(dir_OM, "forecast.ss")
  ForecastFile <- readLines(ForecastDir, warn = F)
  
  ForecastFile[13] <- 1 # 1 year-quarters
  
  writeLines(ForecastFile, paste0(dir_OM_Final, "/forecast.ss"))
  
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
  
  writeLines(ParFile, paste0(dir_OM_Final, "/ss3.par"))
  
  # starter file
  starter <- r4ss::SS_readstarter(paste0(dir_OM_Final, "starter.ss"), verbose = FALSE)
  
  #specify to use the ss3.par as parameters
  starter$init_values_src = 1
  #turn off estimation of parameters 
  starter$last_estimation_phase = 0
  #
  starter$maxyr_sdreport <- istep * Mcycle * 4 + 197

  #write new starter file
  r4ss::SS_writestarter(starter, dir_OM_Final, verbose = FALSE, overwrite = TRUE)
  
  # run the OM
  command <- paste("cd", dir_OM_Final, "& go_nohess.bat", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
  
  return(dir_OM_Final)
}