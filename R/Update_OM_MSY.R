#' Update OM's data file with "true" data
#'
#' @param dir_OM the directory of OM
#' @param dir_OM_Boot the directory of the OM bootstrap in the current step
#' @param Mcycle the number of years within a management cycle
#' @param dat_name name of the SS data file
#' 
#' @author Haikun Xu 
#' @export

Update_OM_MSY = function(istep, dir_OM_Final, dir_OM_MSY, dat_name, ss_name, ctl_name) {
  
  dir.create(dir_OM_MSY)
  
  files = c(
    paste0(dir_OM_Final, ctl_name),
    paste0(dir_OM_Final, dat_name),
    paste0(dir_OM_Final, ss_name),
    paste0(dir_OM_Final, "go_nohess.bat"),
    # paste0(dir_OM_Final, "forecast.ss"),
    # paste0(dir_OM_Final, "starter.ss"),
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
  Fvector <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
  Fvector <- Fvector[3:length(Fvector)]
  FmultScale <- sum(Fvector) # F
  
  # Fmultiplier
  Fmult <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3] # FMSY
  FFMSY <- FmultScale/Fmult
  
  # SMSY
  Smsy <- as.numeric(ForeDat[ForeDat[, 1] == c("SSBio"), 2])
  Smsy <- Smsy[length(Smsy)]
  
  # S
  replist <- r4ss::SS_output(dir = dir_OM_MSY, covar = T, printstats = F, verbose = FALSE)
  TimeSeries <- replist$timeseries
  Srecent <- TimeSeries$SpawnBio[nrow(TimeSeries)]
  
  SSMSY <- Srecent / Smsy
  
  return(list("SSMSY" = SSMSY, "FFMSY" = FFMSY))
}