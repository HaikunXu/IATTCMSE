#' Extract and save the final OM's output
#' 
#' @author Haikun Xu 
#' @export

Extract_OM = function(dir_OM_Final, startquarter, plot = c(3, 7)) {
  
  library(dplyr)
  # read the report file from the final OM
  om_out = r4ss::SS_output(dir = dir_OM_Final, covar = F, verbose = FALSE, printstats = FALSE)
  r4ss::SS_plots(replist=om_out, uncertainty=F, datplot=T, plot = plot, verbose = FALSE)
  
  Recruit <- om_out$recruit[,c("Yr", "dev", "pred_recr")] %>%
    filter(Yr >= startquarter)
  
  Catch <- om_out$catch %>%
    group_by(Yr) %>%
    summarise(Tot_catch = sum(Obs)) %>%
    filter(Yr >= startquarter)
  
  Dynamic_Bzero <- om_out$Dynamic_Bzero %>%
    mutate(SBR_d = SSB / SSB_nofishing) %>%
    filter(Yr >= startquarter)
  
  SBR_d <- Dynamic_Bzero$SSB[nrow(Dynamic_Bzero)] / Dynamic_Bzero$SSB_nofishing[nrow(Dynamic_Bzero)]
  
  SB <- om_out$timeseries[, c("Yr", "SpawnBio")]
  SB$SBR <- SB$SpawnBio / SB$SpawnBio[which(SB$Yr == (startquarter - 2))]
  
  SB <- SB %>%
    filter(Yr >= startquarter)
  
  Output <- data.frame(
    "Year" = Recruit$Yr,
    "Catch" = c(Catch$Tot_catch,NA),
    "SBR_d" = Dynamic_Bzero$SBR_d,
    "SB" = SB$SpawnBio,
    "SBR" = SB$SBR,
    "Recruit" = Recruit$pred_recr,
    "R_devs" = Recruit$dev
  )
  
  return(Output)
  
}