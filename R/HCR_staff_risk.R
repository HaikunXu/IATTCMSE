#' Runs the staff's HCR algorithm
#'
#' @param dir_EM the directory of the EM used to compute the current SBR_d for harvest control rule
#' @param CurrentClosure the current number of closure days
#' 
#' @author Haikun Xu 
#' @export

HCR_staff_risk = function(dir_EM, istep, CurrentClosure) {
  
  SBR_d_em <- rep(NA, 4)
  Fmult_em <- rep(NA, 4)
  Frecent_em <- rep(NA, 4)
  SB_em <- rep(NA, 4)
  max_gradient_em <- rep(NA, 4)
  
  for (m in 1:4) {
    # read EM output file
    em_out <- r4ss::SS_output(dir_EM[m], covar = FALSE, verbose = FALSE, printstats = FALSE)
    
    # max gradient
    max_gradient_em[m] <- em_out$maximum_gradient_component
    
    # dynamic SBR
    Dynamic_Bzero <- em_out$Dynamic_Bzero
    SBR_d_em[m] <- Dynamic_Bzero$SSB[nrow(Dynamic_Bzero)] / Dynamic_Bzero$SSB_nofishing[nrow(Dynamic_Bzero)]
    SB_em[m] <- Dynamic_Bzero$SSB[nrow(Dynamic_Bzero)]
    
    # get Fmult
    ForeRepName <- paste(dir_EM[m], "Forecast-report.SSO", sep = "")
    
    # Get management report
    ForeRepStart <- grep("Management_report", readLines(ForeRepName))
    ForeRepEnd <- grep("THIS FORECAST IS FOR PURPOSES", readLines(ForeRepName))[1]
    
    ForeDat <- read.table(file = ForeRepName, col.names = c(seq(1, 10, by = 1)), fill = T, quote = "", colClasses = "character", 
                          nrows = ForeRepEnd - ForeRepStart, skip = ForeRepStart - 1)
    ForeDat <- as.data.frame(ForeDat)
    
    # get F30%
    Fmult_em[m] <- as.numeric(ForeDat[ForeDat[, 1] == c("Fmult"), 2])[3] # F30%
    
    # get current F
    FvectorRepStart <- grep("Seasonal_apicalF=Fmult", readLines(ForeRepName))
    Fvector <- read.table(file = ForeRepName, nrows = 1, skip = FvectorRepStart[1] + 1)
    Fvector <- Fvector[3:length(Fvector)]
    Frecent_em[m] <- sum(Fvector) # F
  }
  
  SBR_d <- mean(SBR_d_em)
  Fmult <- mean(Fmult_em)
  Frecent <- mean(Frecent_em)
  SB <- mean(SB_em)
  max_gradient <- max(max_gradient_em)
  
  # Find FHCR from the estimated Sbio using the HCR
  Fadjust <- min(5 * SBR_d, 1)
  
  # Check the Fscale with the 10 days maximum and re-adjust with Fscale = current opening +- 10 days / current opening
  Fratio <- Fmult * Fadjust / Frecent # Fnew = Fmult * Fadjust
  NewClosure <- max(365 - (365 - CurrentClosure) * Fratio, 0)
  
  # if(SBR_d >= 0.2) {
    if ((CurrentClosure - NewClosure) > 10) {
      NewClosure <- CurrentClosure - 10
      Fratio <- (365 - NewClosure) / (365 - CurrentClosure)
      # Fadjust <- Fratio * Frecent / Fmult
    }
    
    if ((NewClosure - CurrentClosure) > 10) {
      NewClosure <- CurrentClosure + 10
      Fratio <- (365 - NewClosure) / (365 - CurrentClosure)
      # Fadjust <- Fratio * Frecent / Fmult
    }
  # }
  # else {
  #   if ((CurrentClosure - NewClosure) > 30) {
  #     NewClosure <- CurrentClosure - 30
  #     Fratio <- (365 - NewClosure) / (365 - CurrentClosure)
  #     Fadjust <- Fratio * Frecent / Fmult
  #   }
  #   
  #   if ((NewClosure - CurrentClosure) > 30) {
  #     NewClosure <- CurrentClosure + 30
  #     Fratio <- (365 - NewClosure) / (365 - CurrentClosure)
  #     Fadjust <- Fratio * Frecent / Fmult
  #   }
  # }
  
  # Fscale <- Fmult * Fadjust
  
  return(
    list(
      "SBR_d" = SBR_d,
      # "Fadjust" = Fadjust,
      "NewClosure" = NewClosure,
      "max_gradient" = max_gradient,
      "Fratio" = Fratio,
      "SB" = SB
    )
  )
}