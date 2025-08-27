#' Runs the staff's HCR algorithm
#'
#' @param dir_EM the directory of the EM used to compute the current SBR_d for harvest control rule
#' @param CurrentClosure the current number of closure days
#' 
#' @author Haikun Xu 
#' @export

HCR_staff = function(dir_EM, CurrentClosure) {
  
  # read EM output file
  em_out <- r4ss::SS_output(dir_EM, covar = FALSE, verbose = FALSE, printstats = FALSE)
  
  # max gradient
  max_gradient <- em_out$maximum_gradient_component
  
  # dynamic SBR
  Dynamic_Bzero <- em_out$Dynamic_Bzero
  SBR_d <- Dynamic_Bzero$SSB[nrow(Dynamic_Bzero)] / Dynamic_Bzero$SSB_nofishing[nrow(Dynamic_Bzero)]

  # Find FHCR from the estimated Sbio using the HCR
  if (SBR_d > 0.2) Fadjust <- 1
  else Fadjust <- SBR_d / 0.2
  
  # get Fscale for Btarget = 0.3
  Fmult <- IATTCassessment::Fmult(dir_EM)
  Fscale <- Fmult * Fadjust
  
  # Check the Fscale with the 10days maximum and re-adjust with Fscale = current opening +- 10 days / current opening
  NewClosure <- 365 - (365 - CurrentClosure) * Fscale
  
  if ((Fscale > 1) & (CurrentClosure - NewClosure > 10)) {
    NewClosure <- CurrentClosure - 10
    Fscale <- (365 - NewClosure) / (365 - (NewClosure + 10))
    Fadjust <- Fscale / Fmult
  }
  
  if ((Fscale < 1) & (NewClosure - CurrentClosure > 10)) {
    NewClosure <- CurrentClosure + 10
    Fscale <- (365 - NewClosure) / (365 - (NewClosure - 10))
    Fadjust <- Fscale / Fmult
  }
  
  return(list("SBR_d" = SBR_d, "Fscale" = Fscale, "Fadjust" = Fadjust, "NewClosure" = NewClosure, "max_gradient" = max_gradient))

}