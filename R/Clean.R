#' Clean unused folders to save space
#'
#' @param pdir parent directory path
#' @param sdir benchmark assessment model directory path
#' @param HS the name of the harvest strategy
#' @param HCR the name of the harvest control rule
#' @param OM the name of the operating model
#' @param itrnum iteration number
#' @param nquarters total number of new quarters to be simulated in the MSE
#' @param Mcycle the number of years within a management cycle
#' 
#' @author Haikun Xu 
#' @export

Clean = function(pdir, sdir, HS, HCR, OM, itrnum, nquarters, Mcycle) { 
  
  itr = paste0("itr", itrnum, "/")
  nsteps <- nquarters / 4 / Mcycle
  
  if (clean == TRUE) {
    for (istep in 1:nsteps) {
      unlink(paste0(pdir, HS, HCR, OM, itr, "step", istep), recursive = TRUE)
    }
  }
  
  dir_OM_Final <- paste0(dir_itr, "OM_Final/")
  command <- paste("cd", dir_OM_Final, "& CLEAN.BAT", sep = " ")
  ss <- shell(cmd = command, intern = T, wait = T)
}