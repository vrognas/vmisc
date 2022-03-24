#' model_table function
#'
#' Gather data from a NONMEM run using xpose::xpose_data(), and output it into a
#' table. By default, it outputs the run number, the reference run number, the
#' contents of `$PROBLEM` (model description), the number of parameters, the OFV,
#' the dOFV from the reference run, and the termination message.
#' Credit: Estelle Chasseloup
#'
#' @param run The run number
#' @param dir Path to the run directory
#' @param col Columns to print
#'
#' @keywords xpose nonmem results table
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' model_table(
#'   run = 163,
#'   dir = "/path/to/run/directory"
#' )
#' }

model_table <- function(run, dir, col = NULL) {
  vec = rep(0, length(run))
  tab <- data.frame(
    Run_nb      = vec,
    Ref         = vec,
    Description = vec,
    Prm_nb      = vec,
    OFV         = vec,
    dOFV        = vec,
    Termination = vec
  )

  for (i in 1:length(run)) {
    if(is.na(as.numeric(run[i])) == TRUE){
      xpdb <- xpose::xpose_data(runno = run[i], dir = dir, prefix = "")
    } else { xpdb <- xpose::xpose_data(runno = run[i], dir = dir)}

    sum <- xpose::get_summary(xpdb)
    prm <- xpose::get_prm(xpdb)
    tab$Run_nb[i] <- run[i]
    tab$OFV[i] <- sum$value[sum$label == "ofv"]
    tab$Prm_nb[i] <- length(prm$fixed[prm$fixed == FALSE])
    tab$Description[i] <- sum$value[sum$label == "label"]
    tab$dOFV[i] <- sum$value[sum$label == "ref"]
    tab$Ref[i] <- sum$value[sum$label == "ref"]
    tab$Termination[i] <- sum$value[sum$label == "term"]
  }

  tab$OFV <- as.numeric(tab$OFV)
  tab$dOFV <- as.numeric(tab$dOFV)

  for (i in 1:length(tab$dOFV)) {
    ref <- tab$dOFV[i]
    if (isTRUE(ref == tab$Run_nb[i])) {
      tab$dOFV[i] <- "Ref"
    } else if (ref %in% run) {
      tab$dOFV[i] <- round(tab$OFV[i] - tab$OFV[tab$Run_nb == ref], 2)
    } else {
      tab$dOFV[i] <- "-"
    }
  }

  if(is.null(col)){
    final <- tab
  } else {
    final <- tab[, c(col)]
  }
  return(final)

}
