#' @title plot method for roll object
#' @description graph a barchart of relative frequencies
#' @param die a roll object
#' @param ... generic arguments
#' @output a barplot
#' @export
plot.roll <- function(die, ...){
  roll_summary = summary(die)
  barplot(roll_summary$freqs$prop,
          names.arg = die$sides,
          ylab = "relative frequencies",
          xlab = "sides of die",
          main = sprintf("Frequencies in a series of %s die rolls", toString(length(die$rolls)))
  )
}

