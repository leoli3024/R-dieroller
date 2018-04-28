#' @title print method for summary.roll
#' @description displays the class of the object, and the data frame freqs
#' @param die a summary.roll object
#' @export
print.summary.roll <- function(die) {
  cat('summary "roll"\n\n')
  print(die$freqs)
  invisible(die)
}

#' @title summary.roll
#' @description a summary method for roll objects
#' @param die a roll object
#' @return the summary.roll object
#' @output a dataframe called "freqs" with side, count & prop as columns
#' @export
summary.roll <- function(die) {
  count = 1:length(die$sides);
  for (i in 1:length(die$sides)) {
    count[i] = sum(die$rolls == die$sides[i])
  }
  df <- data.frame(side = die$sides, count = count, prop = count / die$total)
  res <- list(freqs = df)
  class(res) <- "summary.roll"
  return(res)
}

set.seed(123)
fair_50rolls <- roll(fair_die, times = 50)
fair50_sum <- summary(fair_50rolls)
fair50_sum
class(fair50_sum)
names(fair50_sum)
fair50_sum$freqs