#' @title make roll
#' @description an auxiliary function to put informatoin of a roll together
#' @param die a die object
#' @param rolls vector of rolling outcomes
#' @param times a number times
#' @return a list of information containing: rolls, sides, prob & total
make_roll <- function(die, rolls, times) {
  res <- list(
    rolls = rolls,
    sides = die$sides, # info from object die
    prob = die$prob, # info from object die
    total = times)
  class(res) <- "roll"
  res
}

#' @title check times
#' @description checks the validity of the argument times
#' @param times a number times
#' @return whether times is valid
check_times <- function(times) {
  if (times <= 0 | !is.numeric(times)) {
    stop("\nargument 'times' must be a positive integer")
  } else {
    TRUE
  }
}

#' @title print method for roll
#' @description displays the class of the object, and the generated rolls
#' @param die a roll object
#' @export
print.roll <- function(die) {
  cat('object "roll"\n\n')
  cat('$rolls\n')
  print(die$rolls)
  invisible(die)
}

# all auxiliary functions must be named before calling function die
#' @title roll
#' @description a constructor function that makes a roll
#' @param die a die object
#' @param times number of times to roll the die
#' @return the object of class "rolls"
#' @export
roll <- function(die, times = 1) {
  check_times(times)
  rolls <- sample(die$sides, size = times, replace = TRUE, prob = die$prob)
  make_roll(die, rolls, times)
}

fair_die <- die()
set.seed(123)
fair50 <- roll(fair_die, times = 50)
fair50
names(fair50)
fair50$rolls
fair50$sides
fair50$prob
fair50$total

str_die <- die(
  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
  prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35)
)
set.seed(123)
str_rolls <- roll(str_die, times = 20)
names(str_rolls)
str_rolls