#' @title check sides
#' @description checks the validity of the argument sides
#' @param sides a vector of sides
#' @return boolean value
check_sides <- function(sides) {
  if (length(sides) != 6) {
    stop("\n'sides' must be a vector of length 6")
  }
  TRUE
}

#' @title check probabilities
#' @description checks the validity of the argument prob
#' @param prob a vector of prob
#' @return boolean value
check_prob <- function(prob) {
  if (length(prob) != 6 | !is.numeric(prob)) {
    stop("\n'prob' must be a vector of length 6")
  }
  if (any(prob < 0) | any(prob > 1)) {
    stop("\n'prob' values must be between 0 and 1")
  }
  if (sum(prob) != 1) {
    stop("\nelements in 'prob' must add up to 1")
  }
  TRUE
}

#' @title print method for die
#' @description displays the class of the object, and a tabular display of the sides and prob
#' @param die a die object
#' @export
print.die <- function(die) {
  cat('object "die"\n\n')
  cd <- data.frame(
    side = die$sides, prob = die$prob
  )
  print(cd)
  invisible(die)
}

# all auxiliary functions must be named before calling function die
#' @title die
#' @description creates a standard fair die by default
#' @param sides vector of six elements, by default numbers 1-6
#' @param prob vector of probabilities for each side (1/6 by default)
#' @return the object die
#' @export
die <- function(sides = c(1, 2, 3, 4, 5, 6), prob = c(rep(1 / 6, 6))) {
  check_sides(sides)
  check_prob(prob)
  res <- list(sides = sides, prob = prob)
  class(res) <- "die"
  return(res)
}

fair_die <- die()
fair_die

weird_die <- die(sides = c('i', 'ii', 'iii', 'iv', 'v', 'vi'))
weird_die

loaded_die <- die(prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35))
loaded_die

#bad_die <- die(sides = c('a', 'b', 'c', 'd', 'e'))
#bad_die

#bad_die_prob <- die(
#  sides = c('a', 'b', 'c', 'd', 'e', 'f'),
#  prob = c(0.2, 0.1, 0.1, 0.1, 0.5, 0.1)
#)
#bad_die_prob