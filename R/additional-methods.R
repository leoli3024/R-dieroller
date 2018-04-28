#' @title extraction
#' @description extract the value of a given roll
#' @param roll a roll object
#' @param i index of element to be extracted
#' @return new roll
#' @export
"[.roll" <- function(roll, i) {
  roll$rolls[i]
}

#' @title replacement
#' @description replace the value of a given roll
#' @param roll a roll object
#' @param i index of element to be replaced
#' @param value value of element to replace
#' @return new roll
#' @export
"[<-.roll" <- function(roll, i, value) {
  if (sum(value == roll$sides) == 0) {
    stop(sprintf('\nreplacing value must be %s to %s',
                 roll$sides[1], roll$sides[length(roll$sides)]))
  }
  if (i > roll$total) {
    stop("\nindex out of bounds")
  }
  roll$rolls[i] <- value
  make_roll(die(roll$sides, roll$prob), roll$rolls, roll$total)
}

#' @title addition
#' @description add more rolls
#' @param obj a roll object
#' @param incr number of rolls to add
#' @return new roll
#' @export
"+.roll" <- function(obj, incr) {
  if (length(incr) != 1 | incr <= 0) {
    stop("\ninvalid increament (must be positive)")
  }
  more_rolls <- roll(die(obj$sides, obj$prob), times = incr)
  make_roll(die(obj$sides, obj$prob), c(obj$rolls, more_rolls$rolls), obj$total + incr)
}

set.seed(123)
fair_die <- die()
fair500 <- roll(fair_die, times = 500)
summary(fair500)
fair500[500]
fair500[500] <- 1
fair500[500]
summary(fair500)
fair600 <- fair500 + 100
summary(fair600)
plot(fair500, 500)

