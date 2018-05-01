
library(testthat)
# main Option --------------------------------------------------------------------
GenericError <- "GenericError"
r <- Right$new(value = 2)
l <- Left$new(value = GenericError)
inverse <- function(x) {
	if (x == 0) stop("division by 0") # force an exception
	1/x
}
# function that manage the exception
# inverse_Option <- function(x) {
# 	if (x == 0) return(Left$new())
# 	Right$new(value = 1/x)}

inverse_Either <- function(x) {
	if (x == 0) return(Left$new(value = "Division by 0"))
	Right$new(value = 1/x)
}
# #
test_that("Either map, flatMap, lift", {
	expect_equal(1, 2)
	expect_equal(r$map(inverse), Right$new(value = 0.5))
  expect_equal(r$flatMap(inverse_Either), Right$new(value = 0.5))
  expect_equal(r$lift(inverse)(r), Right$new(value = 0.5))
  expect_equal(l$map(inverse), Left$new(value = GenericError))
  expect_equal(l$flatMap(inverse_Option), Left$new(value = GenericError))
  expect_equal(l$lift(inverse)(l), Left$new(value = GenericError))
})

test_that("Option fold method", {
	expect_equal(
		inverse_Either(2)$
			fold(function(m) m,
					 function(x) paste("the result is", x) ),
		"the result is 0.5")

	expect_equal(
		inverse_Either(0)$
			fold(function(m) m,
					 function(x) paste("the result is", x) ),
		"Division by 0")
})

test_that("Option$ap", {
	fab <- Right$new(value = function(a) a * 3)

	expect_equal(r$ap(fab),
							 fab$map( function(f) f(r$value) ))

	expect_error(r$ap(r)) # type problem
})
