# require(magrittr)
library(testthat)
# main Option --------------------------------------------------------------------

s <- Some$new(value = 2)
p <- None$new()
inverse <- function(x) {
	if (x == 0) stop("division by 0") # force an exception
	1/x
}
# function that manage the exception
inverse_Option <- function(x) {
	if (x == 0) return(None$new())
	Some$new(value = 1/x)}

test_that("Option map, flatMap, lift", {
	expect_equal(s$map(inverse), Some$new(value = 0.5))
  expect_equal(s$flatMap(inverse_Option), Some$new(value = 0.5))
  expect_equal(s$lift(inverse)(s), Some$new(value = 0.5))
  expect_equal(p$map(inverse), None$new())
  expect_equal(p$flatMap(inverse_Option), None$new())
  expect_equal(p$lift(inverse)(p), None$new())
})


test_that("Option fold method", {
	expect_equal(
		inverse_Option(2)$
			fold(function() "cannot divide by 0",
					 function(x) paste("the result is", x) ),
		"the result is 0.5")

	expect_equal(
		inverse_Option(0)$
			fold(function() "cannot divide by 0",
					 function(x) paste("the result is", x) ),
		"cannot divide by 0")
})

test_that("Option$ap", {
	# Option<A -> B>
	fab <- Some$new(value = function(a) a * 3)

	expect_equal(s$ap(fab),
							 fab$map( function(f) f(s$value) ))

	expect_error(s$ap(s)) # type problem
})

test_that("Option$ap can do the lift", {

	sum <-
		function(a)
			function(b) { a + b }


	of <- function(o) { Some$new(o) }

	sumOption <- function( fa, fb ) {
	 	fb$ap(fa$ap(of(sum)))
	}
	aO <- of(3)
	bO <- of(2)

	sumOption(aO, bO)


})
