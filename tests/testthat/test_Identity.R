context("Identity")

test_that("Identity of the identity", {
	id <- function(x) x
	i <- Identity$new(value = 3)

	expect_equal( i$map(id), i)

})
