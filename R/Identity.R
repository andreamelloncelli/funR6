
Identity <- R6::R6Class("Identity",
												public = list(
													value = NULL,
													initialize = function(value) {
														self$value = value
														lockEnvironment(self, bindings = TRUE)
													},
													map = function(f) {
														Identity$new(value = f(self$value))
													}
													# flatMap = function(f) {
													# 	f(self$value)
													# },
													# fold = function(f, g) {
													# 	g(self$value)
													# },
													# ap = function(fab) {
													# 	fab$map(function(f) f(self$value))
													# })
												)
)
