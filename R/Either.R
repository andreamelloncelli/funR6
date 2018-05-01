
# Either ------------------------------------------------------------------

Either <- R6::R6Class("Either",
											public = list(
												lift = function(f) {
													function(this) this$map(f)
												})
)

# # Left --------------------------------------------------------------------

Left <- R6::R6Class("Left",
										inherit = Either,
										public = list(
										 	value = NULL,
										 	initialize = function(value = NULL) {
										 		self$value = value
										 	},
										 	map = function(f) {
												Left$new()
											},
											flatMap = function(f){
												Left$new()
											},
											fold = function(f, g) {
												f()
											},
											ap = function(fab) {
												Left$new()
											})
)
# setMethod("map",
# 					signature("Left", "function"),
# 					function(this, f) {
# 						return(Left(value = f(this@value)))
# 					})
# setMethod(f = "fold",
# 					signature("Left", "function", "function"),
# 					function(this, f, g) {f(this@value)})
# # Right -------------------------------------------------------------------

Right <- R6::R6Class("Right",
										 inherit = Either,
										 public = list(
										 	value = NULL,
										 	initialize = function(value) {
										 		self$value = value
										 	},
										 	map = function(f) {
										 		Right$new(value = f(self$value))
										 	},
										 	# setMethod("map",
										 	# 					signature("Right", "function"),
										 	# 					function(this, f) {
										 	# 						return(Right(value = f(this@value)))
										 	# 					})
										 	flatMap = function(f) {
										 		f(self$value)
										 	},
										 	fold = function(f, g) {
										 		g(self$value)
										 	},
										 	ap = function(fab) {
										 		fab$map(function(f) f(self$value))
										 	})
)
#
# setMethod(f = "fold",
# 					signature("Right", "function", "function"),
# 					function(this, f, g) {g(this@value)})
