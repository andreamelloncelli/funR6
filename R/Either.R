
# Either ------------------------------------------------------------------

Either <- R6::R6Class("Either",
											public = list(
												initialize = function() {
													lockEnvironment(self, bindings = TRUE)
												},
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
										 		lockEnvironment(self, bindings = TRUE)
										 	},
										 	map = function(f) {
												Left$new(value = self$value)
											},
											flatMap = function(f){
												Left$new(value = self$value)
											},
											fold = function(f, g) {
												f(self$value)
											},
											ap = function(fab) {
												Left$new()
											})
)
# Right -------------------------------------------------------------------

Right <- R6::R6Class("Right",
										 inherit = Either,
										 public = list(
										 	value = NULL,
										 	initialize = function(value) {
										 		self$value = value
										 		lockEnvironment(self, bindings = TRUE)
										 	},
										 	map = function(f) {
										 		Right$new(value = f(self$value))
										 	},
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
