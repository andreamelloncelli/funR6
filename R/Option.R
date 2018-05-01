# # Option ------------------------------------------------------------------
#
Option <- R6::R6Class(classname = "Option",
											public = list(
												lift = function(f) {
													function(this) this$map(f)
												})
)

# # None --------------------------------------------------------------------
#
None   <- R6::R6Class(classname = "None",
											inherit = Option,
											public = list(
												map = function(f) {
													None$new()
												},
												flatMap = function(f){
													None$new()
												},
												fold = function(f, g) {
													f()
												},
												ap = function(fab) {
													None$new()
												}
											))
# # Some --------------------------------------------------------------------
#
Some   <- R6::R6Class(classname = "Some",
											inherit = Option,
											public = list(
												value = NULL,
												initialize = function(value) {
													self$value = value
													lockEnvironment(self, bindings = TRUE)
												},
												map = function(f) {
													Some$new(value = f(self$value))
												},
												flatMap = function(f) {
													f(self$value)
												},
												fold = function(f, g) {
													g(self$value)
												},
												ap = function(fab) {
													fab$map(function(f) f(self$value))
												}
											))
