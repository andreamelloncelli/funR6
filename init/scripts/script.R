# funtore_applicativo -----------------------------------------------------
# init
fab <- Some$new(value = function(a) a * 3)
fab
s <- Some$new(value = 2)
p <- None$new()
s
s$ap(fab)
## or
fab$map( function(f) f(s$value) )

## Qui il problema di tipo
s$ap(s) # error
###
