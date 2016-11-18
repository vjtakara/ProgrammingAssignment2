##receives a square matrix called x and saves it in the cache. Also creates
##a cache for saving two functions, setInv and getInv
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	##function which stores an y variable in a x variable (in another environment) and resets inv in that same env
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	##function which stores x in get variable
	get <- function() x

	##function which saves invMtx in inv variable in another env
	setInv <- function(invMtx) inv <<- invMtx
	
	##function which gets inv variable
	getInv <- function() inv

	##returns a list of functions with set, get, setInv and getInv arguments
	list(set = set, get = get,setInv = setInv,getInv = getInv)
}

##receives a square matrix called x. Verifies if there is an inv function saved in the cache
##if it is not in the cache, inv variable will get inverse matrix x value by solve function.
##Otherwise, it's gonna get the calculated value saved in inv variable
cacheSolve <- function(x, ...) {
	
	inv <- x$getInv()
	##verifies if inv variable is NULL. If it isn't, cacheSolve is gonna return it's value
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	##if it is not NULL, function will continue to store the
	##matrix inverse in inv variable and stores it in the cache
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}
