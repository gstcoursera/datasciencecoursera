## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # we initialize the inverse matrix value
    i <- NULL

    # we set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    # function to get the value of the matrix
    get <- function() { x }

    # function to set the value of the inverse
    setInverse <- function(inverse) {
	   i <<- inverse
	}

    # function to get the value of the inverse
    getInverse <- function() { i }

    # we return a list of all the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special 
## "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cache via 
## the setInverse function.

cacheSolve <- function(x, ...) {
	# return a matrix that is the inverse of 'x'
    i <- x$getInverse()

    # if the inverse is already cached,
	# we get the inverse from the cache
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }

    # we get the matrix
    data <- x$get()

    # calculate the inverse
    i <- solve(data, ...)

    # cache the inverse of the matrix
    x$setInverse(i)

    # return the result
    i
}
