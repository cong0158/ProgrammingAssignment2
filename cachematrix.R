## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix would return a list that represent the matrix 
## with 4 different functions(set, get, setsolve, and getsolve)
## in the function, m is the variable represent the "solved" matrix of x and x is the matrix. 
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x

	setsolve <-  function(s) m <<- s
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
## The cacheSolve function is to solve the matrix of x, it would detemine if the x is being solved
## if it is solved, cacheSolve would directly return the value without compution.
## if not, it would compute the result and return the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(class(m) != "NULL"){
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
