## We create a pair of functions that work in tandem to
## pass a matrix to get back the inverse
## it will cache this inverted matrix to 
## return on subsequint calls

## makeCacheMatrix will take a matrix and create a list
## which has methods to set and get a cached matrix

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setInverseMatrix <- function(solve) m <<- solve
		getInverseMatrix <- function() m
		list(set = set, get = get,
			setInverseMatrix = setInverseMatrix,
			getInverseMatrix = getInverseMatrix)

}


## this method returns a matrix inverse by
## calling the funtion on the makeCacheMatrix object
## @property x : variable represnting a 
## makeCacheMatrix list object 

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()

		if(!is.null(m)){
			message("gotch ya cache right here")
			return(m)
		}
		data <- x$get()
		m <- solve(data)
		x$setInverseMatrix(m)
		m
	
}