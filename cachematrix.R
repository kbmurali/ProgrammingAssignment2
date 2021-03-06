## This function is a convenient wrapper function that returns a 'special matrix' 
## containing get & set accessor functions for the invertible matrix and its matrix inverse. 
## Specifically this function returns a named list with following accessor functions: 
## 
## set : function to set a reference to invertible square matrix
## get : function to get a reference to invertible square matrix
## setinverse : function to set a reference to inverse of the square matrix 
## getinverse : function to get a reference to inverse of the square matrix
##
## A convenient function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	x_inv <- NULL

	set <- function(y){
		x <<- y
		x_inv <<- NULL
	}

	get <- function() x

	setinverse <- function( inverse ) x_inv <<- inverse

	getinverse <- function() x_inv

	list( set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse )
}


## This function takes the 'special matrix' as created by 'makeCacheMatrix' function 
## and solves for the inverse of the matrix. It first checks to see if matrix inverse 
## has already been solved. If so, it just returns the matrix inverse from the cache 
## and skips the computation. Otherwise, it computes the matrix inverse, caches it 
## and returns the computed inverse

cacheSolve <- function(x, ...) {
	x_inverse <- x$getinverse()

	if( !is.null( x_inverse ) ){
		message( "Using cached inverse" )
		
		return( x_inverse )
	}

	x_matrix <- x$get()

	x_inverse <- solve( x_matrix, ... )

	x$setinverse( x_inverse )

	x_inverse
}
