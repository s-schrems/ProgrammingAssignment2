## These functions are designed to work concurrently... if the inverse of the matrix
## has been found previously, it is returned without doing any other calulations.
## That is how this saves time.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   #sets inv variable to NULL... 
        setmatrix <- function(y) {
                x <<- y  #caches matrix so cacheSolve can check to see if it has changed
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        #returns a list of functions as an R object
        list(setmatrix=setmatrix, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function down here is the one that does the inverse of the matrix
## First checks to see if the inverse has been found (!is.null part)
## If it has been found, it returns the results and quits.  

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Morpheus says:  Getting the cached data... this won't take long!")
                return(inv)
        }
        message("Morpheus says: No cached data found... Neo is the one!  He can calculate the inverse!")
        data <- x$get()   #this gets the matrix
        inv <- solve(data) #finds the inverse of the matrix!
        x$setinverse(inv)  #assigns resulting inverse matrix to x
        inv
}

## Let's hope nothing has changed in the Matrix