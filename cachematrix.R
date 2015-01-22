## Carlos Martínez. Modified from the example for Assignment 2. Coursera.

## The functions below allow to calculate the inverse of a given matrix and
##   to store the calculated inverted matrix in cache for further ru-use insofar
##   no new matrix is entered. 


## This 1st function creates a list containing methods to set and store both  
## the input matrix and its computed inverted matrix. The actual calculation of the
## inverted matrix is done by function cacheSolve below.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL   #resets the cached variable containing the inverted matrix

        set <- function(y) {
                x <<- y       # assigns the input matrix to a variable accessible 
                              #    to the parent environment. The inverted matrix 
                              #    will be computed by cacheSolve function. 
                i <<- NULL    # cleans up the content of te variable containing the  
                              #    inverted matrix and makes it accessible to the 
                              #     parent environment
        }
        get <- function() x                # gets the input matrix
        setinv <- function(inv) i <<- inv  # stores the inverted matrix computed in
                                           #    cacheSolve in cache (variable i).
        getinv <- function() i             # gets the inverted matrix
            
        list(set = set, get = get,         # sets the "special vector" containing 
             setinv = setinv,              #   the functions that can be invoked                                    
             getinv = getinv)              #   independently
}


## cacheSolve checks whether a previously computed cached inverted matrix exists. 
## If not, it calculates the inverted matrix and calls $setinv function to 
## cache the new calculated inverted matrix in a variable accessible by the parent env.

cacheSolve <- function(x, ...) {
        i <- x$getinv()        # inspects the value of the cache
        if(!is.null(i)) {      # if not null, retrives the cached inverted matrix
                message("getting cached data")
                return(i)
        }
        data <- x$get()        # otherwise, it computes a a new inverted matrix
        i <- solve (data, ...)
        x$setinv(i)            # and stores it in cache invoking the $setinv function
        i
}
