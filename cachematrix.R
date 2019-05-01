##This function creates makeCacheMatrix object and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {  ## assigning to "makeCacheMatrix" the matrix function.
        m <- NULL                            ## clearing the cache.
        set <- function(y) {
                x <<- y    ## Setting the value for x.
                m <<- NULL ## Clearing the cache.
        }
       
        get <- function() x ## Assigning to "get" the value of the matrix.
        setinverse <- function(inverse) m <<- inverse ## Assigning the function to calculate the inverse.
        getinverse <- function() m ## Assigning to "getInverse" the function to do so.
        list(set = set, get = get, ##Returning the list of the functions
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function calculates the inverse of the matrix returned by makeCacheMatrix and checks whether the value of the inverse matrix
## already exists or needs to be calculated

cacheSolve <- function(x) {
        m <- x$getinverse() ## Assigning to "m" the value for the inverse matrix.
        if(!is.null(m)) {   ## If the value is different than NULL, the cached value is returned.
                message("Getting cached data")
                return(m)
        }
        ## If the value is NULL, the value of the inverse matrix needs to be calculated, stored and returned.
        data <- x$get()  
        m <- solve(data) ## The Solve() function calculates the inverse of the matrix.
        x$setinverse(m)  ## The value is stored.
        m                ## The value of the inverse of the matrix is returned.
}
