## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## Write the following functions:
##        makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##        cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##          If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
##          retrieve the inverse from the cache


## Create function that will create matrix object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## Set this list that is called from the second fuction (cacheSolve)
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the value of the mean
        ## 4. get the value of the mean
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Create fuction that will look for the inverse of the matrix in the cache, and return it if it exists.
## If not, this function will calculate the inverse of the matrix. 
cacheSolve <- function(x, ...) {
        #get the inverse from the cache if it's there:
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #if not, calculate the inverse of the matrix:
        data <- x$get()
        i <- solve(data, ...)           ##Solve will compute the inverse of a square invertible matrix
        x$setinverse(i)
        return(i)
}

