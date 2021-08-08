## Assignment 2: Caching the Inverse of a Matrix

## 1. makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get the value of the matrix
        get <- function() {
                x
        }
        ## set the value of the inverse
        setinverse <- function(inverse) {
                i <<- inverse
        }
        ## get the value of the inverse
        getinverse <- function() {
                i
        }
        ## return a list of the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# 2. cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## return the inverse if already calculated
        if (!is.null(i)) {
                message("From cached data:")
                return(i)
        }
        
        ## get the matrix 
        data <- x$get()
        ## calculate the inverse of a square matrix
        ## if X is a square invertible matrix, then solve(X) returns its inverse
        i <- solve(data, ...)
        ## set the inverse
        x$setinverse(i)
        ## return the matrix
        i
}

# 3. using an example square matrix to test the function
A <- matrix(c(1, 2, 3, 4), 2, 2)
#solve(A) 
A1 <- makeCacheMatrix(A)
cacheSolve(A1) #inverse returned after computation
cacheSolve(A1) #inverse returned from cache
