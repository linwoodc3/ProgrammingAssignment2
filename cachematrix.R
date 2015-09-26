

## This file has a pair of functions that cache the inverse of a matrix.

# It creates a special "matrix" object that can cache its inverse.:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the mean
# 4. get the inverse of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # here we set the inverse to null if it hasn't been computered
        set <- function(y) {
                x <<- y    # assigning y to x inside this function environment 
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
        
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data") # a message as the function runs
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...) # Compute the inverse of a square matrix using the solve function in R
        x$setinverse(inv)
        inv
}

# Test to make sure this works:

## source("ProgrammingAssignment2/cachematrix.R")

# x <- matrix(c(2, 4, 3, 1, 5, 7, 10, 3, 9),nrow=3,ncol=2)

# a <- makeCacheMatrix(matrix(x))

# a$setinverse(solve(x))

# a$getinverse()



