## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        invMatrix <- NULL

        setMatrix <- function(y) { 
                x <<- y 
                invMatrix <<- NULL 
        }

        get <- function() x 
        setMatrix <- function(inverse) invMatrix <<- inverse 
        getinverse <- function() invMatrix 
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse) 

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) { 
                message("Getting Cached Data") 
                return(invMatrix) 
        }

        dataMatrix <- x$getMatrix() 
        invMatrix <- solve(dataMatrix, ...) 
        x$setInverse(invMatrix) 
        invMatrix
}
