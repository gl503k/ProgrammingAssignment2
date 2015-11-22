## paired of functions used to create a special object that
## stores a matrix and caches its inverse
## 11/22/15, gl503k@att.com

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() {  ## return input-matrix
        x
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {   ## return inverse-matrix
        inv
    }
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special matrix created by above
## makeCacheMatrix. If inverse has already been calcuated and input matrix
## does not change from previous one, then it retrieves the inverse from cache

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
