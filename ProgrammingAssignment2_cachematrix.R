## Caching the inverse of a Matrix: Below are a pair of functions that are used 
## to create a special object that stores a matrix and catches its inverse. 

# First, I create the function "makeCacheMatrix" that creates a special Matrix 
# object that can cache its inverse. 
## I set the input x as a matrix and then set the solved value "Inv" as a null.
## Then I changed every reference to "mean" to "solve".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInv <- function(inv) inv <<- inv
        getInv <- function() inv
        list(set = set, get = get, 
             setInv = setInv, getInv = getInv)
}


## Secondly, I created the function "cacheSolve" to compute the inverse of the
## special "matrix" returned by "makeCacheMatrix" above. If the inverse has 
## already been calculated (and the matrix has not changed), then "cacheSolve" 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting inversed matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

