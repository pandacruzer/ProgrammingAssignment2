## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() takes in a square matrix as input and creates
## an object that contains the input matrix as x, an object for the
## inverse matrix as inv_x, and setter and getter functions that
## can be used to reset and access x and inv_x for the cacheSolve
## function and for use after the matrix object has been initiated


makeCacheMatrix <- function(x = matrix()) {

    inv_x <- NULL
    
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    
    get <- function() x
    setInv <- function(solved) inv_x <<- solved
    getInv <- function() inv_x
  
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

## cacheSolve() takes in the matrix object initiated by makeCacheMatrix()
## and solve for the inverted matrix inv_x if it hasn't been calculated
## yet and caches it to the object's environment. If the inverted matrix
## has been calculated, it will simply return the inverse

cacheSolve <- function(x, ...) {
    
    inv_x <- x$getInv()
  
    if(!is.null(inv_x)) {
        message("pulling cached inverted matrix")
        return(inv_x)
    }
    
    data <- x$get()
    inv_x <- solve(data, ...)
    
    x$setInv(inv_x)
    
    ## Return a matrix that is the inverse of 'x'
    inv_x
}

