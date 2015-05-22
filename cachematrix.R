### cachematrix.R
## Coursera R Programming course Week 3 assignment
## author: HowlinHank
## Copyright 2015, All Rights Reserved

##----------------------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------

## makeCacheMatrix Library description

# These functions enable you to create a special type of matrix that allow for caching of
# previous results by hiding those in internal instance variables that are acessible by
# a set of accessor functions.  Matrices MUST BE SQUARE.

# The goal of this object is to speed up code by not recalculating a matrix inverse if it has
# already been calculated. If the matrix is unchanged, then it just retrieve the cached result.

##FUNCTONS

# makeCacheMatrix() serves as the constructor

# cacheSolve() is the associated matrix inversion call that either retrieves the cached value
#      or calculates the inverse of the matrix.


##----------------------------------------------------------------------------------------------
##  makeCacheMatrix()---------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------

# makeCacheMatrix() will cache the input matrix and previously computed inverse values. This
# function does not perform the inverse itself rather only acts to store and hide the cached
# values.  Note this returns cached values for inverse, but does not check that the matrix is the
# same.
#  - provides set(x) get() functions to set and retrieve the matrix
#  - provides setInverse(inverse), getInverse() are used to set and get the cached value

## USAGE:
#      y <- makeCacheMatrix(xMatrix)       #create new instance
#      z <- y$get()                        #retrieve the cached matrix
#      y$set(y*2)                          #reset the value of matrix & resets the cached inverse
#      y$setInverse(theInverse)            #to cache a computed inverse
#      y$getInverse()                      #retrieve a stored inverse

makeCacheMatrix <- function(x = matrix()) {
    type <- "makeCacheMatrix"       #type check variable for sanity
    inverse <- NULL
    
    #check if square
    if (ncol(x) != nrow(x)) {
        print( "Matrix is not square. Only square matrices can have inverses.")
        return(NULL)
    }
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL    #flush inverse
    }
    
    get <- function() x
    
    setInverse <- function(invertedMatrix) {
        inverse <<- invertedMatrix
    }
    
    getInverse <- function() {
        inverse
    }
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse, type=type)

}


##----------------------------------------------------------------------------------------------
##  cacheSolve()--------------------------------------------------------------------------------
##----------------------------------------------------------------------------------------------

# cacheSolve() is the associated matrix inversion helper function that uses the R solve() 
# function or retrieves the cached value.  Note that the matrix must be identical in all elements
# as to when the previous call to cacheSolve() was executed in order for the matrix to be 
# considered unchanged and the cached value retrieved.

# See: solve() to get the extra paramenters that can be passed during matrix inversion.
##NOTE: The assumption is that all extra parameters are identical

##USAGE:
#       cacheSolve(x)           #returns the inverse either from cache or calculation

cacheSolve <- function(x, ...) {
    
    #enforce only using with makeCacheMatrix instances
    if( is.null(names(x)) || is.null(x$type) || x$type != "makeCacheMatrix") {
        message("Error: cacheSolve can only be used for special matrices of type 
                makeCacheMatrix")
        return(NULL)
    }
    
    
    inverse <- x$getInverse()
    if(!is.null(inverse)) {     #FOUND cache value
        message("getting cached inverse")
        return(inverse)
    }
    
    #not yet cached
    newMatrix <- x$get()
    inverse <- solve(newMatrix, ...)
    x$setInverse(inverse)
    inverse
            
}


##----------------------------------------------------------------------------------------------
##End of cacheMatrix.R