# This function creates a special "matrix" object that can cache its inverse.
# It returns a list of 4 functions to access the object
# get , set, getsolve, setsolve

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the solution to the inverse of matrix
    
    m <- NULL
    
    # We must reset m to NULL since we are modifying the underlying
    # matrix and the cached value is no longer the valid.  
    # Please note that if m is not set to null, 
    # we cannot detect the matrx had changed
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Please note that this works because x is stored in the function
    get <- function(){
        x
    } 
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set=set, get=get,
         setsolve=setsolve,
         getsolve=getsolve)
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# and the matrix has not changed, then the cachesolve should retrieve 
# the inverse from the cache.
#
# This function test whether the passed argument is a list with 
# "getsolve" and "setsolve" functions.
# This is done to check and make sure that "makeCacheMatrix" is called 
# before cacheSolve function.

cacheSolve <- function(x=matrix(), ...) {
    
    # Return a matrix that is the inverse of 'x'
    
    # The follwing set of if statement validate that makeCacheMatrix had 
    # been called and cacheSolve is called with a list of functions
    
    if(class(x) =="list" ){
        if ( class(x[["getsolve"]])== "function"){
            if ( class(x[["setsolve"]])== "function"){
                m <- x$getsolve()
                if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
                } else {
                    data <- x$get()
                    m <- solve(data, ...)
                    x$setsolve(m)
                    m
                }
                
            } else {
                message("You need to call makeCacheMatrix function first -> Failed setsolve")
            }
            
        } else {
            message("You need to call makeCacheMatrix function first -> Failed getsolve")
        }
                
    } else {
        message("You need to call makeCacheMatrix function first-> Failed list")
    }    
}