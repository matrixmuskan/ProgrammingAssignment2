## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function makeCacheMatrix creates a special "matrix", which is really a list of function to 
# 1. set the value of the matrix
# 2. get the value of the matrix 
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv,getinv = getinv) # return 

}


## Write a short comment describing this function
# This function calculates the mean of the special "atrix" created with the above function. However, it first check to see 
# if the inverse of tha matrix has already been calculated. If so , it will get the inv from the cache and skips the comp. 
# Otherwise, it will calculate the inv of the matrix and sets the value of inv in the cache via setinv function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if ( !is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}

