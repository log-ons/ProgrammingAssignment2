## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    ## makeCacheMatrix - Takes matrix input and if matrix already exit
    ##then return cached inverse matrix else null.
  
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }

    
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Checks if makeCacheMatrix return value then returns cached value
    # Else calculate inv matrix and cach it and also return inv matrix.
    
    m <- x$getinv()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)
    }
        
  
    mat.data <- x$get()
    m <- solve(mat.data, ...)

    x$setinv(m)
    m
}
