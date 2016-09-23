## cacheSolve allows to use a cache calculation of a matrix if the inverse has already
## been calculated, using the function makeCacheMatrix to make a special version
## of a matrix

## makeCacheMatrix creates a special "matrix", which is a list containing:
## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the value of the inverse matrix
## 4. gGetting the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x<<-y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve calculates the inverse of the special 'matrix' defined by makeCacheMatrix.
## if the inverse has already been calculated, it gets the inverse from the cache and skips
## the computation, otherwise it calculates the inverse of the matrix and sets it
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}
