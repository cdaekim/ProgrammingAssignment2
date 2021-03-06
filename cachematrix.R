## Following the template given by the course, makeCacheMatrix is a function
## that receives a matrix, gets a matrix, inverses the matrix, gets the inversed matrix, and stores this in a list.

makeCacheMatrix <- function(x = matrix()) {

    #initilize the inverse matrix 
    invMatrix <- NULL
    #set the matrix #
    set <- function(y) { 
        x <<- y
        invMatrix <<- NULL
    }
    #get the matrix
    get <- function() x
    #set the inverse matrix
    setinverse <- function(inverse) invMatrix <<- inverse
    #get the inversed matrix
    getinverse <- function() invMatrix
    #store in a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Calculates the inverse of the matrix if it is not already cached within the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinverse()
    ##Conditional statement that validates if the inversed matrix exists within the cache
    ##If it does, it will return the inverse
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    #Calculates the inverse of matrix if it does not exist and returns it
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setinverse(invMatrix)
    invMatrix
}
