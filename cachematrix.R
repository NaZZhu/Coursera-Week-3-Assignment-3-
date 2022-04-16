

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## 
        inv = NULL
        set <- function(y) {
                x <<- y              ## "<<-" - assignment operator  
                inv <<- NULL
        }
        
        ## get x matrix and inverse of  x matrix
        get <- function() x
        setinversion <- function(inverse) inv <<- Inverse
        getinversion <- function() {
                inver <- Ginv(x)                         ## library("matlib") 
                inver%*%x
        }
        list(set = set, get = get,
             setinv = setinversion,
             getinv = getinversion)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                return(inv)
        }
        data <- x$get
        inv <- solve(data, ...)       ## solve --> standard function for matrix inversion/returns inverse of x
        x$setinv(inv)
        inv
}

test <- makeCacheMatrix(matrix(1:10, 2, 5))

test$get()
test$getinv()
cacheSolve(test)
