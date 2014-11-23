## Put comments here that give an overall description of what your
## functions do

## Function to create a matrix and store it in cache
makeCacheMatrix <- function(x = matrix(c(1,3,7,6,2,8,3,7,5),nrow=3,ncol=3)) {
     i <- NULL
     set <- function(y = matrix(c(5,3,8,2,7,4,1,8,1),nrow=3,ncol=3)) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) i <<- inverse
     getinv <- function() i
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## Function to calculate and store the inverse of a matrix
cacheSolve <- function(x, ...) {
        i <- x[["getinv"]]()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        m <- x[["get"]]()
	message("calculating inverse")
        i <- solve(m)
        x[["setinv"]](i)
        i
}

