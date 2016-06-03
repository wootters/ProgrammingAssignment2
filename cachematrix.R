## This function reads a square matrix 'x' and returns a list of four functions:
## to declare the matrix 'x' set()
## to retrieve the matrix 'x' get()
## to declare the inverse matrix 'inv' setinv()
## to retrieve the inverse matrix 'inv' getinv()
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(Inverse) inv <<- Inverse
     getinv <- function() inv
     list(set = set, inv = inv,setinv = setinv, getinv = getinv)

}


## this function reads the output of makeCacheMatrix, 'listx' 
## first it checks to see if the inverse matrix 'inv' has been calculated and stored,
## retrieving through xlist$getinv().  If the result is not null, 'inv' is printed out
## If the result of the query is null, then cacheSolve calculates the inverse using solve(x)
## and stores the result using setinv()

cacheSolve <- function(xlist, ...) {
       inv = x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     Matrix <- xlist$get()
     inv <- solve(Matrix, ...)
     xlist$setinv(inv)
     print(inv)
     
     Test <- x %*% inv
}

##  I am not able to make this work.  I get the following error message:   
## Error in array(x, c(length(x), 1L), if (!is.null(names(x))) list(names(x),  : 'data' must be of a vector type, was 'NULL' 
## I do not understand what the problem is.
