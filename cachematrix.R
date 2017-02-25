## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     reverse_matrix <- NULL
     set <- function(y) {
          x <<- y
          reverse_matrix <<- NULL
     }
     get <- function() x
     set_reverse_matrix <- function(solve) reverse_matrix <<- solve
     get_reverse_matrix <- function() reverse_matrix
     list(set = set, get = get,
          set_reverse_matrix = set_reverse_matrix,
          get_reverse_matrix = get_reverse_matrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     reverse_matrix <- x$get_reverse_matrix()
     if(!is.null(reverse_matrix)) {
          message("getting cached data")
          return(reverse_matrix)
     }
     data <- x$get()
     reverse_matrix <- solve(data, ...)
     x$set_reverse_matrix(reverse_matrix)
     reverse_matrix
}
