## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve will retrieve the inverse from the cache.


## These functions are used together as follows:
## USE EXAMPLE
## my_mat = matrix(  c(2, 4, 3, 1, 5, 7,4,2,7,5,8,9,3,2,4,56,8,5,3,2,6,5,7,3,2), nrow=5,  ncol=5) 
## t<- makeCacheMatrix(my_mat)
## cacheSolve(t)
## RESULT has message either  "not cached - calculating" or "getting cached data" depending on whether it finds cached result
## Example result 
## getting cached data
##             [,1]        [,2]          [,3]         [,4]        [,5]
## [1,]  0.0100014801 -0.12200537  0.0670289472 -0.163829742  0.28615228
## [2,] -0.0006131986 -0.02634639 -0.0548495549  0.158036073  0.02262491
## [3,] -0.0161757554  0.20155203 -0.1020658448 -0.003531178 -0.09282558
## [4,]  0.0212082126 -0.01981266  0.0004863299 -0.017592456  0.01059353
## [5,] -0.0123274057 -0.01241198  0.1731968790  0.039138985 -0.09688537


makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}







cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      #print ("Checking if cached")
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      else
      {message("not cached - calculating")
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
