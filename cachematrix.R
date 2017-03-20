# Here, makeCacheMatrix contains a function which does 4 tasks:
#Firstly, set the value of the matrix
#Secondly, get the value of the matrix
#Thirdly, set the value of inverse of matrix
#Fourthly, get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, 
       get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


# The following function first checks if the inverse is already done. 
# If it is, it fetches the result and skips calculations. 
# If it's not,it calculates the inverse, sets the value in cache through setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
# Test for Solution
#> i <- matrix(c(7,8,9,10),2,2)
#> j <- makeCacheMatrix(i)
#> cacheSolve(j)
# [,1] [,2]
# [1,]   -5  4.5
# [2,]    4 -3.5
#> cacheSolve(j)
#getting cached data!
# [,1] [,2]
# [1,]   -5  4.5
# [2,]    4 -3.5
