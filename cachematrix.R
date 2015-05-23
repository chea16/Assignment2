# The following functions are written to make save time in computing inverse of a matrix when inverse of the same matrix is computed again and again.
# In case of repeat computations the cached data can be reported. On the command line run the following commands in sequence:
# s<-makeCacheMatrix()
# s$set(matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3))
# cacheSolve(s)
# cacheSolve(s)
# Explanation: We first call the makeCacheMatrix() function and then assign the value of the matrix using s$set. Inside s$set() function, because of
# "<<-" operator x is assigned a value in the whole of the environment of makeCacheMatrix(). Hence s$get()  also gets its values.  
# When cacheSolve(s) function is called the first time, getinverse() = NULL, i = NULL, hence the if statement isn't computed. Inverse of the matrix is computed using 
# solve() function and its value is feeded into setinverse() function. Again due to "<<-" operator that value of i is available to s$getinverse() function.
# When cacheSolve(s) is invoked again on the command line, s$getinverse() != NULL. Hence if statement is evaluated and cached date is retrieved. The value of inverse is returned.



## The first function, makeCacheMatrix creates a special "vector", which is really a matrix containing a function to
#set the vector's value
#get the vector's value
#set the inverse of the matrix vector
#get the inverse of the matrix vector

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function calculates the inverse of the matrix vector created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
    if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  print(i)
  
        ## Return a matrix that is the inverse of 'x'
}
