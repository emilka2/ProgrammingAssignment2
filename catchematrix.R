# creating makeCacheMatrix function to catche the inverse of 
# part 1
# set new function to catch the data
makeCacheMatrix <- function(m = matrix()) {
  solve <- NULL
  set <- function(newmatrix) {
    m <<- newmatrix
    solve <<- NULL
  }
  # we obtain original matrix data
  get <- function() m  
  # inverse of matrix data
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  # output return
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


# creating CacheSolve for the function above
# part 2
# inverse of our m

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinv(inv)
  inv
}

# end

