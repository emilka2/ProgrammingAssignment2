# creating makeCacheMatrix function to catche the inverse of 
# part 1
makeCacheMatrix <- function(m = matrix()) {
  solve <- NULL
  set <- function(newmatrix) {
    m <<- newmatrix
    solve <<- NULL
  }
  
  get <- function() m
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}


# creating CacheSolve for the function above
# part 2

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
