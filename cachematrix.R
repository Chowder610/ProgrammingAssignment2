## makeCacheMatrix is a function that creates two objects and creates a list that...
## will return 4 functions that can be called in other code to manage a stored matrix object. 

##     x=Matrix object
###    I=Cached Inverse Matrix Object. 


##    1. creates a function that replaces source data matrix and clears out
##       previous stored inverse matrix.
###   2. Creates a function that calls the cached matrix
##    3. creates a function that caches a calculated inverse matrix
##    4. creates a function that calls the cached inverse matrix



makeCacheMatrix <- function(x = matrix()) {      
  I <- NULL
  set <- function (y) {                              #1
    x <<- y
    I <<- Null      
  }
  get <- function() x                                #2       
  setinversem <- function(inverse) I <<- inverse     #3
  getinversem <- function() I                        #4                 
  list( set = set, get = get, setinversem = setinversem, getinversem = getinversem)
}



 
## This is code that checks whether a calculated inverse matrix already exists and ...
##  if a inverted matrix already exists than it the code will get it, if a not then ...
### the code will create an inverse matrix and cache the Inversed matrix. 
####
##         #1. Checks to see if inversed matrix already exists
##         #2. Calls the inverted Matrix if exists.
##         #3. calculates Inverse Matrix if one does not exist
##         #4. calls the setinversem function from the makecahceMatrix function and...
##             caches the new inverted Matrix
##
##

cacheSolve <- function(x, ...) {
        
  I <- x$getinversem()
  if(!is.null(I)) {                         #1
    message("getting cached matrix")
    return(I)                               #2 
  } 
  else {                 
    matrix <- x$get()
    inverse <- solve(matrix, ...)           #3
    x$setinversem(inverse)                  #4
    inverse
  }
}
