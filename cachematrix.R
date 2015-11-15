## Put comments here that give an overall description of what your
## functions do
#
#  makeCacheMatrix:  takes a Matrix as input parameter and:
#        Store it in a different environtment than working 
#        environente. It has been stored in the cache.   
#        Predefine four functions to manage the matrix. 
#        Return a List.
#
#  CacheSolve: Takes a list as input parameter (retourned by 
#         previous function) and verify if the inverse of this
#         matrix has beeb calculed yet and the result is in the cache.
#         If yes, it retuns this inverse matrix and a message saying
#         that it is retourned from cache. 
#         if no, it retournes tne inverted matrix with no message.
#         Return: Inverted matrix that it was given in first function. 
# 
#  Example:
#
#      > A <- matrix(c(1,-14,-1/4,1), nrow=2, ncol=2, byrow=FALSE)
#      > B <- makeCacheMatrix(A)
#      > B$get()
#               [,1]  [,2]
#          [1,]    1 -0.25
#          [2,]  -14  1.00
#
#      > cacheSolve(B)
#               [,1] [,2]
#          [1,] -0.4 -0.1
#          [2,] -5.6 -0.4
#
#      > cacheSolve(B)
#        getting cached data
#               [,1] [,2]
#          [1,] -0.4 -0.1
#          [2,] -5.6 -0.4
#


## Write a short comment describing this function
#
#      This function is a "contructor". It takes
#      a matrix as input parameters and creates a list 
#      with functions set, get, setMatrix and getMatrix.
#      
#      get() retourn the matrix passed as parameter.
#      set() store the matrix
#      setMatrix() Store the result of inversion in cache.
#      getMatrix() Retourn the inverted matrix passed as parameter.
#
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y) {                                 # Erase the cache when initial matrix change
        x <<- y
        m <<- NULL
    }

    get <- function() x                                   # Retrieve initial Matrix
    setMatrix <- function(solve) m <<- solve              # Store Inverted Matrix in cache
    getMatrix <- function() m                             # Retrieve Invertd Matrix from Cache
    list( set = set,                                      # List returned
          get = get,
          setMatrix = setMatrix,
          getMatrix = getMatrix)
}


## Write a short comment describing this function
#
#  This function takes the list, retourned by previos function,
#  as the input paramenter. 
#  first, it find the inverted matrix in cache.
#  if it doesn´t found, takes the matrix, do the invesion process and
#  and store this result in the cache. Finally return the result.
#  if it exsits, this result is returned with a message saying
#  "Getting Cached Data".
#

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

    # Find if already exists inverted matrix 
    m <- x$getMatrix()

    #Check results 
    if (!is.null(m)) {

        # Exists !!!!!!

        message("getting cached data")
        return (m)
        }

    #Not Exists

    data <- x$get()         #Takes initial Matrix
    m <- solve(data, ...)   # Inverted Matrix Calculation
    x$setMatrix(m)          #  Stored in cache
    m                       # Return the result.
}
