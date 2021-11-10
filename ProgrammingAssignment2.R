## Write a short comment describing this function
## In makeCacheMatrix you must introduce a matrix
## It will then create a list that contains a function
## Which consists of: setting the value of the matrix (set), getting the value 
## of the matrix (get), setting the value of the inverse of the matrix (setInverse),
## and getting the value of the inverse of the matrix (getInverse)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL

        set <- function(z) {
                x <<- z
                inver <<- NULL
                }

        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list( set=set, get=get, setInverse=setInverse, getInverse=getInverse )
        }

## Now, cacheSolve returns the inverse of the matrix previously used.
## The if checks wether the matrix is already inversed or not. If it is not, it
## gets the inverse and prints it. If it is, simply prints it.

cacheSolve <- function(x, ...) {
   
        inver <- x$getInverse()

        if(!is.null(inver)) {
                return(inver)
                }

        data <- x$get()
        inver <- solve(data)
        x$setInverse(inver)
        inver
        }
