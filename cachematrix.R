## This code defines 4 functions and then stores them in a list.
## These functions either return already stored matrices or used to calc them
## in conjunction with the cacheSolve below

makeCacheMatrix <- function(x = matrix()) {  ## Sets up a function expecting a matrix input
                m <- NULL  ## nullifies m at this level
                set <- function(y) {  ## creates function "set" with variable y
                        x <<- y  ##Sets x = y in the PARENT function ie level above
                        i <<- NULL ##Nullifies i in the PARENT function ie level above
                }
                get <- function() {x} ## the get function just returns matrix x.  It has no input variable
                setinverse <- function(theinverse) m <<- theinverse  ## sets m as the "theinverse" in the PARENT
                getinverse <- function() {m} ## just returns m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse) ## sets up a list of 4 functions created above.
        }


## This function checks if there is already a stored inverse matrix. If so, then it returns it.
## Otherwise it calculates the inverse matrix and then stores it for future.
## Please note: The input into this function needs to be the output of the makeCacheMatrix  NOT the original matrix.  
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()  ## uses the getinverse function and assigns to m
                if(!is.null(m)) {  ##checks if m already has any content ie do we already have the inverse matrix
                        message("getting cached data")  ## shows the message
                        return(m)  ## if not null, then returns the stored inverse matrix (that's been assigned to m)
                }
                data <- x$get()  ##set data using the get function defined
                m <- solve(data, ...) ## calculate the inverse matrix using solve
                x$setinverse(m)  ## store the inverse matrix to avoid having to run it again in the future
                m  ## prints the inverse matrix m
        }
