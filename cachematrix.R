## Matrix inversion is a resource consuming task.
## Rather than computing the inverse repeatedly, it is good to store it in cache (given the matrix remains the same).
## The following pair of functions compute the matrix inverse and store it in cache.
## If the inverse is needed, then it takes the value from cache rather than computing it

## The makeCacheMatrix function is used to create a matrix object, and provides functions to input and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {     #function takes a matrix as input
  
      inv <- NULL  #initialize the variable for matrix inverse as NULL
      
      ##setMat function sets the matrix
      setMat <- function(mat){  
        x <<-mat
        inv <<- NULL
      }
      
      ##getMat function gets the matrix
      getMat<- function () {  
        x
      }
  
      ##setInv function sets the inverse
      setInv<-function(matInv) {      
        
        inv<<- matInv
      }
      ##getInv function gets the inverse
      getInv <- function() {
       
        inv
      } 
  
      list(setMat = setMat, getMat = getMat,
           setInv = setInv,
           getInv = getInv)
  
}


##  The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix function. If the inverse exists already, it returns the cached value

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'
              
      
      inv <- x$getInv()       #Get the inverse of input matrix
      if(!is.null(inv)) {     #If inverse exists, get the value from cache
        message("getting cached inverse")
        return(inv)           #return the cached inverse
      }
      data <- x$getMat()      #If inverse doesn't exist, get the matrix  
      inv <- solve(data, ...) #Compute the inverse
      x$setInv(inv)           #Set the inverse value

      inv                     #return the computed inverse
  
}


