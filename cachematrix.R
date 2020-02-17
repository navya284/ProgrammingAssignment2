## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        ##Initialization the inverse property
        i<- Null
        ##setting up the matrix 
        Set<-function(matrix){
                m<<-matrix
                i<<-Null
}
##get the matrix
      get<-function() {
     ##return the matrix
                m
 }
        ##setting up the inverse if matrix 
        setInverse <-function(inverse) {
                i<<- inverse
 }
      ##get the inverse of the matrix
        getInverse <-function(){
            ##return it
                i
                }
        ##return list of respective methods
        list(set= set, get=get, setinverse= setinverse, getInverse = getinverse)
        }
##compute the inverse of the special matrix returned by "makecache matrix"
##If the inverse has already been calculated and if and only if the matrix has sent
##changed, then the "cachesolve" should retrieve inverse from the cache.

cachesolve <-function(x,....) {
        ##return a matrix that is the inverse of 'X'
        m<-X$getINverse()
        ##return the inverse if its already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
      
        ##return the inverse if its already set
        if(!is.null(m)){
                message("getting cahced data")
                return(m)
                }
         ##Get the matrix from our object
        data <-x$get()
        ##calculation of the inverse using matrix multiplication
        m<-solve(data) %x% data
        
       ##set the inverse to the object
        x$setinverse(m)
        
        #return matrix
        m
        }
