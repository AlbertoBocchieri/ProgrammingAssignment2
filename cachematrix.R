## In Italian
## Coppia di funzioni che memorizzano nella cache l'inversa di una matrice
## Passa il risultato di una chiamata makeCacheMatrix a cacheSolve

#' Util function che imposta la matrice e l'inversa in un ambiente
#' una matrice invertibile, per esempio
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' x$set(matrix(rnorm(16), 4, 4))
makeCacheMatrix <- function(x = matrix()) {
  # errore se x non è una matrice
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#' Calcola e memorizza nella cache l'inversa di una matrice
#' il risultato di una precedente chiamata a makeCacheMatrix
#' argomenti aggiuntivi da passare per risolvere la funzione; per esempio
#' x = makeCacheMatrix(matrix(rnorm(9), 3, 3))
#' cacheSolve(x)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}