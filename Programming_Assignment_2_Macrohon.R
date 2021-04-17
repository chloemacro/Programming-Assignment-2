createMtrx <- function(a = matrix()) {
  mtrxInverse <- NULL
  def <- function(b) {
    a <<- b
    mtrxInverse <<- NULL
  }
  gain <- function() a
  defInv <- function(MtrxInv) mtrxInverse <<- MtrxInv
  gainInv <- function() mtrxInverse
  list(def = def, gain = gain, defInv = defInv, gainInv = gainInv)
}

solveMtrx <- function(a, ...) {
  mtrxInverse <- a$gainInv()
  if (!is.null(mtrxInverse)) {
    message("Collecting the Data")
    return(mtrxInverse)
  }
  mtrx <- a$gain()
  mtrxInverse <- solve(mtrx, ...)
  a$setInv(mtrxInverse)
  mtrxInverse
}
