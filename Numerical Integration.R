## Soal 1 , Contoh soal dan jawaban Trapezoidal Rule pada R 

trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- b - a
  
  fxdx <- (h / 2) * (f(a) + f(b))
  
  return(fxdx)
}

trapezoid(f, 0, pi/2)


## Soal 2 , Contoh soal dan jawaban Simpson Rule pada R 


simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop('f must be a function with one parameter (variable)')
  }
  
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
  
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
  
  return(s)
}

simpsons.rule(f, 0, pi/2)