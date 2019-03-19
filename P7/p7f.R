library(lattice)
library(reshape2)
library(latticeExtra)

g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -3
high <- 3
step <- 0.10
puntos <- 15
tmax <- 1000

#para graficar

x <- seq(low, high, 0.05)
y <- x
z <- outer(x, y, g)
dimnames(z) <- list(x, y)
d <- melt(z)
names(d) <- c("x", "y", "z")




posiciones <- data.frame(x = double(), y = double(), bestgxy = double())
for (n in 1:puntos) {
  currx <- runif(1, low, high)
  curry <- runif(1, low, high)
  posiciones <- rbind(posiciones, data.frame(x = currx, y = curry, bestgxy = g(currx, curry)))
}

salida <- paste("p7_flat0.png")
titulo <- paste("Inicio")
plano <- levelplot(z ~ x * y, data = d, main = titulo)
Puntos <- (xyplot(posiciones$x ~ posiciones$y, col = "black"))
graficap <- plano + as.layer(Puntos)
png(salida, width = 500, height = 500)
print(graficapuntos)
graphics.off()

for (pot in 1:tmax) {
  resulx <- double()
  resuly <- double()
  
  for (n in 1:puntos) {
    delta <- runif(1, 0, step)
    deltax <- c(-delta, 0, delta)
    delta <- runif(1, 0, step)
    deltay <- c(-delta, 0, delta)
    
    #codigo de la practica 4
    vecinosx <- numeric()
    vecinosy <- numeric()
    for (dx in deltax) {
      for (dy in deltay) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
          vecinosx <- c(vecinosx, dx)
          vecinosy <- c(vecinosy, dy)
        }
      }
    }
    
    tablax <- rbind(resulx, vecinosx + posiciones$x[n])
    tablay <- rbind(resuly, vecinosy + posiciones$y[n])
  }
  
  vecinos <- g(resulx, resuly)
  maxvalue <- max.col(vecinos)
  
  for (i in 1:puntos) {
    posiciones$x[i] <- resulx[i,maxvalue[i]]
    posiciones$y[i] <- resuly[i,maxvalue[i]]
  }
  
  salida <- paste("p7_flat", pot, ".png", sep = "")
  titulo <- paste("Paso", pot)
  plano <- levelplot(z ~ x * y, data = d, main = titulo)
  Puntos <- (xyplot(posiciones$x ~ posiciones$y, col = "black"))
  graficap <- plano + as.layer(Puntos)
  png(salida, width = 500, height = 500)
  print(graficap)
  graphics.off()
  
}