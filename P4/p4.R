n <-  40
k <- 12

celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}


limite <- n # grietas de que largo minimo queremos graficar

inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}

vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]

propaga <- function(replica) {
  # probabilidad de propagacion interna
  prob <- 1
  dificil <- 0.99
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  while (TRUE) { # hasta que la propagacion termine
    grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
    largo <-  largo + 1
    frontera <- numeric()
    interior <- numeric()
    for (v in 1:vc) {
      vecino <- vp[v,]
      xs <- xg + vecino$dx # columna del vecino potencial
      ys <- yg + vecino$dy # fila del vecino potencial
      if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
        if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
          if (voronoi[yg, xg] == voronoi[ys, xs]) {
            interior <- c(interior, v)
          } else { # frontera
            frontera <- c(frontera, v)
          }
        }
      }
    }
    elegido <- 0
    if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
      if (length(frontera) > 1) {
        elegido <- sample(frontera, 1)
      } else {
        elegido <- frontera # sample sirve con un solo elemento
      }
      prob <- 1 # estamos nuevamente en la frontera
    } else if (length(interior) > 0) { # no hubo frontera para propagar
      if (runif(1) < prob) { # intentamos en el interior
        if (length(interior) > 1) {
          elegido <- sample(interior, 1)
        } else {
          elegido <- interior
        }
        prob <- dificil * prob # mas dificil a la siguiente
      }
    }
    if (elegido > 0) { # si se va a propagar
      vecino <- vp[elegido,]
      xg <- xg + vecino$dx
      yg <- yg + vecino$dy
      distanciam <- abs(i[1]-xg) + abs(i[2]-yg)
      if (distanciam>distanciamax) {
        distanciamax <- distanciam
      }
      
    }
    } else {
      break # ya no se propaga
    }
}

resultados <- c(n, k, replica, largo, distanciamax)
return(resultados)

  if (largo >= limite) {
    png(paste("p4g_", replica, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
  }
  
}

numeros <- numeric()
zonas <- n
semillas <- k
for (n in zonas) {
  for (k in semillas) {
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas
    
    for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- sample(1:n, 1)
        columna <- sample(1:n, 1)
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }
    
    suppressMessages(library(doParallel))
    registerDoParallel(makeCluster(detectCores() - 1))
    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    stopImplicitCluster()
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
    
    limite <- n
    
    vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
    for (dx in -1:1) {
      for (dy in -1:1) {
        if (dx != 0 | dy != 0) { # descartar la posicion misma
          vp <- rbind(vp, c(dx, dy))
        }
      }
    }
    names(vp) <- c("dx", "dy")
    vc <- dim(vp)[1]
    
#for (r in 1:10) { # para pruebas sin paralelismo
#    propaga(r)
#}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
largos <- foreach(r = 1:100, .combine=c) %dopar% propaga(r)
stopImplicitCluster()

numeros <- c(numeros, largos)
  }
}
resultados <- matrix(numeros, ncol = 5, nrow = 1600, byrow = T)

summary(resultados)

#################################################
#graficas caja-bigote
#n celdas para 40x40 (1600)
d12<-c(8,45,83,105,150,540)
d16<-c(6,48,81,104,143,460)
d20<-c(8,48,94,115,171,491)
boxplot(d12,d16,d20, ylab="N de celdas", xlab="12                                          16                                               20
        N de semillas (k)")

#manhatan para 40x40 (1600)
d12<-c(3,13,22,24.82,33,64)
d16<-c(3,13,22,25.09,35.25,70)
d20<-c(4,15,24,27.33,37,75)
boxplot(d12,d16,d20, ylab="Distancia Manhattan", xlab="12                                          16                                               20
N° de semillas (K)")

######################################################################

#celdas para 60x60 (3600)
d12<-c(7,48,100,134,197,508)
d16<-c(7,52,98,123,157,639)
d20<-c(9,50,104,144,196,811)
boxplot(d12,d16,d20, ylab="N de celdas", xlab="12                                          16                                               20
        N de semillas (k)")


#manhatan para 60x60 (3600)
d12<-c(4,14,24,30,41.25,104)
d16<-c(3,15.75,22.50,27.20,36,95)
d20<-c(4,16,27.5,30.45,41,96)
boxplot(d12,d16,d20, ylab="Distancia Manhattan", xlab="12                                                 16                                                      20
N de semillas (k)")

###########################################################3
#celdas para 80x80 (6400)
d12<-c(4,47,90,119,150,577)
d16<-c(7,62,112,161,217,807)
d20<-c(9,49,106,153,217,930)
boxplot(d12,d16,d20, ylab="N de celdas", xlab="12                                                 16                                                      20
N de semillas (k)")

#manhatanpara 80x80 (6400)
d12<-c(2,13.75,25,28.68,37,107)
d16<-c(3,15.75,26.50,34.43,42.25,139)
d20<-c(4,16,26,32,44.25,128)
boxplot(d12,d16,d20, ylab="Distancia Manhattan", xlab="12                                                 16                                                      20
N de semillas (k)")