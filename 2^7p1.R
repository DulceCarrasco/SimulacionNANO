pos <- 0
mayor <- 0
dur <- 100
for (t in 1:dur) {
  if (runif(1) < 0.5) {
    pos <- pos + 1
  } else {
    pos <- pos - 1
  }
  dist <- abs(pos)
  if (dist > mayor) {
    mayor <- dist
  }
}
euclideana <- function(p1, p2) {
  return(sqrt(sum((p1 - p2)**2)))
}

manhattan <- function(p1, p2) {
  return(sum(abs(p1 - p2)))
}

ed.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(euclideana(p, origen))
}

md.orig <- function(p) {
  dimension <- length(p)
  origen <- rep(0, dimension)
  return(manhattan(p, origen))
}
caminata <- function(dim, dur, dist) {
  pos <- rep(0, dim)
  mayor <- 0
  for (t in 1:dur) {
    cambiar <- sample(1:dim, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    d <- dist(pos)
    if (d > mayor) {
      mayor <- d
    }
  }
  return(mayor)
}
repetir <- 30
duracion <- 2**7

eucl <-  FALSE
library(parallel)

datos <-  data.frame()

experimento <- function(replica) {
  pos <- rep(0, dimension)
  mayor <- 0
  for (t in 1:duracion) {
    cambiar <- sample(1:dimension, 1)
    cambio <- 1
    if (runif(1) < 0.5) {
      cambio <- -1
    }
    pos[cambiar] <- pos[cambiar] + cambio
    if (eucl) {
      d <- sum(sqrt(pos**2))
    } else { # Manhattan
      d <-  sum(abs(pos))
    }
    if (d > mayor) {
      mayor <- d
    }
  }
  return(mayor)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
clusterExport(cluster, "experimento")

for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir, experimento)
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)
if (eucl) {
  png("p1er.png")
  boxplot(data.matrix(datos), use.cols=FALSE,
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", main="Euclideana")
} else {
  png("p1mr.png")
  boxplot(data.matrix(datos), use.cols=FALSE,
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", main="Manhattan")
}
graphics.off()
repetir <- 30
duracion <- 2**7
eucl <-  FALSE
library(parallel)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "eucl")
datos <-  data.frame()

for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  resultado <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           mayor <- 0
                           for (t in 1:duracion) {
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             if (eucl) {
                               d <- sum(sqrt(pos**2))
                             } else { # Manhattan
                               d <-  sum(abs(pos))
                             }
                             if (d > mayor) {
                               mayor <- d
                             }
                           }
                           return(mayor)
                         })
  datos <- rbind(datos, resultado)
}
stopCluster(cluster)
if (eucl) {
  png("p1er.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Euclideana")
} else {
  png("p1mr.png")
  boxplot(data.matrix(datos), use.cols=FALSE, 
          xlab="Dimensi\u{F3}n", ylab="Distancia m\u{E1}xima", 
          main="Manhattan")
}
graphics.off()
