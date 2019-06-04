library(ggplot2)
library(lattice)
resultados <-data.frame()


n <- 100 #partículas generadas aleatoriamente
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), ad = rep(FALSE, n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
adsorbente <- 0.3
ad <- 0.05 #fuerza de atraccion al adsorbente

ggplot(data=p, aes(x=x ,y=y, col=colores[p$g+6])) + 
  geom_point(show.legend =  TRUE) + xlim(c(0,1)) + ylim(c(0,1)) +  
  ggtitle(paste("Estado inicial")) + scale_shape_discrete(name  ="Carga") + 
  scale_colour_discrete(name  ="Carga", labels=seq(-5,5))
ggsave(paste("p9_1Nt", 0, ".png", sep=""))

eps <- 0.001
grav <- 0

fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy))
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 3))

tmax <- 50
replicas <- 3 #replicas para prueba estadistica

digitos <- floor(log(tmax, 10)) + 1 #para nombrar imagenes
tl <- "0"
while (nchar(tl) < digitos) {tl <- paste("0", tl, sep="")}

for(rep in replicas){
  inicial<- Sys.time()
for(iter in 1:tmax){
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  xcombine <- p$x
  ycombine <- p$y
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  
  
  
  # Buscar maxima diferencia alcanzada
  xdifmax <- numeric(n)
  xdif <- abs(xcombine - p$x)
  ydifmax <- numeric(n)
  ydif <- abs(ycombine - p$y)
  
  for(i in 1:n){
    if (xdif[i] > xdifmax[i]){
      xdifmax[i] <- xdif[i]
    }
    if (ydif[i] > ydifmax[i]){
      ydifmax[i] <- ydif[i]
    }
    if(p$y[i] > adsorbente & p$y[i] < adsorbente + 0.1){ #indica la fuerza de atracción hacia el adsorbente para las partículas
      p$y[i] <- p$y[i] - ad
    }
    if(p$y[i] < adsorbente){
      p$y[i] <- p$y[i] + ad
    }
    if(p$y[i] == adsorbente | (p$y[i] < adsorbente + 0.02 & p$y[i] > adsorbente - 0.02)){
      p$y[i] <- adsorbente
      p$x[i] <- p$x[i] 
      p$ad[i] <- TRUE
    }
    if(p$ad[i]){
      p$y[i] <- adsorbente
      p$x[i] <- p$x[i] 
    }
  }
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  
  ggplot(data=p, aes(x=x ,y=y,col=colores[p$g+6])) + 
    geom_hline(aes(yintercept=adsorbente, size=adsorbente), colour="red") + 
    geom_point(show.legend =  FALSE) + xlim(c(0,1)) + ylim(c(0,1)) +  
    ggtitle(paste("Paso ", iter)) + scale_shape_discrete(name  ="Payer") + 
    scale_colour_discrete(name  ="Payer", labels=seq(-5,5 ))
 ggsave(paste("p9_1Nt", tl, ".png", sep=""))
  
  p$v <- foreach(i = 1:n, .combine=c) %dopar% (xdifmax[i] + ydifmax[i])
  p$vel <- p$v * p$c
  print(iter)
      }
  
  termino <- Sys.time()
  resultado <- rbind(resultados, c(termino-inicial, rep, iter))
}
names(resultado) <- c("Tiempo", "Repeticiones", "Pasos")
stopImplicitCluster()




