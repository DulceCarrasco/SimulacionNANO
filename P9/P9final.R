library(ggplot2)
library(lattice)

n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=rnorm(n))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
mmax <- max(p$m)
mmin <- min(p$m)
p$m <- ((p$m - mmin)*(p$m - mmin) / (mmax - mmin))+1 #masa entre 1 y 5

p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)



png("p9i.png")
xyplot(p$y ~ p$x,cex=p$m, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="",
       par.settings = list(superpose.symbol = 
       list(pch = 15, cex = 2,labs="masa",
      col = colores)))
graphics.off()



png(paste("p9_t", 0, ".png", sep=""),width = 800,height = 700)
print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )
       +geom_point(show.legend =  TRUE)+xlim(c(0,1))+ylim(c(0,1))+  
         ggtitle(paste("Estado inicial"))
 + scale_shape_discrete(name  ="Carga")+ 
scale_colour_discrete(name  ="Carga", labels=seq(-5,5)))
graphics.off()



eps <- 0.001
grav <- 0

fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m #masa
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
  return(c(fx, fy)/(mi+1)) #masa que no tome valores de 0
}


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
plot(p$x, p$y, col=colores[p$g+6], pch=15, cex=1.5, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),
     main="Estado inicial", xlab="X", ylab="Y")
graphics.off()
for (iter in 1:tmax) {
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
  for (i in 1:n){
    if (xdif[i] > xdifmax[i]){
      xdifmax[i] <- xdif[i]
    }
    if (ydif[i] > ydifmax[i]){
      ydifmax[i] <- ydif[i]
    }
  }
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png", sep=""))
 print( ggplot(data=p, aes(x=x ,y=y, size=m, col=colores[p$g+6]) )+geom_point(show.legend =  FALSE)+xlim(c(0,1))+ylim(c(0,1))+  
           ggtitle(paste("Paso ", iter))    )
        g + scale_shape_discrete(name  ="Payer")+ 
          scale_colour_discrete(name  ="Payer", labels=seq(-5,5 ))
  graphics.off()

p$v <- foreach(i = 1:n, .combine=c) %dopar% (xdifmax[i] + ydifmax[i])
p$v <- p$m * p$v
p$vel <- p$v * p$c


png(paste("p9_thism", tl, ".png", sep=""))
hist(p$vel,freq=TRUE, col="lightcyan",
   main=paste("Paso ", tl, sep=""),xlab="Relacion velocidad-masa", ylab=""
   ,cex.lab=1.5,cex.main=1.5)
graphics.off()

 png(paste("p9_thisc", tl, ".png", sep=""))
hist(p$vel,freq=TRUE, col="lightcyan",
main=paste("Paso ", tl, sep=""),xlab="Relacion velocidad-carga",ylab=""
 ,cex.lab=1.5,cex.main=1.5)
graphics.off()


png(paste("p9_pairs", tl, ".png", sep=""))
pairs(~x+y+v+c+m,data=p, main=paste("Paso ", tl, sep=""))
graphics.off()
}
stopImplicitCluster()
colnames(p)[which(names(p) == "w")] <- "m"

print(iter)







