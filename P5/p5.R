inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)

f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png",width = 1080, height = 880 ) # dibujamos f(x) para ver como es
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))),xlab="x",ylab="g(x)")
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()

suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador
muestra <- generador(50000) # sacamos una muestra
png("p5m.png",width = 1080, height = 880) # validamos con un dibujo
hist(muestra, freq=F, breaks=50, main=" ",
     xlim=c(inicio, final), ylim=c(0, 0.4), xlab="x", ylab="Densidad")
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()

desde <- 3
hasta <- 7
cuantos <- 500

vectores=data.frame()

parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

valor <-0.048834

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() -1 ))


for (pedazo in c(100,1000,10000,100000,1000000)) {
  print(paste("pedazo", pedazo))
  for (repeticiones in 1:50) {
    montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
    integral <- sum(montecarlo) / (cuantos * pedazo)
    resultados <- (pi / 2) * integral
    diferencia <- (valor-resultados)
    vectores <- rbind(vectores, c(repeticiones, pedazo, valor, resultados, diferencia))
    
  }
}
stopImplicitCluster()

names(vectores)=c("replica","muestra","real","estimado","error")

png("p5e.png") 
boxplot(data=vectores,error~muestra,xlab="Tamaño de muestra",ylab="Error",main="")
abline(h=0, col="red", pch=15)
graphics.off()

png("p5a.png") 
boxplot(data=vectores,estimado~muestra,xlab="Tamaño de muestra",ylab="Aproximacion",main="")
abline(h=0.048834, col="red", pch=15)
graphics.off()