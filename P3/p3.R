primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

noprimos=c(valores)
mispri=c(valores)

combinacion <- sort(sample(c(mispri, noprimos)))
original <- combinacion
invertido <- rev(combinacion)
replicas <- 10
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 3))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
}
stopImplicitCluster()
summary(ot)
summary(it)
summary(at)

#data.frame

proporcion1=c(75, 75, 75) #tomando como relevancia los numeros primos
proporcion2=c(50,50,50)
proporcion3=c(25,25,25)

tiempo1=c(valores)  
          
tiempo2=c(valores)  
  
tiempo3=c(valores)  
          
combinaciones=c("ot", "it", "at")

cincoDig1 <- data.frame(proporcion1, tiempo1, combinaciones)
cincoDig2 <- data.frame(proporcion2,tiempo2,combinaciones)
cincoDig3 <- data.frame(proporcion3,tiempo3,combinaciones)

#graficas caja-bigote para 5,7 y 10 dígitos
# 3,2, y 1 nucleo

boxplot(tiempo1,tiempo2,tiempo3, xlab="75                                          50                                           25", 
        ylab="tiempo(m)", main="1 núcleo", col=c(125,230,448))


