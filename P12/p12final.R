
cluster <- makeCluster(detectCores() - 1)


binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}
decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}
 
r <- 5
c <- 3
dim <- r * c
tranqui <- 0.99
tope <- 9
digitos <- 0:tope
k <- length(digitos)

#repeticiones
replica<-20

tmax<-5000
entrenamiento <- ceiling(0.7 * tmax)
prueba <- tmax - entrenamiento
datos<- data.frame( Replica= integer(), Negras=integer(), Grises=integer(), Blancas=integer(), Porcentaje=integer())


 
modelos <- read.csv("modelo", sep=" ", header=FALSE, stringsAsFactors=F)

#variar probabilidades 
for (PN in c(0.995,0.92,0.002)) {
  for(PG in c(0.92,0.002,0.995)){
    for(PB in c(0.002,0.995,0.92)){
     
      modelos[modelos=='n'] <- pn # pixeles negros en plantillas
      modelos[modelos=='g'] <- pg # pixeles grises en plantillas
      modelos[modelos=='b'] <- pb # pixeles blancos en plantillas
      for(replicas in 1:replica){
        print(replicas)
      
        tasa <- 0.15
        contadores <-vector()
        n <- floor(log(k-1, 2)) + 1
        neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones
       
        
         #ENTRENAMIENTO
        for (t in 1:entrenamiento) { # entrenamiento
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,]
          correcto <- binario(d, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            if (deseada != resultado) {
              ajuste <- tasa * (deseada - resultado)
              tasa <- tranqui * tasa
              neuronas[i,] <- w + ajuste * pixeles
            }
          }
        }
        clusterExport(cluster, c( "neuronas", "binario", "decimal", "modelos", "tope" ,"k", "dim", "n"))
        
        
        #PRUEBA 
        contadores <-parSapply(cluster, 1:prueba , function(x){
          d <- sample(0:tope, 1)
          pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
          correcto <-binario(d, n)
          salida <- rep(FALSE, n)
          for (i in 1:n) {
            w <- neuronas[i,]
            deseada <- correcto[i]
            resultado <- sum(w * pixeles) >= 0
            salida[i] <- resultado
          }
          r <- min(decimal(salida, n), k) 
          return(r==d)})
        datos<-rbind(datos, data.frame(Replica= replicas, Negro=pn, Gris=pg, Blanco=pb, Porcentaje=(sum(contadores)/prueba)*100))
    
      }
    }
  }
}

#plots

colnames(datos)<-c("Replica", "Negro","Gris","Blanco","Porcentaje")


ggplot(datos, aes(Blanco, Gris)) + 
  geom_raster(aes(fill=Porcentaje)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("plot_GB.png")

ggplot(datos, aes(Negro, Gris)) + 
  geom_raster(aes(fill=Porcentaje)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("plot_GN.png")

ggplot(datos, aes(Negro, Blanco)) + 
  geom_raster(aes(fill=Porcentaje)) + 
  scale_fill_gradient(low="yellow", high="red")
ggsave("plot_BN.png")







