l <- 1.5
n <- 50 
pi <- 0.05 
pr <- 0.02 
v <- l / 30 
PV <- seq(0,1,0.1) #probabilidad
r <- 0.1 #umbral
m <- 1
resultado <- data.frame()

for(pv in PV){ #vacuna
  for(rep in 1:40){
    agentes <- data.frame(x = double(), y = double(), dx = double(), dy = double(), estado  = character(), amigo = NULL)
    for (i in 1:n) { 
      if(runif(1) < pv){ 
        e <- "R"
      } else if(runif(1) < pi){
        e <- "I"
      } else{
        e <- "S"
      }
      
      agentes <- rbind(agentes, data.frame(x = runif(1, 0, l), y = runif(1, 0, l),
                                           dx = runif(1, -v, v), dy = runif(1, -v, v),
                                           estado = e))
      
      levels(agentes$estado) <- c("S", "I", "R")
    }
    
    epidemia <- integer()
    may <- 0
    act <- 0
    
    while(TRUE) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        
        a$x <- a$x + a$dx
        a$y <- a$y + a$dy
        if (a$x > l) {
          a$x <- a$x - l
        }
        if (a$y > l) {
          a$y <- a$y - l
        }
        if (a$x < 0) {
          a$x <- a$x + l
        }
        if (a$y < 0) {
          a$y <- a$y + l
        }
        agentes[i, ] <- a
      }
    
      }
      act <- infectados
      if(may < act){
        may <- act
      } else if(act < may){
        break
      }
     
      m <- m + 1 #recuperados
    }
    max_infectados <- max(epidemia)
    porcentaje <- 100 * max_infectados / n
    resultado <- rbind(resultado, c(pv, rep, max_infectados, porcentaje))
    print(pv) 
  }

colnames(resultado) <- c("Probabilidad", "Replicas", "Max_Infectados", "Porcentaje")
print(resultado)

#Grafica caja-bigote

p0 <-c(12,10,4,2,16,24,0,44,8,38,46,52,32,12,18,12,16,50,18,0,34,38,16,14,8,2,4,14,28,12)
p1 <-c(8,8,0,0,22,10,4,0,10,8,0,14,0,10,20,12,8,24,24,6,50,8,8,12,6,24,34,14,6,6)
p2 <-c(10,24,0,26,10,42,10,12,2,0,6,6,14,8,26,0,12,12,6,4,6,10,4,8,18,8,4,8,8,16)
p3 <-c(2,6,12,10,26,8,10,16,6,26,0,12,2,2,8,2,8,12,6,10,0,8,8,18,4,10,18,6,30,26)
p4 <-c(12,16,16,0,6,16,10,12,0,8,0,2,4,4,10,12,18,0,6,0,44,2,0,10,2,2,4,16,8,12)
p5 <-c(0,0,0,0,12,8,14,0,8,18,24,6,0,2,4,6,0,2,4,6,0,8,6,0,10,10,0,12,4,4,0,0,6,0)
p6 <-c(0,20,4,0,0,0,0,0,0,0,14,6,2,4,0,10,12,2,4,0,2,10,0,10,0,8,12,12,0,8)
p7 <-c(0,0,0,6,0,16,2,6,0,0,4,0,0,6,0,4,4,4,8,14,0,0,2,0,2,4,0,10,8)
p8 <-c(2,0,0,0,0,0,0,6,0,6,4,4,4,2,0,0,0,2,2,6,0,4,4,0,2,2,0,0,0,0)
p9 <-c(0,0,0,4,0,0,0,0,0,2,0,0,0,0,0,0,0,0,6,4,0,0,0,0,0,0,0,0)
p10 <-c(0)

pp0 <-c(0.00,    8.50,   15.00,   19.47,   31.00,   52.00)
pp1 <-c(0.00,    6.00,    8.00,  11.87,   14.00,   50.00)
pp2 <-c(0.00,    6.00,    8.00,   10.67,   12.00,   42.00)
pp3 <-c(0.0,     6.0,     8.0,    10.4,    12.0,    30.0)
pp4 <-c(0.0,     2.0,     7.0,     8.4,    12.0,    44.0)
pp5 <-c(0.000,   0.000,   4.000,   5.118,   8.000,  24.000)
pp6 <-c(0.000,   0.000,   2.000,   4.667,   9.500,  20.000)
pp7 <-c(0.000 ,  0.000,   2.000,   3.448,   6.000,  16.000)
pp8 <-c(0.000,   0.000,   0.000,   1.667,   3.500,   6.000)
pp9 <-c(0.0000,  0.0000,  0.0000,  0.5714,  0.0000,  6.0000)
pp10 <-c(0)



boxplot(pp0,pp1,pp2,pp3,pp4,pp5,pp6,pp7,pp8,pp9,pp10, xlab= "Probabilidad", names= c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1"), ylab="Porcentaje de infectados",
        ylim = c(0,100)) 









