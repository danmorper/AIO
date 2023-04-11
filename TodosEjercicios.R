# 6.1----

#Sacado de queueing pag3
# Load the package
library(queueing)
# Create the inputs for the model.
i_mm1 <- NewInput.MM1(lambda=2, mu=3) 
# Optionally check the inputs of the model
CheckInput(i_mm1) #si cambias a lambda=4 y mu=3 te da fatal error
# Create the model
o_mm1 <- QueueingModel(i_mm1)  
# Print on the screen a summary of the model
print(summary(o_mm1), digits=2) 
#Tenemos que saber interpretar cada uno (lambda, mu, c, k ,m, RO, P0...)

fw <- o_mm1$FW
fwq <- o_mm1$FWq
n <- 10
ty <- "l"
ylab <- "FW(t), FWq(t)"
xlab <- "t"
cols <- c("black", "red")
leg <- c("FW(t)", "FWq(t)")
curve(fw, from=0, to=n, type=ty, ylab=ylab, xlab=xlab, col=cols[1], main=gTitle)
curve(fwq, from=0, to=n, type=ty, col=cols[2], add=T)
legend("bottomright", leg, lty=c(1, 1), col=cols)

# 6.2----

#7 horas = 7/24 dias mu=1/E(poisson)
i_mm1 <- NewInput.MM1(lambda=3, mu=24/7) 

CheckInput(i_mm1)

o_mm1 <- QueueingModel(i_mm1)  
o_mm1$W 

coste_fijo <- 375
coste_hora <- 25
coste_total <- coste_fijo+coste_hora*o_mm1$W
paste("El coste total de arreglar un coche es el coste de arreglarlo,", coste_fijo, ",más 25euros/dia*numeros de dias que tarda en arreglarse", o_mm1$W, ", es decir,", coste_total)

i_mm1_new <- NewInput.MM1(lambda=3, mu=24/5) 
CheckInput(i_mm1_new) #si cambias a lambda=4 y mu=3 te da fatal error
o_mm1_new <- QueueingModel(i_mm1_new)  
o_mm1_new$W #Tiempo medio estancia de los clientes en el sistema

paste("Será rentable si el nuevo coste de arreglar el cochees menor de", coste_fijo+coste_hora*(o_mm1$W+o_mm1_new$W))





# 6.3---
library(queueing)
lambda <- 60
mu <- 60*60/150
c <- 3
i_mm3 <- NewInput.MMC(lambda=lambda, mu=mu, c=c) 
i_mm3
QM3 <- QueueingModel(i_mm3)
QM3
QM3$W
paste("El tiempo hasta que ha sido atendido en horas es", QM3$W)

NI_MM1 <- NewInput.MMC(lambda=lambda, mu=c*mu, c=1)
NI_MM1
QM1 <- QueueingModel(NI_MM1)
QM1$W
paste("El tiempo hasta que ha sido atendido en una sola cola en horas es", QM1$W)

# 6.8 Lavadero de vainas ---------------------------------------

lambda <- 20
mu <- 60/12
k = 10 # limitacion del servicio

i_mm1k = NewInput.MM1K(lambda,mu,k)
i_mm1k
CheckInput(i_mm1k)
QM3 = QueueingModel(i_mm1k)
QM3
sum(QM3$Pn)

p1 = QM3$Pn[11]*lambda*10
cat('El número de coches que se pierden en un día es',p1)

# Ejercicio9 ----
#Cuál es la prob de que haya 
#a,b,c)
lambda <- 12
c <- 1
mu <- 4*2.4
k <- 6

cola <- NewInput.MM1K(lambda,mu,k)
cola
CheckInput(cola)
QM3 <- QueueingModel(cola)
QM3$Pn
sum(QM3$Pn)
summary(QM3) #aquí se ven la soluciones

p1 <- QM3$Pn[11]*lambda*10
p1


#d)
lambda <- 12
c <- 1
mu <- 4*2.4
k <- 7

cola <- NewInput.MM1K(lambda,mu,k)
cola
CheckInput(cola)
QM3 <- QueueingModel(cola)
QM3$Pn #Me tendría que fijar en cuantos barcos más van a poder pasar
sum(QM3$Pn)

# Ejercicio 12----
# Modelo MM ? (no hay espacio de capacidad, por eso hay espacio en blanco)
#? e smayor que 5, hay que estudiar la población de maquinas

#El primer k es la capacidad del sistema y la segunda el tamaño de la pob
#K es numero de maquinas en la estacion, la cola me dará el número de máquinas estropeadas
#Si tienes 5 maquinas tienes que calcular la probabilidad de que haya 0 estropeadas
#si tienes 27 maquinas tienes que calcular la probabilidad de que haya menos o igual a 22 maquinas estropeadas
#Siempre tiene haber 5 maquinas funcionando

#Estoy muy cansada, chica :(
for (lambda in 5:30) {
  cola <- NewInput.MM1KK(lambda =lambda,)
}

# 6.23 PROBLEMA DE EXAMEN----
#lambda=50000/240
#mu es Numero de puertas 480/5 en la tercera etapa
# a)

lambda = 50000/240
mu = c(220,140,480/5)
c = c(1,2,3)

NI_MMC <- NewInput.MMC(lambda,mu[1],c[1])
NI_MMC
QM1 <- QueueingModel(NI_MMC)
QM1$L

NI_MMC <- NewInput.MMC(lambda,mu[2],c[2])
NI_MMC
QM2 <- QueueingModel(NI_MMC)
QM2$L

NI_MMC <- NewInput.MMC(lambda,mu[3],c[3])
NI_MMC
QM3 <- QueueingModel(NI_MMC)
QM3$L

# b)
NI_MMC <- NewInput.MMCK(lambda,mu[2],c[2], k=7)
NI_MMC
QMK <- QueueingModel(NI_MMC)
QMK$Pn

# c)

# d)
lambda <- 70000/240





#Chatgpt
# Instalar y cargar la librería 'queueing'
install.packages("queueing")
library(queueing)

# Tasas de servicio y llegada
llegadas <- 50000 / (240 * 480)
servicio1 <- 220 / 480
servicio2 <- 140 / 480
servicio3 <- 1 / 5

# Crear modelos de cola para cada etapa
etapa1 <- queueing::NewInput.MM1(llegadas, servicio1)
etapa2 <- queueing::NewInput.MM1(llegadas, servicio2 * 2)
etapa3 <- queueing::NewInput.MM1(llegadas, servicio3 * 3)

# Calcular el número de puertas en cada etapa
puertas_etapa1 <- queueing::L(etapa1)
puertas_etapa2 <- queueing::L(etapa2)
puertas_etapa3 <- queueing::L(etapa3)

# Imprimir resultados
cat("Puertas en la etapa 1:", puertas_etapa1, "\n")
cat("Puertas en la etapa 2:", puertas_etapa2, "\n")
cat("Puertas en la etapa 3:", puertas_etapa3, "\n")

# 6.21 Reparaciones electrónicas ----
#1 M/M/1 ; lambda = 1 ; mu = 10
#2 M/M/3 ; lambda = 0 ; mu = 60/35
#3 M/M/4 ; lambda = 0 ; mu =60/65
#4 M/M/2 ; lambda = 0 ; mu = 60/12.5

#Matriz de probabilidades c(0,57*0.83, 0.17, )
library(queueing)
datos<-c(0,0.17,0.57*0.83,0.43*0.83,
         0,0,0,0,
         0.05,0.95,0,0,
         0,1,0,0)
probs<-matrix(data = datos,byrow = T,nrow = 4,ncol = 4)
I<-NewInput.MM1(lambda=9,mu=10)
q_I<-QueueingModel(I)
RG<-NewInput.MMC(lambda = 0,mu=60/35,c=3)
q_RG<-QueueingModel(RG)
E<-NewInput.MMC(lambda = 0,mu=60/65,c=4)
q_E<-QueueingModel(E)
F<-NewInput.MMC(lambda = 0,mu=60/12.5,c=2)
q_F<-QueueingModel(F)
Empresa<-NewInput.OJN(prob = probs, I,F,RG,E)
CheckInput(Empresa)
q_empresa<-QueueingModel(Empresa)
summary(q_empresa)

Ap=q_empresa$Lk; Ap #Aparatos por término medio en cada nodo
Ap[1] #de media
Ap[2] #en el inicio
Ap[2] #en reparación

Tiempo = q_empresa$Wk
Tiempo[1] #de media pasa
Tiempo[2] #horas en inicio
Tiempo[3] #en fábrica
Tiempo[4] # en algo (?)

q_empresa$W #tiempo medio en horas que un aparato pasa en la  empresa

# 6.23 Gran Muralla china----
#Rollos lambda=12+0-8 mu=60/4=15
#Pollo lambda=8+0.2*12=10.4 mu=60/5=12

#pito