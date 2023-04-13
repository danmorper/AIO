# Daniel Moreno Pérez Ejercicio 6.2 Teoría de colas
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
