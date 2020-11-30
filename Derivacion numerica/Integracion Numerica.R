#Integracion Numerica
rm (list = ls())
dev.off () #Borra graficos
library(ggplot2)

#Grafico Inicial ---
fun1 = function(x) {sin(x)+exp(cos(x^2))+1} # Funcion
curve(fun1, from = 0, to = 2*pi, n = 1000, ylim = c(0,5), ylab = "f(x)")
title("f(x) = seno(x) + exp[coseno(x^2)]+ 1")
abline(h = seq(0.5), v = seq(0.6), col = "red")
curve(fun1, from = 0, to = 2*pi, n = 1000, add =1)

#Ejemplo 1 (1 de 2)----
fun2 = function(x){sin(x)+1} # Funcion a integrar

a = 0
b = 2*pi

curve(fun2, from = a, to = b, n = 1000, ylim = c(0, 2.5), ylab = "f(x)")
title("f(x) = seno(x) + 1")
abline(h = seq(0, 2.5, by = 0.5), v = seq(0,6), col = "red")
abline(h = 0, v = 0, lwd = 2)
curve(fun2, from = a, to = b, n = 1000, add = 1)

x = seq(from = a, to = b, lengh.out = 1000)
fx = fun2(x)
datagg = data.frame(x,fx)
ggplot(datagg, aes(x,fx))+geom_area()

F2 = function(x){-cos(x)+x} #integral indefinida de la Fun2 
(I = F2(2*pi) - F2(0)) # El resultado exacto es 2*pi
I == 2*pi


# Ejemplo 1 (2 de 2) ----
fun2 = function(x){sin(x)+1} #Funcion a Integrar
a = 0
b = 2*pi

dx = pi/4
x = seq(from = a, to = b ,by = dx ) #Creo set de puntos 
fx = fun2(x)
datagg = data.frame(x, fx)
ggplot(datagg, aes(x,fx))+ 
       geom_col(width = dx, position = position_nudge(x = dx/2),
                orientation = "x")+
       geom_point()+
       stat_function(fun=fun2)
          
(I.aprox.pi4 = dx*sum(fun2(x))) # Aproximacion con delta x = pi/4

dx = pi/100
x = seq (from = a, to = b, by = dx) # Creo set de puntos
fx = fun2(x)
datagg = data.frame(x,fx)
ggplot(datagg, aes(x,fx))+
        geom_col(width = dx, position = position_nudge(x = dx/2),
                 orientation = "x")+
        geom_point()+
        stat_function(fun = fun2)
(I.aprox.pi100 = dx*sum(fun2(x)))# Aproximacion con delta x = pi/100

dx = pi/1000
x = seq (from = a, to = b, by = dx) # Creo set de puntos
(I.aprox.pi1000 = dx*sum(fun2(x)))# Aproximacion con delta x = pi/1000

dx = pi/100000
x = seq (from = a, to = b, by = dx) # Creo set de puntos
(I.aprox.pi1000 = dx*sum(fun2(x)))# Aproximacion con delta x = pi/100000

dx = 10^-6
x = seq(from = a, to = b, by = dx) #creacion del set de puntos
(I.aprox.dx = dx*sum(fun2(x)))# Aproximacion con delta x = 10^-6

##Ejemplo 2 [Grafico Inicial solucion]
fun1 = function(x){sin(x)+exp(cos(x^2))+1} #funcion a integrar
a = 0
b = 2*pi
dx = 10^-6
x = seq(from = a, to = b, by = dx)
(I.aprox.dx = dx*sum(fun1(x)))

x= seq(from = a, to = b, length.out = 10000)
fx = fun1(x)
datagg = data.frame(x,fx)
ggplot(datagg, aes(x,fx))+geom_area()

dx = pi/4
x= seq(from = a, to = b, by = dx)
(I.aprox.dx = dx*sum(fun1(x)))

dx = pi/100
x= seq(from = a, to = b, by = dx)
fx = fun1
datagg = data.frame(x,fx)
ggplot(datagg, aes(x,fx))+
  geom_col(width = dx, position = position_nudge(x = dx/2),
           orientation = "x")+
            geom_point()+
            stat_function(fun=fun1, n =1000)
(I.aprox.dx = dx*sum(fun1(x)))

dx = pi/1000
x= seq(from = a, to = b, by = dx)
(I.aprox.dx = dx*sum(fun1(x)))


  