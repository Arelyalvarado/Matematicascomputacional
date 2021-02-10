#Linea Recta 1
m <- -1 # pendiente
b <- 0 # interseccion

# funcion de la linea recta
f <- function (m , b , x){
  return ( m * x + b )
 }

x <- seq ( -5 , 5, 1)# vector de -5 a 5
y <- f(m , b , x) # evaluamos
plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") # graficamos
abline(h = 0, v = 0)

#Linea Recta 2
m <- 1 # pendiente
b <- 8 # interseccion

# funcion de la linea recta
f <- function (m , b , x){
  return ( m * x + b )
}

x <- seq ( -5 , 5, 1)# vector de -5 a 5
y <- f(m , b , x) # evaluamos
plot(x, y, type = "l", xlab = "Eje X", ylab = "Eje Y") # graficamos
abline(h = 0, v = 0)

#Parabola 1
g <- function ( x)
  {
   return (3*x ^2 + 6*x - 3)
  }

x <- seq ( -5 , 5 , 0.01) # vector de -5 a 5
y <- g(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que pasa por el 0 en las y

#Parabola 2
g <- function ( x)
{
  return (x ^2 + 2*x + 0)
}

x <- seq ( -5 , 5 , 0.01) # vector de -5 a 5
y <- g(x)

plot (x , y , type = "l", xlab = "Eje X", ylab = " Eje Y") # graficamos
abline ( h = 0, v = 0) # una linea horizontal que pasa por el 0 en las x y una linea vertical que pasa por el 0 en las y

#Circunferencia 1
circunferencia  <- function(h, k, r){
  if (r  >= 0){ # r tiene  que  ser  positivo
    if (r == 0){ # si es r = 0, entonces  es un  punto
       plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y") # grafica  del  punto
      } else{
        x <- seq(h - r, h + r, 0.01) # ya que no  podemos  graficar  en todo R^2
        ypositiva  <- k + sqrt(r^2 - ((x - h)^2)) # parte  positiva  de la  circunferencia
        ynegativa  <- k - sqrt(r^2 - ((x - h)^2)) # parte  negativa  de la  circunferencia10
        # graficamos  primero  la  parte  positiva
        plot(x, ypositiva , type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1),
             k + (r + 1)),
             xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa , type = "l") # agregamos  la parte  negativa
        abline(h = 0, v = 0) # agregamos  los  ejes
        points(x = h, y = k, col = "red") # dibujamos  el  centro
        }
    } else{
      return(print("El radio  no es  positivo."))
      }
  }
# ejecutamos  la  funcion
  circunferencia (-2, 1, 3)

#Circunferencia 1
circunferencia  <- function(h, k, r){
  if (r  >= 0){ # r tiene  que  ser  positivo
    if (r == 0){ # si es r = 0, entonces  es un  punto
       plot(x = h, y = k, xlab = "Eje X", ylab = "Eje Y") # grafica  del  punto
      } else{
        x <- seq(h - r, h + r, 0.01) # ya que no  podemos  graficar  en todo R^2
        ypositiva  <- k + sqrt(r^2 - ((x - h)^2)) # parte  positiva  de la  circunferencia
        ynegativa  <- k - sqrt(r^2 - ((x - h)^2)) # parte  negativa  de la  circunferencia10
        # graficamos  primero  la  parte  positiva
        plot(x, ypositiva , type = "l", xlim = c(h - (r + 1), h + (r + 1)), ylim = c(k - (r + 1),k + (r + 1)),xlab = "Eje X", ylab = "Eje Y")
        lines(x, ynegativa , type = "l") # agregamos  la parte  negativa
        abline(h = 0, v = 0) # agregamos  los  ejes
        points(x = h, y = k, col = "red") # dibujamos  el  centro
      }
    } else{
      return(print("El radio  no es  positivo."))
    }
  }
# ejecutamos  la  funcion
circunferencia (-2, 1, 3)

#Elipse 1
elipse <- function(h, k, a, b, horizontal){
  if(a > b){
    c <- sqrt(a^2 - b^2)
    if(horizontal){
        x <- seq(h-a, h+a, 0.01)
        ypositiva <- k + sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
        ynegativa <- k - sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
        plot(x, ypositiva, type = "l", xlim = c(h - (a + 1 ), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)))
        lines(x, ynegativa, type = "l")
        abline(h = 0, v = 0)
        points(x = c(h - c, h + c), y = c(k, k), col = "red")
     }  else{
        x <- seq(h - b, h + b, 0.01)
        ypositiva <- k + sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
        ynegativa <- k - sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
        plot (x, ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1)), ylim = c(k - (a + 1) , k + (a + 1)))
        lines (x, ynegativa , type = "l")
        abline (h = 0, v = 0)
        points (x = c(h, h), y = c(k - c, k + c), col = " red ")
    }
  } else{
    return(print("No cumple con las condiciones para ser una elipse. (a no es mayor que b)"))
  }
}


elipse(2, 2, 25, 9, T) 
  
#Elipse 2
elipse <- function(h, k, a, b, horizontal){
  if(a > b){
    c <- sqrt(a^2 - b^2)
    if(horizontal){
      x <- seq(h-a, h+a, 0.01)
      ypositiva <- k + sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
      ynegativa <- k - sqrt ((b^2 - (b^2/a^2) * ((x - h) ^2) ))
      plot(x, ypositiva, type = "l", xlim = c(h - (a + 1 ), h + (a + 1)), ylim = c(k - (b + 1), k + (b + 1)))
      lines(x, ynegativa, type = "l")
      abline(h = 0, v = 0)
      points(x = c(h - c, h + c), y = c(k, k), col = "red")
    }  else{
      x <- seq(h - b, h + b, 0.01)
      ypositiva <- k + sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
      ynegativa <- k - sqrt ((a^2 - (a^2/b^2) * ((x - h) ^2)))
      plot (x, ypositiva , type = "l", xlim = c(h - (b + 1) , h + (b + 1)), ylim = c(k - (a + 1) , k + (a + 1)))
      lines (x, ynegativa , type = "l")
      abline (h = 0, v = 0)
      points (x = c(h, h), y = c(k - c, k + c), col = " red ")
    }
  } else{
    return(print("No cumple con las condiciones para ser una elipse. (a no es mayor que b)"))
  }
}


elipse(2, 3, 9, 4, T)   

  
  
  
  
  
  
  
  
    