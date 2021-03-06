#Enunciado

#En el poder judicial tienen un sistema que clasifica los documentos por categor�as, con 
#el objetivo de ordenarlos de manera eficiente, las categor�as m�s frecuentes son las 
#medidas de protecci�n y oficios con solicitudes.En cuanto a los oficios, estos van 
#asociados a un caso, el cual se puede repetir m�s de una vez.

#A continuaci�n se le entrega una lista con medidas de protecci�n (mp) y oficios (of):

listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                          c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                          c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                          c("mp","Antonia"),c("mp","Christian","Mario"),
                          c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                          c("of","av04","dape"),c("of","av02","arme"))

#Dentro de los oficios podemos encontrar tem�ticas como: Antecedentes (ante), Datos 
#Personales (dape), Medidas Cautelares (meca), Audiencia de Revisi�n de Medidas (arme) y 
#Ampliaci�n Medidas de Protecci�n (ampr). Estas pueden ser aceptadas o rechazadas por el 
#tribunal.

#Ejercicio 1 (3 ptos): Las mp tienen los nombres de las personas a las cuales est�n 
#asociadas, realice una funci�n que cuente cu�ntos ni�os hay por cada una, y entregue 
#una estad�stica de cu�ntos ni�os hay por mp.

#Ejemplo:
#Se cuentan con 5 mp de 2 ni�os
#Se cuentan con 4 mp de 1 ni�os
#Se cuentan con 2 mp de 3 ni�os

##Desarrollo

#Receta de dise�o
#recuentoMp: list(c()) -> int y string "Se cuentan con n mp de n ni�os"
#recuentoMp: devuelve un recuento de la cantidad de medidas de protecciones de la lista
#y cuantos ni�os existen por cada "mp", mediante un for que recorre la lista
#ejemplo: recuentoMp(c("mp","a","b"),c("mp",c",b")) devuelve Se cuentan con 2 mp de 2 ni�os"

recuentoMp<- function(list){

for (i in listaDocumentos){    
  if(i[1]=="mp"){
    mp<- 0
    ni�os<- 0
    for (j in length(i)-1){
      mp<- mp+1
      ni�os<- ni�os+(length(i)-1)
    }
    print(paste("Se cuentan con",mp,"mp de",ni�os,"ni�os"))
  }  
  }
}

recuentoMp(listaDocumentos)
summary(mp)
summary(ni�os)

#Ejercicio 2 (3 ptos): Los oficios est�n compuestos por el c�digo al cual pertenecen, 
#construya una funci�n que almacene los c�digos y las tem�ticas a las que est�n 
#asociadas.

#Ejemplo:
#av01, ante, arme, ampr
#av02, arme
#av03, dape, ampr

##Desarrollo

#Receta de dise�o
#tematicasOficio: list(c()) -> string "codigo ("av0n") , tematica ("ampr")"
#tematicasOficio: almacena los codigos junto con las tem�ticas asociadas de todos los
#oficios de la lista
#ejemplo: tematicasOficio(c("of","av01","ante"),c("of","av01",arme")) devuelve: 
#"av01","ante","arme"

tematicasOficio<- function(list){

av01 <- c()
av02 <- c()
av03 <- c()
av04 <- c()
av05 <- c()
av07 <- c()
av08 <- c()

for (k in listaDocumentos) {
  if(k[1]=="of"){
    if(k[2]=="av01"){
      av01 <- c(av01,k[3])
    }
    if(k[2]=="av02"){
      av02 <- c(av02,k[3])
    }
    if(k[2]=="av03"){
      av03 <- c(av03,k[3])
    }
    if(k[2]=="av04"){
      av04 <- c(av04,k[3])
    }
    if(k[2]=="av05"){
      av05 <- c(av05,k[3])
    }
    if(k[2]=="av07"){
      av07 <- c(av07,k[3])
    }
    if(k[2]=="av08"){
      av08 <- c(av08,k[3])
    }
  }
}
print(paste("av01",",",av01[1],",",av01[2],",",av01[3]))
print(paste("av02",",",av02[1],",",av02[2],",",av02[3]))
print(paste("av03",",",av03[1]))
print(paste("av04",",",av04[1]))
print(paste("av05",",",av05[1]))
print(paste("av07",",",av07[1]))
print(paste("av08",",",av08[1]))
}

tematicasOficio(listaDocumentos)


#Ejercicio 3(2 ptos): Construya una funci�n que act�e de juez y retorne aprobada o 
#reprobada para los diferentes oficios, y entregue la cantidad que hay de cada una.

#Ejemplo:
#Llegaron 10 oficios de los cuales: 7 son aprobados y 3 reprobados

##Desarrollo

#Receta de dise�o
#funcionJuez: list(c()) -> string "Se cuentan con n aprobados y n reprobados"
#funcionJuez: recorre todos los oficios de la lista, si corresponde a una "dape" la aprueba
#sino la reprueba y finalmente entrega un recuento de todos los oficios recorridos
#ejemplo: funcionJuez(c("of","av01","dape"),c("of","av01",arme")) devuelve: "Se cuentas con
#1 aporobada y 1 reprobada

funcionJuez <- function(list){

aprobado <- 0
reprobado <- 0

for (l in listaDocumentos){
  if(l[1]=="of")
  {
    if(l[3]=="dape")
    {
      print("aprobado")
      aprobado <- aprobado+1
    } else {
      print("reprobado")
      reprobado<- reprobado+1
    }
  }
}    
print(paste("Llegaron",aprobado+reprobado,"oficios de los cuales:",aprobado,"son aprobados y",reprobado,"reprobados"))
}

funcionJuez(listaDocumentos)


###Juan Acu�a Vega
###Curso de Big Data, ICI, Utem 2020