# Tarea2_FOR_LOOP
En este repositorio se almacenará el desarrollo de la guía 2 correspondiente al curso Big Data.

#Desarrollo Ejercicio 1

#Receta de diseño
#recuentoMp: list(c()) -> int y string "Se cuentan con n mp de n niños"
#recuentoMp: devuelve un recuento de la cantidad de medidas de protecciones de la lista
#y cuantos niños existen por cada "mp", mediante un for que recorre la lista
#ejemplo: recuentoMp(c("mp","a","b"),c("mp",c",b")) devuelve Se cuentan con 2 mp de 2 niños"

recuentoMp<- function(list){

for (i in listaDocumentos){    
  if(i[1]=="mp"){
    mp<- 0
    niños<- 0
    for (j in length(i)-1){
      mp<- mp+1
      niños<- niños+(length(i)-1)
    }
    print(paste("Se cuentan con",mp,"mp de",niños,"niños"))
  }  
  }
}

recuentoMp(listaDocumentos)
summary(mp)
summary(niños)


#Desarrollo Ejercicio 2

#Receta de diseño
#tematicasOficio: list(c()) -> string "codigo ("av0n") , tematica ("ampr")"
#tematicasOficio: almacena los codigos junto con las temáticas asociadas de todos los
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


#Desarrollo Ejercicio 3

#Receta de diseño
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