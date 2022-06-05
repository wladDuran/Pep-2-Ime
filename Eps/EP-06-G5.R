library(dplyr)
library(ggplot2)
set.seed(685)

# 1. En su desquiciada investigación para acabar con los vampiros, Van Helsing ha descubierto que sus enemigos
# tienen predilección por la sangre humana tipo AII+. El cazador sospecha que estos monstruos tienen
# preferencia por la sangre de los adultos, pero aún no está convencido. Por esta razón, mediante artimañas,
# ha encerrado a 14 niños y 20 adultos con este tipo de sangre en un reducto de vampiros. Tras 3 noches, 3 de
# los niños y 13 de los adultos fueron atacados. ¿Existe relación entre el ataque de vampiros y la edad de la
# víctima?
  
#Se pide determinar si existe relación entre la edad de las víctimas de los vampiros y el ataque que estos realizan.

# H0: la edad no tiene relación con el ataque de los vampiros
# HA: la edad está realcionada con el ataque de los vampiros

#Se definen la cantidad de niños y adultos antes y después del ataque.
ninos <- c(14, 3)
adultos <- c(20, 13)

#Se crea la tabla
tabla <- as.table( rbind ( ninos , adultos ) )
dimnames ( tabla ) <- list ( persona = c(" ninos ", " adultos ") ,
                   ataque = c("antes", "despuÃ©s") )
print(tabla)


# Aplicar prueba exacta de Fisher .
alfa <- 0.05
prueba <- fisher.test (tabla,1 - alfa)
print(prueba)

#Por la prueba de Fisher se acepta H0, por lo que la edad de las víctimas no está relacionada con el ataque de los vampiros.


# 2. Una Universidad ha detectado que muchos de sus nuevos estudiantes ingresan con elevados niveles de 
# ansiedad. Para ello, han decidido evaluar un nuevo programa de bienvenida que busca facilitar la adaptaciÃ³n 
# a la vida universitaria. Para ello, han reclutado a un grupo de 15 voluntarios a quienes se les midiÃ³ el nivel de 
# ansiedad (alto o bajo) antes y despuÃ©s de participar en el programa de bienvenida:
# . 4 estudiantes no presentaron ansiedad ni antes ni despuÃ©s.
# . 5 estudiantes inicialmente ansiosos dejaron de estarlo.
# . 4 estudiantes mantuvieron un elevado nivel de ansiedad.
# . Los 2 estudiantes restantes desarrollaron sÃ­ntomas de ansiedad tras participar en el programa.
# Â¿QuÃ© se puede concluir acerca del nuevo programa de bienvenida?

# Debemos determinar si el programa de bienvenida de la universidad está siendo exitoso en términos de lograr bajar la ansiedad de los estudiantes 
# que lo toman.

#H0: el programa no cambia los niveles de ansiedad en la Universidad
#Ha: el programa sí cambia los niveles de ansiedad en la Universidad


alumnos <- seq(1:15)
antes <- c(rep(" No Ansioso ", 4) , rep(" Ansioso ", 5) , rep(" Ansioso ", 4), rep(" No Ansioso ", 2))
despues <- c(rep(" No Ansioso ", 4) , rep(" No Ansioso ", 5) , rep(" Ansioso ", 4), rep(" Ansioso ", 2))
datos <- data.frame(alumnos, antes, despues)
tablaMcNemar <- table(antes,despues)
print(tablaMcNemar)

pruebaMcNemar <- mcnemar.test(tabla)
print(pruebaMcNemar)

#Por prueba de mcNemar se rechaza H0 en favor de Ha, por lo que el programa estaría cambiando los niveles de ansiedad en la Universidad.

# 3. En noviembre de 2019, se realizÃ³ un estudio acerca de la aprobaciÃ³n al presidente SebastiÃ¡n PiÃ±era entre 440 
# profesores y estudiantes de una prestigiosa universidad, obteniÃ©ndose los resultados que se muestran en la 
# tabla. Â¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?

#Se busca mostrar si las opinones entre los profesores y los estudiantes son similares en la Universidad.

#H0: Los profesores y estudiantes tienen preferencias iguales en cuanto a la aprobación de Sebastián Piñera
#Ha: Los profesores y los estudiantes tienen distintas opiniones en cuanto a la aprobación de Sebastián Piñera

#chi cuadrado homogeneidad

estudiantes <- c(35, 208, 17)
profesores <- c(20, 151, 9)

tabla <- as.table( rbind ( estudiantes , profesores ) )

dimnames ( tabla ) <- list ( persona = c("Estudiantes", "Profesores") ,
                             voto = c("Aprueba", "Desaprueba", "Ninguno") )
print(tabla)

pruebaChi <- chisq.test( tabla )
print(pruebaChi)

#Por prueba de mcNemar, se acepta H0, por lo que los estudiantes y los profesores tendrían iguales preferencias en cuanto a la aprobación de
#Sebastián Piñera.


# 4. La Facultad de IngenierÃ­a desea saber si existe diferencia significativa en el desempeÃ±o de los estudiantes en 
# asignaturas crÃ­ticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobÃ³ o reprobÃ³. Â¿QuÃ© puede concluir la Facultad? IndicaciÃ³n:
#   obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 685. Considere un nivel de 
# significaciÃ³n Î±=0,05.

#Queremos saber si es que existe relación entre las aprobaciones y reprobaciones de las materias de cálculo, algebra y física.

#prueba chi cuadrado independencia


datosTabla <- read.csv2("C:\\Wadito\\EP07Datos.csv")

print(datosTabla)

datosMuestra <- sample(datosTabla, 50, replace = FALSE, prob = NULL)




