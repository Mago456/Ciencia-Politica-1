#A continuación se van a resolver los ejercicios que el Profesor Magallanes 
#dejó como tarea para la segunda clase.

#Muchos de los comandos que se usan en la resolución son buscados por los propios JPs, por lo que 
#se insta a l@s alumn@s que busquen comandos constantemente en la web. 

#Rtip: el R no tiene problemas reconociendo puntos. Pero sí tiene problemas reconociendo comas, estas sí deben ser recodificadas.
#Rtip: diferencias entre string (conjunto de caracteres), factor (conjunto de número), numeric (número), characters (caracteres)


#Primero,solicito todos los paquetes que voy a utilizar durante todos los ejercicios.
#Si no están instalados, debo instalarlos primero.
library(htmltab)
library(tidyr)
library(stringr)
library(magrittr)

####Problema 1####

#Extraigo la base de datos que voy a limpiar.

url3 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/274.html"

cdio = htmltab(doc = url3, 
               which ='//*[@id="fieldListing"]', #herramientas de desarrollador
               encoding = "UTF-8")

#Empiezo a explorar la base
str(cdio)
names(cdio)

#Cambio los nombres de las variables por algo más amigable y pequeño.
names(cdio) = c("Pais", "co2")

#Modifico la variable que está sucia
#separate es un comando que te permite separar en dos nuevas columnas lo que tenías en una columna según un criterio. 
#Todas las filas deben cumplir el mismo criterio, sino no va a separar algunas celdas.

cdio=separate(cdio,co2,into=c("co2",'xtra1','xtra2'), " ")
head(cdio) #miramos el resultado

#Quiero ver NA's, esto para facilitar la transformación de la base. Deben coincidir los NAs en cada columna para poder reemplazar bien.
table(cdio$xtra1,useNA ='always') #quiero ver los NAs
table(cdio$xtra2,useNA ='always') #quiero ver los NAs
#Por los resultados vemos que no hay NAs

#Eliminando extra2 porque ya no me sirve
cdio$xtra2=NULL

#Recodificamos los billones y millones con exponencial. El MT lo pone como 1 porque al multiplicarlo será el mismo número.
cdio$xtra1=recode(cdio$xtra1,billion=10^9,million=10^6,Mt=1) #recodificamos
table(cdio$xtra1,useNA ='always') #vemos resultado del cambio

#Para poder hacer la multiplicación, cambiamos las comas por nada de la variable co2.
cdio$co2 =gsub(",", "", cdio$co2) #Eliminar caracteres especiales
cdio$co2=as.numeric(cdio$co2) #convertimos a numérico 

#Estandarizando (multiplicando y dividiendo)
cdio$co2=(cdio$co2*cdio$xtra1)/(10^6) #a millones

#Exploramos el resultado final
head(cdio$co2,10) #resultado
summary(cdio)
cdio$xtra1=NULL
head(cdio) #Resultado final

####Problema 2####

demolink = "https://en.wikipedia.org/wiki/Democracy_Index"
demopath = '//*[@id="mw-content-text"]/div/table[2]/tbody'
demo = htmltab(doc = demolink, which =demopath)

View(demo)

#Hay varias formas de cambiar los nombres de variables, pero el camino más lógico aquí es el siguiente:

names(demo)

demo#limpieza solo de nombres. 

names(demo)=str_split(names(demo),">>",simplify = T)[,1]%>%gsub('\\s','',.)
names(demo)

#elimnando columnas raras
names(demo)
demo=demo[,-c(1,11)] #sin Rank ni changes
names(demo)

#eliminando los espacios en blanco en todas las columnas detalladas. 
demo[,-c(1,8,9)]=lapply(demo[,-c(1,8,9)], trimws,whitespace = "[\\h\\v]") # no blanks

#Viendo si todos son numéricos
str(demo)
demo[,-c(1,8,9)]=lapply(demo[,-c(1,8,9)], as.numeric) # a numerico

#RESULTADO FINAL
str(demo)
 
####Problema 3####

url1 = "https://www.cia.gov/library/publications/resources/the-world-factbook/fields/211rank.html" 

gdp = htmltab(doc = url1, 
              which ='//*[@id="rankOrder"]', #herramientas de desarrollador
              encoding = "UTF-8") 

#Explorando
names(gdp)

#Solo se necesitan las columas 2 y 3
gdp = gdp[,c(2,3)]

#cambiando nombres
names(gdp)
names(gdp) = c("Pais", "PBI")
names(gdp)

#Reemplazando el dolar por nada en PBI y chancáncolo encima
gdp$PBI =   gsub("\\$|\\,", "", gdp$PBI) #| es "o" 
head(gdp$PBI,20) #Resultado

#Convirtiendo a numérica
str(gdp$PBI)
gdp$PBI = as.numeric(gdp$PBI)
str(gdp)

#Viendo como queda al final
summary(gdp)
str(gdp)

#LISTO

####Problema 4####

#extrayendo la data
link ="https://en.wikipedia.org/wiki/List_of_freedom_indices"
linkPath ='//*[@id="mw-content-text"]/div/table[2]'
freedom = htmltab(doc =link, which = linkPath)

names(freedom)

#Dandome cuenta que hay caractéres y no números.
str(freedom)
View(freedom)


#Dejando los datos como categóricos y solo cambiando los NA para que las contabilice como tales.
freedom[2:5]= replace(freedom[2:5], freedom[2:5] == "n/a",NA)

View(freedom)

#Modificando los valores a numéricos
head(freedom[2:5])

#subseteamos para jugar de manera separada
sub1=freedom[2:5]

#entonces ahora puedo ir modificando las categorías como variables ordinales
sub1=replace(sub1, sub1 == "free",1)
head(sub1)

#y así deberían cambiar los demás