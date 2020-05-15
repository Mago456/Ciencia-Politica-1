library(htmltab)
link="https://www.cia.gov/library/publications/resources/the-world-factbook/fields/257.html"
url='//*[@id="fieldListing"]'

elec = htmltab(doc = link, 
               which = url, #herramientas de desarrollador
               encoding = "UTF-8")
names(elec) = c("País", "Electricidad")

head(elec)

#Extraer solo el número

elec$Electricidad=str_extract_all(elec$Electricidad, "(\\d+\\.*\\d*)(?=\\%)")

head(elec)

#Convertir el número

elec$Electricidad = as.numeric(elec$Electricidad)

#Respondiendo la pregunta de Media, mediana y demás...
summary(elec$Electricidad)
