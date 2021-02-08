# Librerías ---------------------------------------------------------------
library(tidyverse)
library(readxl)
library(writexl)
library(sf)
library(rgdal)
library(viridis)
library(janitor)
library(rmarkdown)


# Importación de bases de datos -------------------------------------------

# correspondiente a las transferencias por concepto de la ley 18.450
cnr_nacional <- read.csv("https://github.com/marc0pz/articulo__rtr/raw/main/AR002T0001380.csv", 
                         row.names=NULL, na=T, sep=";") 



# archivo .shp que contiene las coordenadas de las comunas de la región de O'Higgins para su mapeo

#En "destfile" debemos adecuar destino del archivo según nuestro PC (en este caso "C:/Users/marco/Desktop) 
#junto con crear carpeta en el escritorio (en este caso llamada "shp") y descomprimirlo el archivo ("shp_comunas.zip") allí
download.file("https://www.bcn.cl/obtienearchivo?id=repositorio/10221/10396/2/Comunas.zip", 
              destfile="C:/Users/marco/Desktop/shp/shp_comunas.zip") 


#si todo está listo, se procede a leer uno de los archivos descomprimidos ("comunas.shp")
#nuevamente se debe ajustar el path (en este caso "C:/Users/marco/Desktop)
shp_nacional <- read_sf("C:/Users/marco/Desktop/shp/comunas.shp") 

# Limpieza de bases de datos ----------------------------------------------

# rename de variables para simplificar su uso
cnr_nacional <- cnr_nacional %>% rename(codigo="Código.del.Proyecto", 
                                        nombre="Nombre.del.Proyecto" , 
                                        beneficiario="Nombre.del.Beneficiario" , 
                                        cant_beneficiarios="Cantidad.de.beneficiarios" , 
                                        region="Región" , 
                                        provincia="Provincia" , comuna="Comuna" , 
                                        consultor="Consultor" , 
                                        estrato="Estrato" , origen_fondos="Origen.de.los.Fondos" , 
                                        tipo_obra="Tipo.de.Obra" , 
                                        superficie_fisica="Superficie.Física" , 
                                        superficie_tecnificada="Superficie.Tecnificada" , 
                                        estado_proyecto="Estado.del.Proyecto..Pagado" , 
                                        fecha_pago_recursos="Fecha.pago.recursos" , 
                                        pago_uf="Monto.Bonificado.UF.bono.vigente" , 
                                        costo_total_proyecto_uf="Costo.Total.del.Proyecto.UF.bono.vigente")     


# select para elegir las variables que interesan
cnr_limpio <- cnr_nacional %>% select(cant_beneficiarios, region, provincia, comuna, estrato, tipo_obra, 
                                      superficie_fisica, superficie_tecnificada, fecha_pago_recursos, pago_uf, 
                                      costo_total_proyecto_uf)                                                  


# filter de las transferencias hacia los productores de la región de O'Higgins
cnr_ohiggins <- cnr_limpio %>% filter(region=="O´Higgins")                                            


# rename de variables para simplificar su uso
shp_nacional <- shp_nacional %>% rename(region="Region", provincia="Provincia", comuna="Comuna")     


# filter de las coordenadas de las comunas de la región de O'Higgins
shp_ohiggins <- shp_nacional %>% filter(region=="Región del Libertador Bernardo O'Higgins")           


# Manipulación de bases de datos ------------------------------------------

# eliminamos posibilidad de que los próximos valores sean expresados en notación científica
options(scipen = 999)                         


# creación tabla 1 referente a transferencias a regiones asociadas a la ley N° 18.450
cnr_limpio$pago_uf <- gsub(",", ".", cnr_limpio$pago_uf)

cnr_limpio$pago_uf <- as.numeric(as.character(cnr_limpio$pago_uf))

cnr_regional <- cnr_limpio %>% 
  group_by(region) %>% 
  summarise(pago_uf=sum(pago_uf))             


# tabla 1: creación variable que cuantifique en pesos chilenos en lugar de UF
cnr_regional <- cnr_regional %>% 
  mutate(pago_pesos=pago_uf*29138)            


# tabla 1: se ordenan los valores de mayor a menor para facilitar su comprensión
cnr_regional <- cnr_regional %>% 
  arrange(desc(pago_pesos))                   


# tabla 1: rename a variables para pulir sus nombres
cnr_regional <- cnr_regional %>% 
  rename("Región"=region, 
         "Monto de las transferencias (UF)"=pago_uf, 
         "Monto de las transferencias ($)"=pago_pesos)      


# tabla 1: añadimos fila "total"    
cnr_regional <- cnr_regional %>% 
  adorn_totals("row")                         


# tabla 1: le damos formato a los valores (separados por puntos y comas) para facilitar comprensión
cnr_regional$"Monto de las transferencias ($)" <- format(as.numeric(cnr_regional$"Monto de las transferencias ($)"), big.mark=".")
cnr_regional$"Monto de las transferencias (UF)" <-  format(as.numeric(cnr_regional$"Monto de las transferencias (UF)"), big.mark = ".", decimal.mark = ",") 


# tabla 1 lista, la guardamos
write_xlsx(cnr_regional, "C:/Users/marco/Desktop/R/articulo_rtr/base_datos/transferencias 18.450 a regiones.xlsx") 


# creación tabla 2 referente a transferencias a comunas de O'Higgins asociadas a la ley 18.450 
cnr_ohiggins$pago_uf <- gsub(",", ".", cnr_ohiggins$pago_uf)

cnr_ohiggins$pago_uf <- as.numeric(as.character(cnr_ohiggins$pago_uf))

pago_cnr_comunal <- cnr_ohiggins %>% 
  group_by(comuna) %>% 
  summarise(pago_uf = sum(pago_uf))           


# tabla 2: creación variable que cuantifique en pesos chilenos en lugar de UF
pago_cnr_comunal <- pago_cnr_comunal %>% 
  mutate(pago_pesos=pago_uf*29138)            


# tabla 2: se ordenan los valores de mayor a menor para facilitar su comprensión
pago_cnr_comunal <- pago_cnr_comunal %>% 
  arrange(desc(pago_pesos))                   


# tabla 2: rename a variables para pulir sus nombres
pago_cnr_comunal <- pago_cnr_comunal %>% 
  rename("Monto de las transferencias (UF)"=pago_uf, 
         "Monto de las transferencias ($)"=pago_pesos) 


# tabla 2: añadimos fila "total"
pago_cnr_comunal <- pago_cnr_comunal %>% 
  adorn_totals("row")                         


# tabla 2: le damos formato a los valores (separados por puntos y comas) para facilitar comprensión
pago_cnr_comunal$`Monto de las transferencias ($)` <- format(as.numeric(pago_cnr_comunal$`Monto de las transferencias ($)`), big.mark = ".")
pago_cnr_comunal$`Monto de las transferencias (UF)` <- format(as.numeric(pago_cnr_comunal$`Monto de las transferencias (UF)`), big.mark = ".", decimal.mark = ",") 


# tabla 2 lista, la guardamos
write_xlsx(pago_cnr_comunal, "C:/Users/marco/Desktop/R/articulo_rtr/base_datos/transferencias 18.450 a comunas.xlsx") 


# Mapeo de las transferencias en las comunas ------------------------------

# creación base de datos de transferencias de la ley 18.450 a las comunas de O'Higgins para mapear
pago_cnr_mapeo <- cnr_ohiggins %>% 
  group_by(comuna) %>% 
  summarise(pago_uf = sum(pago_uf))                


# unión de bases de datos generales: transferencias de la 18.450 a las comunas junto a .shp
union <- shp_ohiggins %>% 
  left_join(pago_cnr_mapeo)                        


# creación ilustración 1
ggplot(data=union) +
  geom_sf(aes(fill=pago_uf)) +
  labs(fill = "Transferencias en UF", options(scipen = 999)) +
  theme_void() +
  labs(title = "Ilustración 1
       Distribución espacial de las transferencias asociadas a la ley N° 18.450",
       subtitle = "Comunas de la región de O´Higgins, 1990-2019",
       caption = "Fuente: Elaboración propia en base a datos de solicitud de transparencia con folio N° AR002T0001380") +
  scale_fill_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5))   


# guardamos ilustración 1
ggsave("C:/Users/marco/Desktop/R/articulo_rtr/mapa_1.png") 


