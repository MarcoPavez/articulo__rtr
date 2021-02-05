
# Librerías ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(writexl)
library(sf)
library(viridis)

# Importación de bases de datos -------------------------------------------

cnr_nacional <- read_xlsx("C:/Users/marco/Desktop/R/articulo_rtr/base_datos/AR002T0001380.xlsx")      # correspondiente a las transferencias por concepto de la ley 18.450


shp_nacional <- read_sf("C:/Users/marco/Desktop/R/articulo_rtr/shp/comunas.shp")                      # archivo .shp que contiene las coordenadas de las comunas de la región de O'Higgins para su mapeo


# Limpieza de bases de datos ----------------------------------------------

cnr_nacional <- cnr_nacional %>% rename(año="Año de llamado" , 
                                        codigo="Código del Proyecto", 
                                        nombre="Nombre del Proyecto" , 
                                        beneficiario="Nombre del Beneficiario" , 
                                        cant_beneficiarios="Cantidad de beneficiarios" , 
                                        region="Región" , 
                                        provincia="Provincia" , comuna="Comuna" , 
                                        consultor="Consultor" , 
                                        estrato="Estrato" , origen_fondos="Origen de los Fondos" , 
                                        tipo_obra="Tipo de Obra" , 
                                        superficie_fisica="Superficie Física" , 
                                        superficie_tecnificada="Superficie Tecnificada" , 
                                        estado_proyecto="Estado del Proyecto: Pagado" , 
                                        fecha_pago_recursos="Fecha pago recursos" , 
                                        pago_uf="Monto Bonificado UF bono vigente" , 
                                        costo_total_proyecto_uf="Costo Total del Proyecto UF bono vigente")     # rename de variables para simplificar su uso


cnr_limpio <- cnr_nacional %>% select(año, cant_beneficiarios, region, provincia, comuna, estrato, tipo_obra, 
                                      superficie_fisica, superficie_tecnificada, fecha_pago_recursos, pago_uf, 
                                      costo_total_proyecto_uf)                                                  # select para elegir las variables que interesan


cnr_ohiggins <- cnr_limpio %>% filter(region=="O´Higgins")                                            # filter de las transferencias hacia los productores de la región de O'Higgins


shp_nacional <- shp_nacional %>% rename(region="Region", provincia="Provincia", comuna="Comuna")      # rename de variables para simplificar su uso


shp_ohiggins <- shp_nacional %>% filter(region=="Región del Libertador Bernardo O'Higgins")           # filter de las coordenadas de las comunas de la región de O'Higgins


# Manipulación de bases de datos ------------------------------------------

cnr_regional <- cnr_limpio %>% group_by(region) %>% summarise(pago_uf=sum(pago_uf))             # creación tabla 1 referente a transferencias a regiones asociadas a la ley N° 18.450

cnr_regional <- cnr_regional %>% mutate(pago_pesos=pago_uf*29138)                               # tabla 1: creación variable que cuantifique en pesos chilenos en lugar de UF

cnr_regional <- cnr_regional %>% arrange(desc(pago_pesos))                                      # tabla 1: se ordenan los valores del mayor al menor para facilitar su comprensión

cnr_regional <- cnr_regional %>% rename("Región"=region, 
                                        "Monto de las transferencias (UF)"=pago_uf, 
                                        "Monto de las transferencias ($)"=pago_pesos)           # tabla 1: rename a variables para pulir sus nombres

cnr_regional$"Monto de las transferencias ($)" <- format(as.numeric(cnr_regional$"Monto de las transferencias ($)"), big.mark=".")
cnr_regional$"Monto de las transferencias (UF)" <-  format(as.numeric(cnr_regional$"Monto de las transferencias (UF)"), big.mark = ".", decimal.mark = ",") # tabla 1: le damos formato a los valores 
                                                                                                                                                            # (separados por puntos y comas) para facilitar comprensión

write_xlsx(cnr_regional, "C:/Users/marco/Desktop/R/articulo_rtr/base_datos/transferencias 18.450 a regiones.xlsx") # tabla 1 lista, la guardamos



pago_cnr_comunal <- cnr_ohiggins %>% 
  group_by(comuna) %>% 
  summarise(pago_uf = sum(pago_uf))                         # monto total de las transferencias de la ley 18.450 según comunas de la región de O'Higgins

union <- shp_ohiggins %>% left_join(pago_cnr_comunal)



ggplot(data = union) +
  geom_sf(aes(fill = pago_uf)) +
  labs(fill = "Transferencias en UF", options(scipen = 4)) +
  theme_void() +
  labs(title = "Ilustración 1
       Distribución espacial de las transferencias asociadas a la ley N° 18.450",
       subtitle = "Comunas de la región de O´Higgins, 1990-2019",
       caption = "Fuente: Elaboración propia en base a datos de solicitud de transparencia con folio N° AR002T0001380") +
  scale_fill_viridis_c() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5))



