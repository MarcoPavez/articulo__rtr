
# Librerías ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

# Importación de bases de datos -------------------------------------------

cnr_nacional <- read_xlsx("C:/Users/marco/Desktop/R/articulo_rtr/base_datos/AR002T0001380.xlsx")

shp_nacional <- read_sf("C:/Users/marco/Desktop/R/articulo_rtr/shp/comunas.shp")

# Limpieza de bases de datos ----------------------------------------------

cnr_nacional <- cnr_nacional %>% rename(año="Año de llamado" , codigo="Código del Proyecto", nombre="Nombre del Proyecto" , beneficiario="Nombre del Beneficiario" , 
                                        cant_beneficiarios="Cantidad de beneficiarios" , region="Región" , provincia="Provincia" , comuna="Comuna" , consultor="Consultor" , 
                                        estrato="Estrato" , origen_fondos="Origen de los Fondos" , tipo_obra="Tipo de Obra" , superficie_fisica="Superficie Física" , 
                                        superficie_tecnificada="Superficie Tecnificada" , estado_proyecto="Estado del Proyecto: Pagado" , fecha_pago_recursos="Fecha pago recursos" , 
                                        pago_uf="Monto Bonificado UF bono vigente" , costo_total_proyecto_uf= "Costo Total del Proyecto UF bono vigente")











