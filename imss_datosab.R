###Script asegurados IMSS construcción Sonora

##Borrar datos del entorno
rm(list=ls())

#Establecer límite de memoria
memory.limit(size = 200000)
options(timeout=1000)

#Directorio de trabajo
setwd("D:/")
dir.create("imss")


#Librerías
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, extrafont, 
               kableExtra)

#URL de descarga de datos de asegurados
url<-"http://datos.imss.gob.mx/sites/default/files/asg-"

#Función de descarga====
descarga <- function(index_archivo) {
  download.file(glue::glue("{url}{index_archivo}-12-31.csv"),
                destfile=glue::glue("imss/asg-{index_archivo}-12-31.csv"))
  
}

#Ejecución de la función=====
walk(2015:2020,  ~ descarga(.x))



#Listado de archivos descargados=====
archivos <- list.files(path = "imss", pattern = "asg")


##Url de claves municipios IMSS
urlc<-"http://datos.imss.gob.mx/sites/default/files/diccionario_de_datos_1.xlsx"

cat<-openxlsx::read.xlsx(urlc,sheet=4)%>%
  janitor::row_to_names(1)%>%
  mutate(cve_entidad=as.numeric(cve_entidad))%>%
  filter(cve_entidad==26)%>%
  rename(nom_mun=5)%>%
  select(cve_municipio, nom_mun)


##Lectura y limpieza====
consolidada<-
  #leer y agrupar los datos
  purrr::map(archivos,
                 ~ read.csv(glue::glue("imss/{.x}"),
                            sep="|",fileEncoding="latin1"
                 ) %>%
                  #Nombre de las variables en minúsculas
                   janitor::clean_names()%>%
                  #Seleccionar variables requeridas
                  select(cve_entidad, 
                         cve_municipio,
                         sector_economico_1,
                         ta))%>%
  #Traer nombre de archivo y solo dejar el año
  set_names(archivos)%>%
  bind_rows(.id="year")%>%
  mutate(year=as.numeric(str_sub(year,5,8)))%>%
###Filtros de entidad y sectores requeridos
#Filtrar entidad requerida y sector económico (construcción)      
filter(cve_entidad==26 &
         sector_economico_1==4)%>%
  #Pegar nombres de municipios
  left_join(cat)%>%
  #Agrupar para sacar totales
  group_by(year,nom_mun)%>%
  summarise(ta=sum(ta))%>%
  mutate(ta=replace_na(ta,0))%>%
  ungroup()%>%
  group_by(year)%>%
  mutate(total=sum(ta),
         #Participación en el total estatal
         pct=ta/total*100)%>%
  ungroup()%>%
  #Filtro de municipio
  filter(nom_mun=="Hermosillo")%>%
  #Variación porcentual anual
  mutate(crec=(ta/lag(ta,
                      1)-1)*100)



#Preparar tabla de resultados


consolidada%>%
  select(!nom_mun)%>%
  mutate(ta=format(ta, big.mark = ","),
         total=format(total, big.mark = ","),
         pct=round(pct,1),
         crec=round(crec,1))%>%
  kable(caption=text_spec("Hermosillo. Trabajadores asegurados ante el IMSS en la industria de la construcción
Registro de diciembre de cada año
                          ",
                          bold=T, color="black",font_size = 30),
        format="html",
        align = "c",
        col.names = c("Año",
                      "Asegurados Hermosillo",
                      "Asegurados en la entidad",
                      "Participación del municipio\nen la entidad",
                      "Crecimiento anual (%"))%>%
  kable_styling(full_width = F, font_size = 20,
                html_font = "Century Gothic")%>%
  row_spec(0, bold = F, color = "black", background = "#feb24c")%>%
  footnote(general = "Elaborado por CANADEVI Nacional. Gerencia de Fondos de Vivienda. 
Coordinación de Indicadores de Vivienda con datos abiertos del IMSS.",
           general_title = "
Fuente: ")%>%
  #Salvar
  as_image(file="imsshermosillo.png")
