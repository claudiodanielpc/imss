  ##Trabajadores asegurados ante el IMSS
  
  
  ##Se cargan las librerías necesarias
  
  
  if(!require('pacman')) install.packages('pacman')
  pacman::p_load(tidyverse, gganimate,
                 gifski, dplyr,lubridate, scales,ggimage)
  
  
  ##Se importa la base de datos
  ##URL del archivo
  urlimss<-"https://raw.githubusercontent.com/claudiodanielpc/imss/master/archivos/serietiempoimss.csv"
  
  imss<-read.csv(urlimss,
               encoding="UTF-8",header=TRUE,check.names=FALSE)
  
  
 
  ##se crean variables necesarias
  
  imss<-imss%>%
    ##Variable fecha
    mutate(fecha=ymd(paste(año, mes, 15,sep= ' ')))%>%
    ##Variaciones anuales
    group_by(tipo)%>%
    mutate(var=(trab/lag(trab,12)-1)*100)%>%
    ungroup()%>%
    ##Quitar NAs
    filter(!is.na(var))%>%
    ##Etiquetas para los datos en el gráfico
    mutate(etiqueta=paste(format(round(var,1),nsmall=0,big.mark = ",")))
  
  
  
###Se crea la paleta de colores para la gráfica
  colores <- c("#feb24c","#bdbdbd")

  ##Se crea el gráfico plano
  p<-ggplot(imss, aes(fecha, var)) +
    geom_line(aes(color = tipo),size=3)+
    #Mostrar los valores de fechas específicas
    geom_text(aes(label=ifelse((fecha =='2020-04-15' | fecha=="2009-09-15" & tipo=="construcción"),
                               etiqueta,
                               ifelse((fecha=="2020-04-15" & tipo=="total"),etiqueta,""))),color="red",
              hjust=0.5,vjust=2,size=6,fontface="bold")+
    scale_color_manual("Tipo",values = colores,labels=c("Construcción","Total"))+
    theme_grey() +
    scale_x_date(date_breaks="3 months",date_labels = "%b %Y")+
    labs(
      title = "México. Trabajadores asegurados ante el IMSS: totales y construcción",
      subtitle = "(Variación porcentual respecto al mismo mes del año anterior)",
      y = "Var. %",
      x="Fecha",
      caption = "Fuente: @claudiodanielpc con información del IMSS."
    )+
    theme(plot.title = element_text(hjust = 0, size=20,face="bold"),
          plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
          plot.caption = element_text(hjust = 0,size=10),
          legend.position = "right",
          axis.text.x = element_text(angle = 90))
  
  ##Mostrar gráfico
  p
  
  ##Salvar gráfica
  ggsave("grafhist.png", height=10, width=20, units='in', dpi=300)
  
  ##Adicional: Si se quiere crear animación del gráfico con gganimate, se corre el código de abajo
  
  #animp<-p+
#transition_reveal(along=imss$fecha)
  
  
  
  
#animp %>% animate(fps = 15,
 #                   nframes = 100,
  #duration=7,
  #end_pause = 25,
  #width = 800, height = 800)
  
  #anim_save("empleoconst.gif")
  
  