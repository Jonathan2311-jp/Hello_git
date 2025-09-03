##############################################################

###############################################################
library(dplyr)
library(readxl)
data_esta=read_excel("C:/Users/Admin/Downloads/Esta/Datos trabajo descriptiva.xlsx")
View(data_esta)
#########################################################3
# Cambiar nombre a variables
attach(data_esta)
View(data_esta)
names(data_esta)
str(data_esta)
#############################################################

# Integrante_del_hogar 
data_esta=rename(data_esta, Integrante_del_hogar = f2_b, Sexo=f2_c,
                 Edad=f2_d,Nivel_educacional=f2_e, Categoria_ocupacional=f2_f,
                 Acuerdo =f2_g,Monto_total=f2_h,Horas_semanales=f2_i)

###############################################################

# Convertir en cualitativas (nominales u ordinales) o cuantitativas (discretas o continuas)
#  Cambiar los nombres de las categorías que aparecen codificadas numéricamente y nombrarlas como aparece en 
# la encuesta.

# Integrante_del_hogar- Nominal
data_esta$Integrante_del_hogar=factor(data_esta$Integrante_del_hogar,
                                         levels=c("1","2"),
                                         labels=c("Si","No"))
table(data_esta$Integrante_del_hogar)

###################################################################
# Sexo - Nominal

data_esta$Sexo=factor(data_esta$Sexo, levels=c("1","2"),
                      labels=c("Hombre","Mujer"))
table(data_esta$Sexo)

##################################################################
# Edad - Continua
#################################################################
# Nivel_educacional - Ordinal

data_esta$Nivel_educacional=factor(data_esta$Nivel_educacional, 
                                   levels=c("1","2","3","4","88","99"),
                                    labels=c("Ninguno","Primaria","Secundaria",
                                             "Superior","No sabe","No responde"),
                                   ordered = TRUE)
table(data_esta$Nivel_educacional)
##################################################################
# Categoria Ocupacional - Ordinal

data_esta$Categoria_ocupacional=factor(data_esta$Categoria_ocupacional, 
                                   levels=c("1","2","3","4","88","99"),
                                   labels=c("Trabajador_Asalariado","Socios_Trabajadores","Familiar_Sin_Pago",
                                            "Aprendices","No sabe","No responde"),
                                   ordered = TRUE)
table(data_esta$Categoria_ocupacional)
###################################################################
# Acuerdo - Ordinal 

data_esta$Acuerdo=factor(data_esta$Acuerdo, 
                                       levels=c("1","2","3","88","99"),
                                       labels=c("Acuerdo_Palabra","Escrito","Escrito_Firmado",
                                                "No sabe","No responde"),
                                       ordered = TRUE)
table(data_esta$Acuerdo)
###################################################################ç
# Monto_total
# Tener en cuenta esos valore para saber la cantidad

data_esta$monto_general=ifelse(data_esta$Monto_total == 88, "No se sabe",
                                  ifelse(data_esta$Monto_total == 99, "No responde",
                                         as.character(data_esta$Monto_total)))
table(data_esta$monto_general)
data_esta$monto_general=factor(data_esta$monto_general)
summary(data_esta$monto_general)

# Convertir en NA el valor 88 y 99
data_esta$Monto_total[data_esta$Monto_total==88]=NA
data_esta$Monto_total[data_esta$Monto_total==99]=NA
summary(data_esta$Monto_total)

#######################################################################
# Horas semanales

data_esta=data_esta %>%
  mutate(Horas_semanales_gen = case_when(
    Horas_semanales == 86 ~ "Trabaja de manera ocasional",
    Horas_semanales == 87 ~ "No tiene horario regular",
    Horas_semanales == 88 ~ "No sabe",
    Horas_semanales == 99 ~ "No responde",
    TRUE ~ as.character(Horas_semanales)
  ))
table(data_esta$Horas_semanales_gen)
data_esta$Horas_semanales_gen=factor(data_esta$Horas_semanales_gen)


# Convertir en NA el valor 86,87,88 , y 99
data_esta$Horas_semanales[data_esta$Horas_semanales %in% c(86, 87, 88,99)]=NA
summary(data_esta$Horas_semanales)

####################################################################33

########### Summary ##############################################

##################################################################

resumen_final=summary(data_esta);resumen_final

#################################################################
# Tabla de frecuencia- Agrupada
library(fdth)

n=length(data_esta$Edad) 
datos=sort(data_esta$Edad)
max_dato=max(data_esta$Edad) 
min_dato=min(data_esta$Edad)
R=max_dato - min_dato
k=1 + (3.32*log10(n))
A=R/round(k)
A=ceiling(A)

Tabla_frec=fdt(data_esta$Edad, start = min(data_esta$Edad), end = (min(data_esta$Edad)+round(k)*A), h=A,  breaks = "Sturges"); Tabla_frec

# Extraer la tabla interna del objeto fdt
Tabla_frec_df=as.data.frame(Tabla_frec$table)
Tabla_frec_mod=rename(Tabla_frec_df,Limites="Class limits", Frec_abso="f",
                  Frec_rel="rf",Frec_Rel_Por="rf(%)",
                  Frec_Acum="cf",Frec_Acum_Rel="cf(%)");Tabla_frec_mod

Tabla_frec_mod=Tabla_frec_mod %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Crear la fila de totales
fila_total = data.frame(
  Limites = "Total",
  Frec_abso = sum(Tabla_frec_mod$Frec_abso),
  Frec_rel = 1,
  Frec_Rel_Por = 100,                         
  Frec_Acum =" ",
  Frec_Acum_Rel = " ",
  check.names = FALSE
)

tabla_completa=rbind(Tabla_frec_mod, fila_total)
tabla_completa

###################################################################33

library(gridExtra)
library(grid)

# Quitar rownames y redondear
tabla_limpia = tabla_completa
rownames(tabla_limpia) = NULL
tabla_limpia = data.frame(lapply(tabla_limpia, function(x) 
  if(is.numeric(x)) round(x, 4) else x))

# Mostrar en ventana de plots con título
grid.newpage()
grid.draw(tableGrob(tabla_limpia, 
                    rows = NULL,        # elimina numeración de filas
                    theme = ttheme_default(
                      core = list(fg_params = list(cex = 0.8)),   # tamaño texto celdas
                      colhead = list(fg_params = list(cex = 0.9, fontface = "bold")) # encabezados
                    )))

# Agregar título arriba
grid.text("Tabla de Frecuencias", y = unit(0.95, "npc"), gp = gpar(fontsize = 14, fontface = "bold"))

############################################################3
# Medidas - Media, Mediana  y desviavione standar - Agrupada
# Clase
limites=as.character(Tabla_frec_df$`Class limits`)
mi=numeric(length(limites)) 
for (i in 1:10) {
  limite_i=gsub("\\[|\\)|\\]", "", limites[i])  
  limite_inf=as.numeric(strsplit(limite_i, ",")[[1]][1])  
  limite_sub=as.numeric(strsplit(limite_i, ",")[[1]][2])
  mi[i]=((limite_inf+limite_sub)/2)
}

media_agrupada=(sum(mi*(Tabla_frec_df$f)))/n;media_agrupada
summary(data_esta$Edad)

# Media nos da agrupado 40.72 y no agrupada 40.36 años

#######################################################ç
# Mediana

n=sum(Tabla_frec_df$f)       
pos_mediana = n * 0.5          

for (i in 1:length(Tabla_frec_df$cf)) {
  if (Tabla_frec_df$cf[i] >= pos_mediana) {
    fila_mediana = i
    fila_anterior = i - 1
    
    # Intervalo de la clase mediana
    limite_i = as.character(Tabla_frec_df$`Class limits`[fila_mediana])
    limites  = gsub("\\[|\\)|\\]", "", limite_i)
    limites  = strsplit(limites, ",")[[1]]
    limite_inf = as.numeric(limites[1])
    limite_sup = as.numeric(limites[2])

    fi_abs = Tabla_frec_df$f[fila_mediana]
    Fi_1   = ifelse(fila_anterior == 0, 0, Tabla_frec_df$cf[fila_anterior])
    ampli  = limite_sup - limite_inf  
    Mediana = limite_inf + ((pos_mediana - Fi_1) / fi_abs) * ampli
    
    print(paste("Fila de la mediana:", fila_mediana))
    print(paste("Límite inferior:", limite_inf))
    print(paste("Frecuencia acumulada anterior (Fi-1):", Fi_1))
    print(paste("Frecuencia de la clase (fi):", fi_abs))
    print(paste("Amplitud:", ampli))
    print(paste("Valor de la Mediana:", round(Mediana, 2)))
    break
  }
}


######################################################
# Desviaciòn estandar

media_agrupada=(sum(mi*(Tabla_frec_df$f)))/n;media_agrupada
cuadrados=(mi-media_agrupada)^2;cuadrados
fi_cu=Tabla_frec_df$f*cuadrados;fi_cu
# Vaianza pobla
varianza_p=(sum(fi_cu))/(n-1)
desvia_p=sqrt(varianza)
cat("Poblacional", varianza_p , desvia_p)
# Varianza Media
varianza_m=(sum(fi_cu))/n;varianza_m
desvia_m=sqrt(varianza);desvia_m
cat("Poblacional", varianza_m , desvia_m)

##################################################

cat("Media:" ,media_agrupada,"Mediana:",Mediana,"Desviaciòn:",desvia_p)

# Interpretaciòn

# El promedio de edad de las personas que trabajan por cuenta 
# propia o en microemprendimientos es cercano a los 41 años. 
# La mediana muestra que el 50% de los trabajadores tiene menos de
# 39 años, mientras que la otra mitad supera esa edad. En cuanto a 
# la dispersión, las edades presentan una desviación estándar de 
# aproximadamente 14 años, lo que refleja una alta variabilidad: en este 
# sector participan tanto jóvenes en sus 20s como personas que 
# superan los 60 años.

###################################################################








