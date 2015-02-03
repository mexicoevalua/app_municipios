# Preparar datos para aplicación interactiva

# Descargar datos
#====
# Diego Valle-Jones provee una excelente compilación de los datos del secretariado en su blog
# la base de datos también tiene las estimaciónes de población por municipio 
# http://crimenmexico.diegovalle.net/en/csv/
# Fuente original: http://www.incidenciadelictiva.secretariadoejecutivo.gob.mx/

# Utilizar datos desde el archivo gzip para consumir menos memoria
gunzip("incidenciaDelictiva/data/fuero-comun-municipios.csv.gz")
data  <- read.csv("incidenciaDelictiva/data/fuero-comun-municipios.csv", as.is=T)


# Años
table(data$year) # 2011, 2012, 2013, 2014

# Total crimenes
sum(data$count, na.rm=T) #5,473,225

# Agregar datos para crímenes en los siguientes grupos: homicidios, secuestros, robos, otros delitos

# Agregar DELITOS PATRIMONIALES, DELITOS SEXUALES, LESIONES, OTROS DELITOS, en una sola categoría llamada OTROS DELITOS
other  <- c("DELITOS PATRIMONIALES","DELITOS SEXUALES","LESIONES", "OTROS DELITOS")
data$group  <- data$crime
for(x in other){
  data$group  <- gsub(x,"OTROS DELITOS", data$group)
}

# Agregar homicidios dolosos como grupo
data$group[data$group =="HOMICIDIOS" & data$type =="DOLOSOS"]  <- "HOMICIDIOS DOLOSOS"
data$group[data$group =="HOMICIDIOS" & data$type =="CULPOSOS"]  <- "HOMICIDIOS CULPOSOS"
table(data$group)

# Agregar extorsion como grupo
data$group[data$type =="EXTORSION"]  <- "EXTORSION"
table(data$group)

# Agregar el robo de vehiculos como grupo
data$group[data$subtype =="DE VEHICULOS"]  <- "ROBO A VEHICULOS"

# Agregar el robo con violencia como grupo
data$group[data$group =="ROBOS" & data$type =="CON VIOLENCIA"]  <- "ROBO CON VIOLENCIA"
table(data$group)

# Cambiar nombre secuestro
data$group  <- gsub("PRIV. DE LA LIBERTAD \\(SECUESTRO\\)","SECUESTRO",data$group)
table(data$group)
format(table(data$group)  / length(data$group)*100, digits=2)

# Mantener sólo homicidios culposos, extorsión, robo a vehículos, robo con violencia y secuestro.
temp  <- subset(data, data$group == "EXTORSION" | data$group == "HOMICIDIOS DOLOSOS"  | data$group == "ROBO A VEHICULOS" | 
                data$group == "EXTORSION" | data$group == "ROBO CON VIOLENCIA" | data$group == "SECUESTRO")

# Se excluirán de los cálculos las observaciones con NA.
names(data)
require(plyr)

ave <- ddply(temp, c("state_code","mun_code","year","group"), summarize,
             averiguaciones = sum(count,na.rm=T))

# La tabla mostrará sólo un numero selecto de averiguaciones previas por lo que el número no coincidirá con el total
format(sum(data$count,na.rm=T),big.mark=",") # Numero total de averiguaciones 5,369,157
format(sum(ave$averiguaciones,na.rm=T), big.mark=",") # Numero total de averiguaciones 1,284,105

# Estimaciones de poblacion por municipio
population <- ddply(data, c("state_code","mun_code","year"), summarize,
                    population = max(population))

# Tasa de averiguaciones previas por cada 100 mil habitantes para delitos selectos
head(ave)
head(population)
ave  <- merge(ave, population, by= c("state_code","mun_code","year"))
ave$rate  <- (ave$averiguaciones*100000 / ave$population)

# Cambiar formato % en rate para reducir a dos decimales
ave$rate  <- as.numeric(format(round(as.numeric(ave$rate),2), nsmall = 2))

# Agregar abreviaturas de los estados
getwd()
temp  <- read.csv("app_municipios/data/state_names.csv")
tail(temp)
ave  <- merge(ave,temp)

# Total de delitos del fuero común:
head(ave)
aveTot <- ddply(data, c("state_code","mun_code","year"), summarize,
                averiguaciones = sum(count,na.rm=T))
head(aveTot)

# El tasa del total de averiguaciones previas sí contiene todos los delitos
format(sum(data$count,na.rm=T),big.mark=",") # Numero total de averiguaciones 5,369,157
format(sum(aveTot$averiguaciones,na.rm=T), big.mark=",") # Numero total de averiguaciones 5,369,157

# Calcular tasa del total de delitos del fuero común
aveTot  <- merge(aveTot, population, by= c("state_code","mun_code","year"))
aveTot$rate  <- (aveTot$averiguaciones*100000 / aveTot$population)
summary(aveTot$rate)

# Cambiar formato % en rate para reducir a dos decimales
aveTot$rate  <- as.numeric(format(round(as.numeric(aveTot$rate),2), nsmall = 2))

# Agregar abreviaturas de los estados
aveTot  <- merge(aveTot,temp)

# Agregar el total de delitos al archivo de grupos
aveTot$group  <- "Total"
head(ave)
head(aveTot)
aveTot  <- aveTot[,c(1,2,3,9,4:8)]
# Verificar que los nombres de las columnas coinciden
names(ave) == names(aveTot) 
ave  <- rbind(ave, aveTot)
names(ave)

# Cambiar los IDs para que coincidan con el dbf y poder agregar los nombres de los municipios
ave$mun_code  <- sprintf("%03d", ave$mun_code)
ave$state_code  <- sprintf("%02d", ave$state_code)
head(ave$mun_code)

# Resultado
table(ave$group)

#====

# Preparar datos para unir con shapefile
#====
#install.packages("reshape")
require(reshape2)

# Agregar id unico
ave$id  <- paste(ave$state_code,ave$mun_code,sep="")
length(unique(ave$id))


####### Agregar nombres de municipios
#Descargar shapefile
#download.file("http://mapserver.inegi.org.mx/MGN/mgm2010v5_0a.zip", "shapefiles/municipios.zip")
#unzip("shapefiles/municipios.zip",exdir="shapefiles", overwrite=T)

require(foreign)
shp  <- read.dbf("mapaHomicidios/shapefiles/Municipios_2010_5A.dbf")
shp$id  <- paste(shp$CVE_ENT,shp$CVE_MUN,sep="")
names(shp)
shp$NOM_MUN <- iconv(shp$NOM_MUN, "windows-1252", "utf-8")
tail(shp)
dataTable  <- merge(ave, shp, by="id")
head(dataTable)
names(dataTable)
dataTable  <- dataTable[,c(10,13,4,5,6,8)]
names(dataTable)  <- c("Estado", "Municipio", "Año","Crimen", "Averiguaciones", "Tasa")

# Cambiar nombres de los delitos
table(dataTable$Crimen)
dataTable$Crimen  <- gsub("Total","Total averiguaciones", dataTable$Crimen) 
dataTable$Crimen  <- gsub("ROBO A VEHICULOS","Robo a vehículos", dataTable$Crimen) 
dataTable$Crimen  <- gsub("ROBO CON VIOLENCIA","Robo con violencia", dataTable$Crimen) 
dataTable$Crimen  <- gsub("SECUESTRO","Secuestro", dataTable$Crimen)
dataTable$Crimen  <- gsub("EXTORSION","Extorsión", dataTable$Crimen) 
dataTable$Crimen  <- gsub("HOMICIDIOS DOLOSOS","Homicidio doloso", dataTable$Crimen) 
unique(dataTable$Crimen)

# Agregar rankings por delito y fecha
dataTable  <- ddply(dataTable, c("Año","Crimen"), transform, Lugar = rank(-Tasa, ties.method = "min"))
head(dataTable)

# Revisar maximos y minimos con rankings
ddply(dataTable, c("Crimen","Año"), summarise,
      min = min(Tasa),
      max = max(Tasa))

# Separar miles con comas
test  <- dataTable  
dataTable$Averiguaciones  <- format(dataTable$Averiguaciones, big.mark=",")
dataTable$Tasa  <- format(dataTable$Tasa, big.mark=",")
dataTable$Lugar  <- format(dataTable$Lugar, big.mark=",")

# Exportar como datos para la aplicacion interactiva

write.csv(dataTable,"app_municipios/data/data_table.csv",row.names=F, fileEncoding="utf8")
# Names for windows computers:
# names(dataTable)  <- c("Estado", "Municipio", "Year","Crimen", "Averiguaciones", "Tasa","Lugar")

# Final del script
#########