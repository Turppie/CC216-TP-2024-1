# instalar bibliotecas
install.packages("dplyr")
library(dplyr)

install.packages("knitr")
library(knitr)

install.packages("ggplot2")
library(ggplot2)

# carga de datos
data <- read.csv("hotel_bookings.csv", header= TRUE, stringsAsFactors = FALSE)

# inspección de datos (i)
sin_valor <- function(x) {
  sum = 0
  for(i in 1:ncol(x)) {
    print(paste("En la columna", colnames(x[i]), "total de valores NA:", sum(is.na(x[, i]))))
  }
}

sin_valor(data)

# limpieza de datos
Data.limpia <- na.omit(data)

data <- Data.limpia

# inspección de datos (ii)
sin_valor(data)

str(data)

# Utilizamos head( ) y tail( ) para ver parte de la data
head(data)

tail(data)

# Pregunta N°1: ¿Cuántas reservas se realizan por tipo de hotel? o ¿Qué tipo de hotel prefiere la gente?
table_hotel <- data %>% count(hotel)
table_hotel %>%
  ggplot(aes(x = hotel, y = n))+
    geom_bar(stat = "identity", fill = c("red","blue"))+
    xlab("Tipo de Hotel")+
    ylab("Número de Reservas")+
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), colour = "white")+
    labs(title = "Cantidad de reservas realizadas según el tipo de hotel")
table_hotel

# En el caso se quiera saber si las personas por su país de origen eligen algún tipo de hotel:
table_hotel_country <- table(data$country, data$hotel)

head(table_hotel_country)

matplot(table_hotel_country, type = "l", lty = 1, col = c("blue", "red"),
        main = "Tendencia de Frecuencia de Hoteles por País",
        xlab = "País", ylab = "Frecuencia", xaxt = "n", cex = 1.5)

axis(1, at = 1:nrow(table_hotel_country), labels = rownames(table_hotel_country), cex.axis = 0.8)

legend("topright", legend = colnames(table_hotel_country), lty = 1, col = c("blue", "red"), cex = 0.8)

# Pregunta N°2: ¿Está aumentando la demanda con el tiempo?
aumento_demanda <- function(hotel){
    hotel_data <- hotel %>%
        mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
        group_by(arrival_date_year, hotel, arrival_date_month) %>%
        summarise(total_bookings = n())

    # Graficar con etiquetas de meses rotadas
    ggplot(hotel_data, aes(x = as.factor(arrival_date_month), y = total_bookings, color = hotel, group = interaction(hotel, arrival_date_year))) +
        geom_line() +
        labs(title = "Cantidad de reservas por mes y hotel en cada año",
             x = "Mes",
             y = "Total de reservas",
             color = "Hotel") +
        theme_minimal() +
        facet_wrap(~ arrival_date_year, scales = "free_y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x
}


aumento_demanda(data)

# Pregunta N°3: ¿Cuándo se producen las temporadas de reservas: alta, media y baja?
# Considerando para hallar la menor demanda de reservas se encontrará por mes por año
reservas_por_mes_por_anio <- table(data$arrival_date_month, data$arrival_date_year)
reservas_por_mes_por_anio

# Data frame para calcular y graficar
df_meses_anio <- as.data.frame(reservas_por_mes_por_anio)
names(df_meses_anio) <- c("Mes", "Anio", "Cantidad")

# Ordenar por meses
orden_meses <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
df_meses_anio$Mes <- factor(df_meses_anio$Mes, levels = orden_meses)

# Excluir los 0
df_meses_anio_sin_0 <- df_meses_anio %>% filter(Cantidad > 0)

# Función para obtener el mes con mayor, menor e intermedia cantidad de reservas

obtener_meses_anios_destacados <- function(df_meses_por_anio) {

  # Mes con mayor cantidad de reservas como temporada alta
  max_row <- df_meses_por_anio[which.max(df_meses_por_anio$Cantidad), ]
  mes_max <- max_row$Mes
  anio_max <- max_row$Anio
  cantidad_max <- max_row$Cantidad

  # Mes con menor cantidad de reservas como temporada baja
  min_row <- df_meses_por_anio[which.min(df_meses_por_anio$Cantidad), ]
  mes_min <- min_row$Mes
  anio_min <- min_row$Anio
  cantidad_min <- min_row$Cantidad

  # Calculo de la mediana de cant. reservas para la temporada media
  mediana_cantidades <- median(df_meses_por_anio$Cantidad)

  # Encontrar el mes más cercano a la mediana
  mediana_row <- df_meses_por_anio[which.min(abs(df_meses_por_anio$Cantidad - mediana_cantidades)), ]
  mes_intermedio <- mediana_row$Mes
  anio_intermedio <- mediana_row$Anio
  cantidad_intermedio <- mediana_row$Cantidad


  return(data.frame(Mes = c(mes_max, mes_intermedio, mes_min),
                    Anio = c(anio_max, anio_intermedio, anio_min),
                    Cantidad = c(cantidad_max, cantidad_intermedio, cantidad_min),
                    stringsAsFactors = FALSE))
}

# A partir de la función creamos vector para mostrar los meses con sus cantidades, representando temporada de reservas alta, media y baja
meses_anios_destacados <- obtener_meses_anios_destacados(df_meses_anio_sin_0)

# Mostrar los resultados
print(meses_anios_destacados)

# Grafica
df_meses_anio_destacados <- data.frame(Mes = meses_anios_destacados$Mes,
                 Anio = meses_anios_destacados$Anio,
                 Cantidad = meses_anios_destacados$Cantidad)

ggplot(df_meses_anio_destacados, aes(x = Mes, y = Cantidad, fill = Mes)) +
  geom_bar(stat = "identity") +
  labs(title = "Demanda de reservas por mes y por año",
       x = "Mes",
       y = "Demanda de reservas") +
  theme_minimal() +
  facet_wrap(~Anio) +
  theme(panel.spacing = unit(0, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Pregunta N°4: ¿Cuándo es menor la demanda de reservas?
# Considerando para hallar la menor demanda de reservas se encontrará por mes por año
reservas_por_mes_por_anio <- table(data$arrival_date_month, data$arrival_date_year)
reservas_por_mes_por_anio

# Data frame para calcular y graficar
df_meses_anio <- as.data.frame(reservas_por_mes_por_anio)
names(df_meses_anio) <- c("Mes", "Anio", "Cantidad")

# Ordenar por meses
orden_meses <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
df_meses_anio$Mes <- factor(df_meses_anio$Mes, levels = orden_meses)

# Dataframe que excluye los 0
df_meses_anio_sin_0 <- df_meses_anio %>% filter(Cantidad > 0)

# Encontrar la fila con la cantidad mínima
min_row <- df_meses_anio_sin_0[which.min(df_meses_anio_sin_0$Cantidad), ]

# Mes, año y cantidad
mes_menor_demanda <- min_row$Mes
anio_menor_demanda <- min_row$Anio
cantidad_menor_demanda <- min_row$Cantidad

# Proyectar los resultados
print(paste("La demanda es menor en", mes_menor_demanda, "de", anio_menor_demanda, "con una cantidad de", cantidad_menor_demanda, "reservas"))

# Grafica
ggplot(df_meses_anio, aes(x = Mes, y = Cantidad, fill = Mes)) +
  geom_bar(stat = "identity") +
  labs(title = "Demanda de reservas por mes y por año",
       x = "Mes",
       y = "Demanda de reservas") +
  theme_minimal() +
  facet_wrap(~Anio) +
  theme(panel.spacing = unit(0, "lines"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Pregunta N°5: ¿Cuántas reservas incluyen niños y/o bebes?
reservas_infantes <- data %>% select(children, babies)
reservas_infantes$niños_o_bebes <- ifelse(reservas_infantes$children > 0 | reservas_infantes$babies >0, "Si", "No")
table(reservas_infantes$niños_o_bebes)
reservas_infantes <- reservas_infantes %>% count(niños_o_bebes)

ggplot(reservas_infantes,aes(x = "", y = n, fill = niños_o_bebes))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start = 0)+
  scale_fill_manual(values = c("#F5B041", "#82E0AA"),labels = c("No", "Si"),name = "Tiene niños y/o bebes")+
  geom_text(aes(label = paste0(round(n/sum(n)*100, 2), "%")), position = position_stack(vjust = 0.5),colours = "white", size = 5) +
  labs(title = "Porcentaje de reservas que tienen niños y/o bebes")

# Pregunta N°6: ¿Es importante contar con espacios de estacionamiento?
filtered_parking_data<-data%>%filter(!is.na(required_car_parking_spaces),required_car_parking_spaces %in% c(0, 1))
requires_parking_space<-nrow(filtered_parking_data%>%filter(required_car_parking_spaces==1))
dont_require_parking_space<-nrow(filtered_parking_data%>%filter(required_car_parking_spaces==0))
total=nrow(filtered_parking_data)
parking_space_required_proportion=requires_parking_space/total
parking_space_not_required_proportion=dont_require_parking_space/total


parking_data <- data.frame(
  Category = c("Requires Parking Space", "Doesn't Require Parking Space"),
  Proportion = c(parking_space_required_proportion, parking_space_not_required_proportion)
)

ggplot(parking_data, aes(x = Category, y = Proportion, fill = Category)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_text(aes(label = scales::percent(Proportion)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 3.5) +
  labs(title = "Proportion of Guests Requiring Parking Space",
       x = "Category",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pregunta N°7: ¿En qué meses del año se producen más cancelaciones de reservas?
# Para ello primero debemos analizar cuantas cancelaciones se realizaron por cada mes, según el año
library(ggplot2)
library(dplyr)

# Filtrar los datos para incluir solo los años 2015, 2016 y 2017
cancelaciones_2015_2017 <- data %>%
  filter(arrival_date_year %in% c(2015, 2016, 2017)) %>%
  group_by(arrival_date_year, arrival_date_month) %>%
  summarise(n_canceled = sum(is_canceled == "1"))

# Graficar las cancelaciones de reservas para los años 2015, 2016 y 2017 (gráfico de barras)
ggplot(cancelaciones_2015_2017, aes(x = as.factor(arrival_date_month), y = n_canceled, fill = as.factor(arrival_date_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cancelaciones de reservas por mes - Años 2015, 2016, 2017",
       x = "Mes",
       y = "Total de cancelaciones",
       fill = "Año") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x


head(cancelaciones_2015_2017)

# Ahora que conocemos las cancelaciones de cada año, tendremos el total de cancelaciones por mes en total
# Calcular el total de cancelaciones por mes en total
cancelaciones_totales_por_mes <- cancelaciones_2015_2017 %>%
  group_by(arrival_date_month) %>%
  summarise(total_cancelaciones = sum(n_canceled))

# Imprimir el total de cancelaciones por mes en total
print(cancelaciones_totales_por_mes)


library(ggplot2)

# Graficar el total de cancelaciones por mes en total (gráfico lineal)
ggplot(cancelaciones_totales_por_mes, aes(x = arrival_date_month, y = total_cancelaciones, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Total de cancelaciones por meses",
       x = "Mes",
       y = "Total de cancelaciones") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar las etiquetas del eje x

# PREGUNTAS ADICIONALES

# ¿Cuál es el porcentaje de cancelaciones de reservas en hoteles tipo Resort Hotel en comparacion con los de City Hotel?
# Filtrar datos para Resort Hotel y City Hotel
resort_data <- data[data$hotel == "Resort Hotel", ]
city_data <- data[data$hotel == "City Hotel", ]

# Calcula el número de cancelaciones de reservas para Resort Hotel
resort_cancelations <- sum(resort_data$is_canceled == 1)

# Calcula el número total de reservas para Resort Hotel
total_resort <- nrow(resort_data)

# Calcula el porcentaje de cancelaciones para Resort Hotel
resort_cancelation_percentage <- (resort_cancelations / total_resort) * 100

# Calcula el número de cancelaciones de reservas para City Hotel
city_cancelations <- sum(city_data$is_canceled == 1)

# Calcula el número total de reservas para City Hotel
total_city <- nrow(city_data)

# Calcula el porcentaje de cancelaciones para City Hotel
city_cancelation_percentage <- (city_cancelations / total_city) * 100

# Imprime los resultados
# Imprime los porcentajes de cancelaciones en Resort Hotel y City Hotel
print(paste("Porcentaje de cancelaciones en Resort Hotel:", resort_cancelation_percentage, "%"))
print(paste("Porcentaje de cancelaciones en City Hotel:", city_cancelation_percentage, "%"))

# Crear un vector con los porcentajes de cancelaciones
cancelation_percentages <- c(resort_cancelation_percentage, city_cancelation_percentage)


hotel_types <- c("Resort Hotel", "City Hotel")

# Graficar el pastel
pie(cancelation_percentages, labels = hotel_types, main = "Porcentaje de cancelaciones por tipo de hotel")

# ¿Cuál es el canal de distribución (DistributionChannel) más utilizado?
canales_por_año <- canales_por_año %>%
  filter(distribution_channel != "Undefined") %>%
  arrange(arrival_date_year, desc(count))

# Crear el gráfico de barras apiladas
ggplot(canales_por_año, aes(x = as.factor(arrival_date_year), y = count, fill = distribution_channel)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de los canales de distribución por año",
       x = "Año",
       y = "Cantidad de usos",
       fill = "Canal de distribución") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar las etiquetas del eje x
  scale_fill_brewer(palette = "Set3")

