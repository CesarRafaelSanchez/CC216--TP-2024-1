file.choose()
data=read.csv("C:\\Users\\USUARIO\\Documents\\CURSO - Fundamentos Data Science\\hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)


#¿Cuántas reservas se realizan por tipo de hotel? O ¿Qué tipo de hotel prefiere la gente?
reservas_por_hotel <- table(data$hotel)
print(reservas_por_hotel)

barplot(reservas_por_hotel, 
        main = "Reservas por Hotel", 
        xlab = "Hotel", 
        ylab = "Cantidad de Reservas",
        col = "skyblue")

# ¿Está aumentando la demanda con el tiempo?

#creamos un vector que contenga todas reservas realizadas de acuerdo al año
reservas_por_año <- aggregate(total_of_special_requests ~ arrival_date_year, data = data, FUN = sum)

#Instalamos ggplot
install.packages("ggplot2")
library(ggplot2) 
windows()


ggplot(data = reservas_por_año, aes(x = arrival_date_year, y = total_of_special_requests)) +
  geom_line() +
  labs(title = "Cantidad total de reservas por año",
       x = "Año",
       y = "Cantidad total de reservas")


# ¿Cuántas reservas incluyen niños y/o bebés?

# Contar el número de reservas que incluyen niños y/o bebés

reservas_con_niños <- sum(!is.na(hotel_bookings_TP$children) & hotel_bookings_TP$children > 0 |
                            !is.na(hotel_bookings_TP$babies) & hotel_bookings_TP$babies > 0)

print(paste("Cantidad total de reservas que incluyen niños y/o bebés:", reservas_con_niños))

# Calcular las reservas por año y mes
bookings_per_year_month <- aggregate(hotel_bookings_TP$hotel, 
                                     by = list(Year = hotel_bookings_TP$arrival_date_year,
                                               Month = hotel_bookings_TP$arrival_date_month), 
                                     FUN = length)

# Renombrar la columna que contiene el número de reservas
names(bookings_per_year_month)[3] <- "Number_of_Bookings"

# Convertir la columna Number_of_Bookings a numérica
bookings_per_year_month$Number_of_Bookings <- as.numeric(bookings_per_year_month$Number_of_Bookings)

# Calcular la media de reservas por mes para cada año
mean_bookings_per_month <- aggregate(bookings_per_year_month$Number_of_Bookings, 
                                     by = list(Month = bookings_per_year_month$Month), 
                                     FUN = mean)

# Encontrar el mes con la menor demanda de reservas
min_demand_month <- mean_bookings_per_month[which.min(mean_bookings_per_month$x), "Month"]

# Crear el gráfico
ggplot(data = mean_bookings_per_month, aes(x = Month, y = x)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Demanda Promedio de Reservas por Mes",
       x = "Mes",
       y = "Número Promedio de Reservas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept = min_demand_month, linetype = "dashed", color = "red") +
  annotate("text", x = min_demand_month, y = max(mean_bookings_per_month$x), 
           label = "Menor Demanda", color = "red", vjust = -0.5, hjust = 0)

setwd("C:/Users/bruce/Downloads/hotel_bookings.csv")
hotel_bookings <- read.csv("hotel_bookings.csv")

# Encontrar cuando se producen las temporadas de reservas alta, media y baja
month_counts <- rep(0, 12)

for (i in hotel_bookings$arrival_date_month) {
  month_index <- match(tolower(i), tolower(month.name))
  if (!is.na(month_index)) {
    month_counts[month_index] <- month_counts[month_index] + 1
  }
}
# Crear grafico
barplot(month_counts, names.arg = month.abb, xlab = "Mes", ylab = "Frecuencia", main = "Frecuencia de clientes por mes", col = "blue")


# Encontrar en que meses del año se producen mas cencelaciones de reservas
bookings_2017 <- subset(hotel_bookings, arrival_date_year == 2017)

cancellations_by_month <- tapply(bookings_2017$is_canceled, bookings_2017$arrival_date_month, sum)
months <- factor(names(cancellations_by_month), levels = month.name)
# Crear grafico
barplot(cancellations_by_month,
        main = "Cancelaciones por mes en 2017",
        xlab = "Mes",
        ylab = "Numero de cancelaciones",
        col = "skyblue")
# Imprimir valores exactos
print(cancellations_by_month)

setwd("C:/Users/tribu/Desktop/TP")
datos <- read.csv("hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)
names(datos)
str(datos)
summary(datos)
colSums(is.na(datos))
#convertimos datos vacios a NA
data<-read.csv("hotel_bookings.csv",na.strings="NA")
colSums(is.na(datos))
data<-na.omit(data)
colSums(is.na(data))

#datos atipicos
#visualizacion
boxplot(data$lead_time)
boxplot(data$stays_in_week_nights)
boxplot(data$stays_in_weekend_nights)
boxplot(data$adults)
boxplot(data$babies)
boxplot(data$children)
boxplot(data$previous_cancellations)
boxplot(data$previous_bookings_not_canceled)
boxplot(data$booking_changes)
boxplot(data$days_in_waiting_list)
boxplot(data$required_car_parking_spaces)
#trasformacion###############

#funcion para datos altos
Psuperior<-function(data,variable){
  percentilS<- quantile(data[[variable]], probs = 0.95, na.rm = TRUE)
  data[[variable]]<- ifelse(data[[variable]] > percentilS, percentilS, data[[variable]])
  return(data)
}
#funcion para datos bajos
Pinferior<-function(data,variable){
  percentilI<- quantile(data[[variable]], probs = 0.05, na.rm = TRUE)
  data[[variable]]<- ifelse(data[[variable]] < percentilI, percentilI, data[[variable]])
  return(data)
}

##para lead_time
data<-Psuperior(data,"lead_time")
boxplot(data$lead_time)
##para stays_in_week_nights
data<-Psuperior(data,"stays_in_week_nights")
boxplot(data$stays_in_week_nights)
##para stays_in_weekend_nights
data<-Psuperior(data,"stays_in_weekend_nights")
boxplot(data$stays_in_weekend_nights)

##para adults OJO====================
# Calcular cuartiles
quartiles <- quantile(data$adults, probs = c(0.25, 0.75), na.rm = TRUE)

# Calcular rango intercuartílico (IQR)
IQR <- quartiles[2] - quartiles[1]

# Calcular límites para identificar outliers
lower_limit <- quartiles[1] - 1.5 * IQR
upper_limit <- quartiles[2] + 1.5 * IQR

# Reemplazar outliers con la mediana
data$adults[data$adults < lower_limit | data$adults > upper_limit] <- median(data$adults, na.rm = TRUE)
boxplot(data$adults)

data$adults[data$adults==0]<-NA
ggplot(data, aes(x = adults)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de la variable 'adults'",
       x = "Número de adults",
       y = "Frecuencia")

##para babies OJO==============
# Calcular cuartiles
quartiles_babies <- quantile(data$babies, probs = c(0.25, 0.75), na.rm = TRUE)

# Calcular rango intercuartílico (IQR)
IQR_babies <- quartiles_babies[2] - quartiles_babies[1]

# Calcular límites para identificar outliers
lower_limit_babies <- quartiles_babies[1] - 1.5 * IQR_babies
upper_limit_babies <- quartiles_babies[2] + 1.5 * IQR_babies

# Reemplazar outliers con la mediana
median_babies <- median(data$babies, na.rm = TRUE)
data$babies[data$babies < lower_limit_babies | data$babies > upper_limit_babies] <- median_babies

boxplot(data$babies)
data <- data[-c(46620,78657),]

ggplot(data, aes(x = babies)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de la variable 'babies'",
       x = "Número de bebés",
       y = "Frecuencia")
data$babies[data$babies==0]<-NA

##para children
ggplot(data, aes(x = children)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de la variable 'children'",
       x = "Número de niños",
       y = "Frecuencia")
data$children[data$children==0]<-NA
boxplot(data$children)
data<-Psuperior(data,"children")
##para required_car_parking_spaces
#solo cuenta con 5 valores atipicos asi que los eliminamos
data <- data[-c(29046, 29047,38118,102763,110813), ]
boxplot(data$required_car_parking_spaces)
data <- data[-c(102763), ]
data$required_car_parking_spaces[data$required_car_parking_spaces==0]<-NA


# matriz de correlación
correlation_matrix <- cor(data[, c("adr", "stays_in_weekend_nights", "stays_in_week_nights", "adults", "children", "babies", "booking_changes")])
print(correlation_matrix)


# gráfico de dispersión
ggplot(data, aes(x = adr, y = stays_in_weekend_nights)) +
  geom_point() +
  labs(title = "Precio de la Habitación X Noches de Fines de Semana",
       x = "Precio de la Habitación",
       y = "Noches de Estancia durante Fines de Semana")


# Convertir la matriz de correlación en un data frame
correlation_df <- as.data.frame(correlation_matrix)
correlation_df$variables <- rownames(correlation_df)

#gráfico de barras para required_car_parking_spaces x reserva fue cancelada
ggplot(data, aes(x = factor(required_car_parking_spaces), fill = factor(is_canceled))) +
  geom_bar(position = "dodge") +
  labs(title = "Cantidad de Espacios de Reserva según Cancelación de Reserva",
       x = "Cantidad de Espacios de Reserva",
       y = "Cantidad de Reservas") +
  scale_fill_discrete(name = "Cancelación de Reserva",
                      labels = c("No", "Sí"))


# gráfico de barras para los meses con mayor cantidad de cancelaciones de reservas
ggplot(data, aes(x = arrival_date_month, fill = factor(is_canceled))) +
  geom_bar() +
  labs(title = "Cancelaciones de Reservas por Mes",
       x = "Mes",
       y = "Cantidad de Cancelaciones") +
  scale_fill_discrete(name = "Cancelación de Reserva",
                      labels = c("No", "Sí")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# cantidad de reservas por hotel
ggplot(data, aes(x = hotel, fill = factor(is_canceled))) +
  geom_bar() +
  labs(title = "Cantidad de Reservas por Hotel",
       x = "Hotel",
       y = "Cantidad de Reservas") +
  scale_fill_discrete(name = "Cancelación de Reserva",
                      labels = c("No", "Sí"))
