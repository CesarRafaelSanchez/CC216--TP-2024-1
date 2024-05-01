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

