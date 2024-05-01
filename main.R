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

