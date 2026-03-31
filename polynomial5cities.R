# Polynomial passing by Spain's 5 largest cities
# www.overfitting.net
# https://www.overfitting.net/


library(Cairo)  # output antialiasing


#######################################
# OLD STYLE CODING

library(ggmap)  # map_data() provides (long, lat) pairs forming all countries borders
library(data.table)

# Polygon plotting function provided by ChatGPT
# Draws a series of points based on coordinates as solid polygons
plot_subregion_polygons <- function(df, x = "long", y = "lat", subregion = "subregion",
                                    ord = "order", border = "black", col = NA) {
    
    df <- as.data.frame(df)   # in case df is a data.table
    subs <- unique(df[[subregion]])
    
    for (s in subs) {
        if (is.na(s)) {  # usually main lands (not islands) of each country have subregion=NA
            d <- df[is.na(df[[subregion]]), ]
        } else {
            d <- df[df[[subregion]] == s, ]
        }
        
        d <- d[order(d[[ord]]), ]  # order by column "order" in case they are not
        polygon(d[[x]], d[[y]],    # to properly plot the polygon
                border = border,
                col = col)
    }
}


# READ WORLD AND CAPITALS COORDINATES
DT=data.table(map_data("world"))  # (long, lat) pairs for all countries
SP=DT[DT$region=='Spain']

# Capitals
cities=data.frame(
    city = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Zaragoza"),
    long = c(-3.7038,  2.1686, -0.3763, -5.9845, -0.8891),
    lat  = c(40.4168, 41.3874, 39.4699, 37.3891, 41.6488)
)


# CALCULATE 4TH ORDER POLYNOM THAT FITS 5 (LONG,LAT) PAIRS
fit = lm(lat ~ poly(long, 4, raw = TRUE), data = cities)
# Extract coefficients
cfs = coef(fit)
# Build polynomial string
poly_string = paste0(
    "y = ",
    round(cfs[1], 3), " + ",
    round(cfs[2], 3), "·x + ",
    round(cfs[3], 3), "·x^2 + ",
    round(cfs[4], 3), "·x^3 + ",
    round(cfs[5], 3), "·x^4"
)
# Generate 800 longitudes from -10 to 5
x_seq = seq(-10, 5, length.out = 800)
# Predict polynomial values
y_seq = predict(fit, newdata = data.frame(long = x_seq))


# MAP

# Output image dimensions
DIMX=512
DIMY=DIMX

CairoPNG("polynomial_spain5largestcities1.png", width=DIMX, height=DIMY, antialias="subpixel")
    # Empty basic plotting parameters
    plot(NA,
         main = paste0("Spain and its five largest cities\n", poly_string),
         xlab = "Longitude (º)", ylab = "Latitude (º)",
         xlim = range(SP$long, na.rm = TRUE),
         ylim = c(35,45),  # range(SP$lat,  na.rm = TRUE),
         cex.main = 1.3, cex.lab = 1, cex.axis = 1,
         asp = 1.3)

    # Solid country maps
    plot_subregion_polygons(SP, border='darkred', col=rgb(1,0,0,0.2))
    
    # Plot the polynomial
    lines(x_seq, y_seq, type = "l", lwd = 2, col = 'blue',
         xlab = "Longitude", ylab = "Latitude",
         main = "4th Degree Interpolating Polynomial (lm version)")
    
    # Plot cities
    points(cities$long, cities$lat, 
           pch = 19,       # solid circle
           cex = 1, col = "black")
    text(cities$long, cities$lat,
         labels = cities$city,
         pos = c(4, 4),    # 4 = right of the point
         cex = 1.3, col = "black")
    
    abline(h=0, v=0, lty="dotted")
dev.off()


#######################################
# COPILOT CODING (USING GGPLOT2)

# "Necesito dibujar en R un mapa de España, indicando sus 5 principales
# ciudades, y además calcular el polinomio de grado 4 que pasa por
# las 5 parejas de (longitud, latitud) a las que corresponden dichas
# ciudades y se dibuje también el polinomio sobre el mapa"

library(ggplot2)  # map_data() provides (long, lat) pairs forming all countries borders
library(dplyr)


# -----------------------------
# Datos de ciudades
# -----------------------------
cities <- data.frame(
    city = c("Madrid", "Barcelona", "Valencia", "Sevilla", "Zaragoza"),
    long = c(-3.7038,  2.1686, -0.3763, -5.9845, -0.8891),
    lat  = c(40.4168, 41.3874, 39.4699, 37.3891, 41.6488)
)

# -----------------------------
# Ajuste del polinomio de grado 4
# latitud = f(longitud)
# -----------------------------
fit <- lm(lat ~ poly(long, 4, raw = TRUE), data = cities)
# Extract coefficients
cfs = coef(fit)
# Build polynomial string
poly_string = paste0(
    "y = ",
    round(cfs[1], 3), " + ",
    round(cfs[2], 3), "·x + ",
    round(cfs[3], 3), "·x^2 + ",
    round(cfs[4], 3), "·x^3 + ",
    round(cfs[5], 3), "·x^4"
)

# Crear secuencia de longitudes para dibujar la curva
long_seq <- seq(min(cities$long), max(cities$long), length.out = 800)
lat_pred <- predict(fit, newdata = data.frame(long = long_seq))
curva <- data.frame(long = long_seq, lat = lat_pred)

# -----------------------------
# Mapa + ciudades + polinomio
# -----------------------------
mapa <- map_data("world")


# MAP

# Output image dimensions
DIMX=512
DIMY=DIMX

CairoPNG("polynomial_spain5largestcities2.png", width=DIMX, height=DIMY, antialias="subpixel")
    ggplot() +
        labs(title = paste0("Spain and its five largest cities\n", poly_string),
             x = "Longitude (º)",
             y = "Latitude (º)") +
        geom_polygon(data = subset(mapa, region == "Spain"),
                     aes(x = long, y = lat, group = group),
                     fill = "gray90", color = "black") +
        geom_text(data = cities, aes(x = long, y = lat, label = city),
                  hjust = -0.2, vjust = 0.5, size = 5) +
        geom_line(data = curva, aes(x = long, y = lat), color = "blue", linewidth = 1) +
        geom_point(data = cities, aes(x = long, y = lat), color = "red", size = 3) +
        coord_quickmap() +
        theme_minimal(base_size = 6) +
        theme(
            plot.title = element_text(size = 17),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)
        )
dev.off()




