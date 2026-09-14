# Sistema visual ----
library(ggplot2)

# Paletas de colores ----
## 4 variables ----
colores_4 <- c("#08989C",
               "#003057",
               "#9F2578",
               "#DB551E"
  )

## 6 variables ----
colores_6 <- c("#08989C", 
               "#003057", 
               "#9F2578", 
               "#DB551E", 
               "#207A63", 
               "#6342FF"
               )

# Tema de gráficas ----
tema_graficas <- theme_minimal(base_family = "sans") + 
  theme(
    # Fondo
    plot.background = element_rect(fill = "#FFFFFF", color = NA),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    
    # Jerarquía de títulos
    plot.title = element_text(face = "bold", 
                              size = 14, 
                              color = "#003057", 
                              hjust = 0.5),
    
    plot.subtitle = element_text(size = 12, 
                                 color = "#27251F", 
                                 hjust = 0.5, 
                                 margin = margin(b = 20)
                                 ),
    
    # Ejes y líneas de división
    axis.line = element_line(color = "#7A8A96", 
                             linewidth = 0.5),
    panel.grid.major.y = element_line(color = "#8696A2", 
                                      linetype = "dashed", 
                                      linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    # Textos de los ejes
    axis.text = element_text(size = 10, 
                             color = "#4D565E"),
    axis.title.x = element_text(face = "bold", 
                                size = 12, 
                                color = "#4D565E", 
                                margin = margin(t = 15)),
    axis.title.y = element_blank(), 
    
    # Leyenda y pie de gráfica
    legend.position = "bottom",
    legend.text = element_text(size = 10, 
                               color = "#4D565E"),
    plot.caption = element_text(size = 8, 
                                color = "#4D565E", 
                                hjust = 0, 
                                margin = margin(t = 20))
  )

theme_set(tema_graficas)