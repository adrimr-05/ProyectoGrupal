
#-------- Gráficos de distribución de las variables cuantitativas ------------------


vars_cuantitativas <- c(cols_integer, cols_double)

for (var in vars_cuantitativas) {
  grafico <- ggplot(cancer_cervix, aes(x = .data[[var]])) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
    theme_minimal()+
    theme(
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(
      title = paste("Distribución de", var),
      x = var,
      y = "Cantidad de personas"
    )
  print(grafico)
 ggsave(filename = paste0(
  "graficos/Distribucion_", var, ".png"),
   plot = grafico, width = 8, height = 6)
}



# Este gráfico muestra si existe una relación entre la edad y el número de parejas sexuales.
graf_edad_parejas <- ggplot(cancer_cervix_clean, aes(x = Age, y = Number.of.sexual.partners)) +
  geom_boxplot(fill ="red")  +
  labs(title = "Edad vs Número de Parejas Sexuales",
       x = "Edad",
       y = "Número de Parejas Sexuales") +
  theme_minimal() +
  theme(
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )

ggsave("graficos/Edad_ParejasSexuales.png", plot = graf_edad_parejas, width = 8, height = 6)

# Muestra la relación entre la edad de inicio y el número de parejas. 
graf_edadInicio_numPare <- ggplot(cancer_cervix_clean, aes(x = First.sexual.intercourse, y = Number.of.sexual.partners)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Edad de inicio de relaciones sexuales vs Número de parejas sexuales",
       x = "Edad de inicio de relaciones sexuales",
       y = "Número de parejas sexuales") +
  theme_minimal() +
  theme(
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )

ggsave("graficos/EdadInicio_ParejasSexuales.png", plot = graf_edadInicio_numPare, width = 8, height = 6)


# Diagnóstico de VPH y la aparición de cáncer de cérvix 
graf_vph_cancer <- ggplot(cancer_cervix_clean, aes(x = factor(Dx.HPV), fill = factor(Dx))) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de diagnóstico de cáncer vs diagnóstico de VPH",
       x = "Diagnóstico de VPH (0 = no, 1 = sí)",
       y = "Proporción",
       fill = "Diagnóstico de cáncer (Dx)") +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  theme_minimal() +
  theme(
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
ggsave("graficos/VPH_CxCervix.png", plot = graf_vph_cancer, width = 8, height = 6)

graf_uso_anticoncept <- ggplot(cancer_cervix_clean, aes(x = factor(Hormonal.Contraceptives))) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Distribución del uso de anticonceptivos hormonales",
       x = "Uso (0 = no, 1 = sí)",
       y = "Cantidad de personas") +
  theme_minimal() +
  theme(
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
ggsave("graficos/Anticonceptivos.png", plot = graf_uso_anticoncept, width = 8, height = 6)


#--- Gráficos de la relación de factores de riesgo con la detección de cáncer ----

# Seleccionar variables categóricas
cat_vars <- cancer_cervix_clean %>%
  select(Smokes, Hormonal.Contraceptives, IUD, STDs, Dx.Cancer)

# Convertir a formato largo
cat_long <- pivot_longer(cat_vars, cols = everything(), names_to = "variable", values_to = "valor")

# Graficar
graf_fxRiesgo <- ggplot(cat_long, aes(x = variable, fill = factor(valor))) +
  geom_bar(position = "dodge") +
  labs(title = "Factores de Riesgo",
       x = "Variable",
       y = "Cantidad de personas",
       fill = "Valor (No, Sí)")+scale_fill_manual(values = c("No"="gray70","Sí"="darkmagenta")) +theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
ggsave("graficos/FactoresRiesgo_CxCervical.png", plot = graf_fxRiesgo, width = 8, height = 6)


# Cantidad de NA por variable
colSums(is.na(cancer_cervix))

# Visualizar outliers con boxplot múltiple
numeric_df <- cancer_cervix %>% select(where(is.numeric)) %>% pivot_longer(cols = everything())


graf_outliers <- ggplot(numeric_df, aes(x = name, y = value)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() + 
  labs(title = "Posibles outliers en variables numéricas",
       x = "Variable", y = "Valor") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),
      plot.title = element_text(face = "bold", color = "#00264d", size = 15, hjust = 0.5),
      axis.title = element_text(color = "#003366"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.background = element_rect(fill = "white", color = NA)
    )
ggsave("graficos/Outliers.png", plot = graf_outliers, width = 8, height = 6)
