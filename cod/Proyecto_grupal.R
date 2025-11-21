
# Carga de paquetes
library(ggplot2)
library(tidyr)
library(readxl)
library(readr)
library(dplyr)
#------

#-----
# Carga de datos
cancer_cervix <- read.csv("data/risk_factors_cervical_cancer.csv", na.strings = "?")
#-----

#-----
# Identificacion de datos

cols_integer <- c(
  "Age",
  "Number.of.sexual.partners",
  "First.sexual.intercourse",
  "Num.of.pregnancies",
  "STDs..number.",
  "STDs..Number.of.diagnosis"
)
cols_double <- c(
  "Smokes..years.",
  "Smokes..packs.year.",
  "Hormonal.Contraceptives..years.",
  "IUD..years.",
  "STDs..Time.since.first.diagnosis",
  "STDs..Time.since.last.diagnosis"
)
cols_factor <- c(
  "Smokes",
  "Hormonal.Contraceptives",
  "IUD",
  "STDs",
  "STDs.condylomatosis",
  "STDs.cervical.condylomatosis",
  "STDs.vaginal.condylomatosis",
  "STDs.vulvo.perineal.condylomatosis",
  "STDs.syphilis",
  "STDs.pelvic.inflammatory.disease",
  "STDs.genital.herpes",
  "STDs.molluscum.contagiosum",
  "STDs.AIDS",
  "STDs.HIV",
  "STDs.Hepatitis.B",
  "STDs.HPV",
  "Dx.Cancer",
  "Dx.CIN",
  "Dx.HPV",
  "Dx",
  "Hinselmann",
  "Schiller",
  "Citology",
  "Biopsy"
)

cancer_cervix <- cancer_cervix %>%
  mutate(across(all_of(cols_integer), ~ as.integer(round(as.numeric(.))))) %>%
  mutate(across(all_of(cols_double), ~ as.double(as.numeric(.)))) %>%
  mutate(across(all_of(cols_factor), ~ factor(ifelse(as.numeric(.) == 1, "Sí", ifelse(as.numeric(.) == 0, "No", NA)), levels = c("No", "Sí"))))


#Aqui verificamos lo cambios
str(cancer_cervix)
#---------

#---------
#2- Agregar las primeras 5 filas de su tabla de datos.

head(cancer_cervix, 5)

#------------------------------------------------------------

# Limpieza de datos

#Se cambian los NA por la mediana de cada variable
df_imputado <- cancer_cervix

for (col in names(df_imputado)) {
  if (is.numeric(df_imputado[[col]])) {
    mediana <- median(df_imputado[[col]], na.rm = TRUE)
    df_imputado[[col]][is.na(df_imputado[[col]])] <- mediana
  }
}
#Primero se identifican las variables cuantitativas
vars_cuantitativas <- c(
  "Age",
  "Number.of.sexual.partners",
  "First.sexual.intercourse",
  "Num.of.pregnancies",
  "STDs..number.",
  "STDs..Number.of.diagnosis",
  "Smokes..years.",
  "Smokes..packs.year.",
  "Hormonal.Contraceptives..years.",
  "IUD..years.",
  "STDs..Time.since.first.diagnosis",
  "STDs..Time.since.last.diagnosis"
)

resumen <- df_imputado %>%
  select(all_of(vars_cuantitativas)) %>%
  summary()

resumen
#-----------------------------------------------------------------------------
## Se reemplazan los NAs de variables numericas 
cancer_cervix_clean <- cancer_cervix %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
#------------------------------------------------------------------------------


#-----------------------------------------------------------------------------
#### 4. Gráficos de distribución de las variables cuantitativas

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



#Este gráfico muestra si existe una relación entre la edad y el número de parejas sexuales.
graf_edad_parejas <- ggplot(cancer_cervix_clean, aes(x = Age, y = Number.of.sexual.partners)) +
  geom_point(alpha = 0.6, color = "red")  +
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
  geom_point(alpha = 0.6, color = "purple") +
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


# Diagnostico de VPH y la aparición de cáncer de cérvix 
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


#Graficos de la relacion de factores de riesgo con la deteccion de cancer
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
       fill = "Valor (0 = no, 1 = sí)")+scale_fill_manual(values = c("gray70","darkmagenta")) +theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
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
  coord_flip() +  # <- gira el gráfico
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

#---------------------------------------------------------------------------------

##8- Investigar técnicas que permitan subsanar los valores perdidos y outliers.

##Para manejar los valores faltantes se suelen combinar tres estrategias. (1) Eliminación de casos: descarta filas o celdas con NA, es simple y evita supuestos, pero reduce el tamaño muestral y puede sesgar si la ausencia no es aleatoria. (2) Imputación simple: reemplaza NA con una estadística que puede ser la mediana o la media depende del analisis, preserva el tamaño muestral y es fácil de interpretar, aunque subestima la variabilidad. (3) Imputación multivariante: usa la información conjunta de otras variables para estimar los NA, manteniendo relaciones entre variables y reflejando mejor la incertidumbre, requiere más cómputo y supuestos, pero ofrece imputaciones más realistas.

#----------------------------------------------------------------------------------

# Aplicacion de tecnicas y metodos estadisticos


#Haremos tablas de Frecuencia para poder ver proporciones y prevalencias.
#Nos importan las clases factor
#Buscamos en las variables a ver cuales son "Factor"


#Porcentaje de gente que no fuma y que si

cancer_cervix_clean %>% 
  count(Smokes) %>% 
  mutate(porcentaje = n / sum(n) * 100)

#Porcentaje de gente que toma anticonceptivos hormonales

cancer_cervix_clean %>% 
  count(Hormonal.Contraceptives) %>% 
  mutate(porcentaje = n / sum(n) * 100)


#Porcentaje de gente que tiene el IUD

cancer_cervix_clean %>% 
  count(IUD) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de personas que tiene o ha tenido enfermedades de transmision sexual

cancer_cervix_clean %>% 
  count(STDs) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de personas que tiene o han tenido cancer 

cancer_cervix_clean %>% 
  count(Dx.Cancer) %>% 
  mutate(porcentaje = n / sum(n) * 100)


#Cuantos pacientes han tenido el virus del Papiloma Humano

cancer_cervix_clean %>% 
  count(STDs.HPV) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de pacientes que se le ha aplicado una biopsia y si resultado

cancer_cervix_clean %>% 
  count(Biopsy) %>% 
  mutate(porcentaje = n / sum(n) * 100)

#Porcentaje de pacientes que se realizaron la prubea de Hinselmann

cancer_cervix_clean %>% 
  count(Hinselmann) %>% 
  mutate(porcentaje = n / sum(n) * 100)

#Porcentaje de pacientes que se realizaron la prubea de Schiller

cancer_cervix_clean %>% 
  count(Schiller) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de personas con el virus del papiloma humano actualmente

cancer_cervix_clean %>% 
  count(Dx.HPV) %>% 
  mutate(porcentaje = n / sum(n) * 100)


# Tabla cruzada: Smokes vs Dx.Cancer

tabla_smokes <- table(cancer_cervix_clean$Smokes, cancer_cervix_clean$Dx.Cancer)
tabla_smokes
prop.table(tabla_smokes, 1) * 100   
prop.table(tabla_smokes, 2) * 100   



#Tabla cruzada: Hormonal.Contraceptives vs Dx.Cancer

tabla_horm <- table(cancer_cervix_clean$Hormonal.Contraceptives, cancer_cervix_clean$Dx.Cancer)
tabla_horm
prop.table(tabla_horm, 1) * 100
prop.table(tabla_horm, 2) * 100



#Tabla cruzada: IUD vs Dx.Cancer

tabla_iud <- table(cancer_cervix_clean$IUD, cancer_cervix_clean$Dx.Cancer)
tabla_iud
prop.table(tabla_iud, 1) * 100
prop.table(tabla_iud, 2) * 100



# Tabla cruzada: STDs vs Dx.Cancer

tabla_stds <- table(cancer_cervix_clean$STDs, cancer_cervix_clean$Dx.Cancer)
tabla_stds
prop.table(tabla_stds, 1) * 100
prop.table(tabla_stds, 2) * 100



# Tabla cruzada: Dx.Cancer (distribución base)

tabla_cancer <- table(cancer_cervix_clean$Dx.Cancer)
tabla_cancer
(tabla_cancer / sum(tabla_cancer)) * 100   # porcentaje global



# 6. TABLAS CRUZADAS: STDs.HPV vs Dx.Cancer

tabla_stds_hpv <- table(cancer_cervix_clean$STDs.HPV, cancer_cervix_clean$Dx.Cancer)
tabla_stds_hpv
prop.table(tabla_stds_hpv, 1) * 100
prop.table(tabla_stds_hpv, 2) * 100



# Tabla cruzada: Biopsy vs Dx.Cancer

tabla_biopsy <- table(cancer_cervix_clean$Biopsy, cancer_cervix_clean$Dx.Cancer)
tabla_biopsy
prop.table(tabla_biopsy, 1) * 100
prop.table(tabla_biopsy, 2) * 100



# Tabla cruzada: Hinselmann vs Dx.Cancer

tabla_hinsel <- table(cancer_cervix_clean$Hinselmann, cancer_cervix_clean$Dx.Cancer)
tabla_hinsel
prop.table(tabla_hinsel, 1) * 100
prop.table(tabla_hinsel, 2) * 100



#Tabla cruzada: Schiller vs Dx.Cancer

tabla_schiller <- table(cancer_cervix_clean$Schiller, cancer_cervix_clean$Dx.Cancer)
tabla_schiller
prop.table(tabla_schiller, 1) * 100
prop.table(tabla_schiller, 2) * 100



#Tabla cruzada: Dx.HPV (HPV actual) vs Dx.Cancer

tabla_dxhpv <- table(cancer_cervix_clean$Dx.HPV, cancer_cervix_clean$Dx.Cancer)
tabla_dxhpv
prop.table(tabla_dxhpv, 1) * 100
prop.table(tabla_dxhpv, 2) * 100






# Smokes vs Dx.Cancer
rownames(tabla_smokes) <- c("No fuma", "Sí fuma")
colnames(tabla_smokes) <- c("No tiene cáncer", "Sí tiene cáncer")
tabla_smokes

#Hormonal.Contraceptives vs Dx.Cancer
rownames(tabla_horm) <- c("No usa anticonceptivos", "Sí usa anticonceptivos")
colnames(tabla_horm) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_horm

#IUD vs Dx.Cancer
rownames(tabla_iud) <- c("No usa DIU", "Sí usa DIU")
colnames(tabla_iud) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_iud

#STDs vs Dx.Cancer
rownames(tabla_stds) <- c("Sin ETS", "Con ETS")
colnames(tabla_stds) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_stds

#STDs.HPV vs Dx.Cancer
rownames(tabla_stds_hpv) <- c("HPV negativo", "HPV positivo")
colnames(tabla_stds_hpv) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_stds_hpv

#Biopsy vs Dx.Cancer
rownames(tabla_biopsy) <- c("Biopsia negativa", "Biopsia positiva")
colnames(tabla_biopsy) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_biopsy


#Hinselmann vs Dx.Cancer
rownames(tabla_hinsel) <- c("Hinselmann negativo", "Hinselmann positivo")
colnames(tabla_hinsel) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_hinsel


#Schiller vs Dx.Cancer
rownames(tabla_schiller) <- c("Schiller negativo", "Schiller positivo")
colnames(tabla_schiller) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_schiller


#Dx.HPV (HPV actual) vs Dx.Cancer
rownames(tabla_dxhpv) <- c("HPV negativo", "HPV positivo")
colnames(tabla_dxhpv) <- c("No tiene cáncer", "Sí tiene cáncer")

tabla_dxhpv




# Chi-cuadrado
chisq.test(tabla_smokes)
chisq.test(tabla_horm)
chisq.test(tabla_iud)
chisq.test(tabla_stds)
chisq.test(tabla_stds_hpv)
chisq.test(tabla_biopsy)
chisq.test(tabla_hinsel)
chisq.test(tabla_schiller)
chisq.test(tabla_dxhpv)

#Usamos Fisher porque para datos mas pequenos es una mejor aproximacion, debido a que el Chi cuadrado falla cuando algun dato da 0 o son de frecuencia pequena.

# Metodo de Fisher
fisher.test(tabla_smokes)
fisher.test(tabla_horm)
fisher.test(tabla_iud)
fisher.test(tabla_stds)
fisher.test(tabla_stds_hpv)
fisher.test(tabla_biopsy)
fisher.test(tabla_hinsel)
fisher.test(tabla_schiller)
fisher.test(tabla_dxhpv)

resultado_fisher <- data.frame(
  Variable = c("Anticonceptivos hormonales", "Uso de DIU", "ETS (general)",
               "HPV previo", "Biopsia positiva", "Hinselmann positivo",
               "Schiller positivo", "HPV actual"),
  p_value = c(0.6209, 0.01013, 1, 0.0005404, 0.0005391, 0.004734,
              0.0003944, 2.2e-16),
  OR = c(1.465961, 4.182074, 1.068107, Inf, 8.025723, 7.415785, 7.307571, 2572.983),
  IC95_inf = c(0.4837623, 1.250913, 0.1169674, 7.922311, 2.368482, 1.680165,
               2.321709, 360.6108),
  IC95_sup = c(5.3108782, 12.458534, 4.6819471, Inf, 24.335535, 25.489174,
               21.443271, 4.5036e15),
  Evidencia_de_relacion = c("No", "Sí", "No", "Sí", "Sí", "Sí", "Sí", "Sí")
)
resultado_fisher[] <- lapply(resultado_fisher, function(x) {
  if (is.numeric(x)) format(x, scientific = FALSE) else x
})
resultado_fisher

# De esta tala podemos sacar varias conclusiones, los factores que no mostraron asociacion significativa con el cancer cervico
# Los anticonceptivos hormonales y el ETS en general no tienen ninguna relacion con la posibilidad de llegar a tnener cancer cerico ya que p>= 0.05

