#----------- Exploración de datos inicial y limpieza --------------#

#-----
# Carga de datos
cancer_cervix <- read.csv("data/risk_factors_cervical_cancer.csv", na.strings = "?")
#-----

#-----
# Identificación de datos

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


# Aquí verificamos los cambios
str(cancer_cervix)
#---------

#---------
# 2- Agregar las primeras 5 filas de su tabla de datos.

head(cancer_cervix, 5)

#------------------------------------------------------------

# Limpieza de datos

# Se cambian los NA por la mediana de cada variable
df_imputado <- cancer_cervix

for (col in names(df_imputado)) {
  if (is.numeric(df_imputado[[col]])) {
    mediana <- median(df_imputado[[col]], na.rm = TRUE)
    df_imputado[[col]][is.na(df_imputado[[col]])] <- mediana
  }
}
# Primero se identifican las variables cuantitativas
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