
#----------Aplicación de técnicas y métodos estadísticos-----------------------


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

# Porcentaje de personas que tiene o ha tenido enfermedades de transmisión sexual

cancer_cervix_clean %>% 
  count(STDs) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de personas que tiene o han tenido cáncer 

cancer_cervix_clean %>% 
  count(Dx.Cancer) %>% 
  mutate(porcentaje = n / sum(n) * 100)


# Cuántos pacientes han tenido el virus del Papiloma Humano

cancer_cervix_clean %>% 
  count(STDs.HPV) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de pacientes que se le ha aplicado una biopsia y su resultado

cancer_cervix_clean %>% 
  count(Biopsy) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de pacientes que se realizaron la prueba de Hinselmann

cancer_cervix_clean %>% 
  count(Hinselmann) %>% 
  mutate(porcentaje = n / sum(n) * 100)

# Porcentaje de pacientes que se realizaron la prueba de Schiller

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

# Usamos Fisher porque para datos más pequeños es una mejor aproximación, debido a que el Chi cuadrado falla cuando algún dato da 0 o son de frecuencia pequeña.

# Método de Fisher
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

# De esta tabla podemos sacar varias conclusiones, los factores que no mostraron asociación significativa con el cáncer cérvico.
# Los anticonceptivos hormonales y el ETS en general no tienen ninguna relación con la posibilidad de llegar a tener cáncer cérvico ya que p>= 0.05

