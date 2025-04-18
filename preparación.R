# cargar base de datos
load("C:/Users/pc17/Desktop/ipo/input/datos/Latinobarometro_2023_Esp_Rdata_v1_0.rdata")
datos_latam <- Latinobarometro_2023_Esp_v1_0

# mods
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2)

# variables
"P13ST.G"# Confianza en paridos politicos 
"P18N.F" # Funcionan bien los partidos politicos
"Sexo" # Sexo
"Edad" # Edad
"Idenpa"#Pais

# separar variables de la base de datos
confedad <- datos_latam %>% select("P13ST.G","P18N.F","sexo","edad","idenpa")

# filtrar datos de chile
confedad <- confedad %>% dplyr::filter(idenpa==152)

# rename
confedad <- confedad %>% rename("fun_partpol"=P18N.F, # Funcionan los partidos políticos 
                                "conf_partpol"=P13ST.G) # Confianza en los partidos políticos 
confedad$conf_partpol  <- set_label(x = confedad$conf_partpol, label = "Confianza en los partidos politicos")
confedad$fun_partpol  <- set_label(x = confedad$fun_partpol, label = "Funcionan los Partidos politicos")

# funcionan los partidos políticos / recode 
frq(confedad$fun_partpol)
confedad$fun_partpol <- recode(confedad$fun_partpol, "c(-5)=NA")
confedad$fun_partpol <- set_labels(confedad$fun_partpol,
                                   labels=c( "Mucha"=1,
                                             "Algo"=2,
                                             "Poca"=3,
                                             "Ninguna"=4))
# confianza en los partidos políticos / recode 
frq(confedad$conf_partpol)
confedad$conf_partpol <- recode(confedad$conf_partpol, "c(-2,-1)=NA")
confedad$conf_partpol <- set_labels(confedad$conf_partpol,
                                    labels=c( "Mucha"=1,
                                              "Algo"=2,
                                              "Poca"=3,
                                              "Ninguna"=4))
# fijar rangos etarios
confedad$edad <- cut(confedad$edad,
                     breaks = c(18, 29, 59, 92),
                     labels = c("Jóvenes (18-29)", "Adultos (30-59)", "Adultos mayores (60-92)"),
                     right = TRUE, include.lowest = TRUE)

# fijar rangos de edad
confedad$edad <- cut(confedad$edad,
                     breaks = c(18, 29, 59, 92),
                     labels = c("Jóvenes (18-29)", "Adultos (30-59)", "Adultos mayores (60-92)"),
                     right = TRUE, include.lowest = TRUE)
# configurar sexo
confedad$sexo <- car::recode(confedad$sexo, "1=0;2=1")
confedad$sexo <- factor(confedad$sexo,
                        labels=c( "Hombre",
                                  "Mujer"),
                        levels=c(0,1))

# juntar variables de confianza
confedad <- confedad %>% mutate(indice_percepcion = rowMeans(select(., conf_partpol, fun_partpol),na.rm = TRUE))

# verificar
head(confedad$indice_percepcion)

# hacer tabla
tabla_final <- confedad %>%
  group_by(edad, sexo) %>%
  summarise(
    Percepcion_promedio = mean(indice_percepcion, na.rm = TRUE),
    .groups = "drop"
  )

print(tabla_final)

# promedio con intervalos del 95%
confedad%>%
  group_by(edad, sexo) %>%
  summarise(
    Media = mean(indice_percepcion, na.rm = TRUE),
    SE = sd(indice_percepcion, na.rm = TRUE)/sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = edad, y = Media, color = sexo)) +
  geom_pointrange(aes(ymin = Media - 1.96*SE, ymax = Media + 1.96*SE),
                  size = 1,
                  position = position_dodge(width = 0.5)) +
  labs(title = "Percepción promedio con intervalos del 95%")

# comparacion por sexo y grupo etario
ggplot(confedad, aes(x = edad, y = conf_partpol, fill = sexo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Confianza en Partidos por Sexo y Edad",
    x = "Grupo Etario",
    y = "Confianza",
    fill = "Sexo"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# distribucion de confianza por densidad
ggplot(confedad, aes(x = conf_partpol)) +
  geom_histogram(aes(y = ..density..), bins = 5, fill = "#1f77b4", alpha = 0.7) +
  geom_density(color = "#ff7f0e", linewidth = 1) +
  labs(rttitle = "Distribución de Confianza en Partidos",
       x = "Nivel de Confianza (1 = Mucha, 4 = Ninguna)",
       y = "Densidad"
  ) +
  theme_minimal()