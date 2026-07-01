# Título: Clase 3 de introducción a R
# Author: Natalie Pineda
# Date: 22 junio 2026

rm(list = ls())
#-----------------------------1. Declaración de variables---------------------------------
a = 0
a <- 2 # Mejor version
b <- 3

suma = a + b
suma

#-----------------------------------------2. Librerías------------------------------------
# Cargar librerías
library("DT")
library("palmerpenguins")
library("dplyr")
library("ggplot2")
library("GGally")
library("RColorBrewer")
library("colorBlindness")
# dobles puntos dobles :: - La función pertener a un paquete
dplyr::select()

#--------------------------------------3. Dataframes--------------------------------------
# Visualizar los primeros 6 elementos por default
head(penguins)
# Visualizar los primeros 10 elementos
head(penguins,10)
# Ver cuantas dimensiones tiene 
dim(penguins)
# Numero de filas y columnas (para seleccionar columnas se pone $)
nrow(penguins)
ncol(penguins)
# Para ver la estructura
str(penguins)
# Número de especies y cuáles son 
penguins$species
unique(penguins$species) # Valores únicos unique()

# dplyr
# 1. Seleccionar el objeto par obtener los nombres únicos
penguins %>% # Llamar al objeto
  dplyr::select(species) %>% # Escribir la columna que queremos
  dplyr::distinct() # Obtener los nombres únicos
  
# Opción A -> R base + estructuras
length(unique(penguins$species))

# Opción B -> dplyr
penguins %>%  # Seleccionar el objeto (dataframe)
  dplyr::select(species) %>%  # Seleccionar la columna para evaluar
  dplyr::n_distinct() # Contar el número de especies únicas

# ¿Cuántos datos registrados tenemos por especie o número de individuos por especie?
penguins %>% 
  dplyr::count(species)

#---------Ejercicio 01: Sexo y distribución del muestreo a través de las islas------------
# ¿Cuántos datos registrados tenemos por especies o número de individuos por especie?
penguins %>% 
  count(species) 
# Número de individuos de cada sexo
penguins %>% 
  count(sex) 
# ¿Cuántos hembras y machos hay de cada especie?
penguins %>% 
  count(species, sex)
# Número de individuos registrados en cada isla:
penguins %>% 
  count(island)
penguins %>%
count(species, island, sex)

#-------------------------Ejercicio 02: Masa de los individuos----------------------------
# Crea una nueva columna llamada body_mass_kg que convierta el peso de gramos a kilogramos.
penguins_kg <- penguins %>% 
  mutate(body_mass_kg = body_mass_g / 1000) 

head(penguins_kg)

# Calcula el promedio de masa corporal por especie.
penguins_kg %>% 
  group_by(species) %>% 
  summarise(mean_mass_kg = mean(body_mass_kg, na.rm = TRUE))

# Crea una columna peso_categoria que indique "Ligero" si el pingüino pesa menos de 4000 g = 4kg y "Pesado" en caso contrario.
penguins_kg <- penguins_kg %>% 
  mutate(peso_categoria = if_else(body_mass_kg < 4, "Ligero", "Pesado"))

head(penguins_kg)

# ¿Cuántos pingüinos tenemos por categoría?
penguins_kg %>% 
  count(peso_categoria)

# Clasifica a los pingüinos en categorías según su masa corporal:
# < 3500 g → "Pequeño"
# 3500–4500 g → "Mediano"
# >4500 g → "Grande"
penguins_kg %>% 
  mutate(categoria = case_when(
    body_mass_g < 3500 ~ "Pequeño",
    body_mass_g >= 3500 & body_mass_g <= 4500 ~ "Mediano",
    body_mass_g > 4500 ~ "Grande"
  ))
#----------------------------Ejercicio 03: Longitud de pico y ala------------------------
# Ordena los pingüinos por longitud del pico (bill_length_mm) de mayor a menor.
penguins_kg %>% 
  arrange(desc(bill_length_mm)) %>%  # de mayor a menor
  # tambien se puede poner arrange(-bill_length_mm)
  head()
# Si queremos que sea de menor a mayor, quitamos desc().
penguins_kg %>% 
  arrange(bill_length_mm) %>%  # de menor a mayor
  head()
# Calcula el promedio y la desviación estándar de la longitud del ala (flipper_length_mm) por especie.
penguins %>% 
  group_by(species) %>% 
  summarise(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE), #promedio
    sd_flipper = sd(flipper_length_mm, na.rm = TRUE) # desviacion estandar
  )
#---------------------------4. Visualización de datos-------------------------------------

ggpairs(penguins, aes(colour = species), progress = FALSE) +
  theme_bw()
# Primer capa
ggplot(data = penguins)

# Segunda capa
ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm))

# Tercera capa

ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = bill_depth_mm)) + 
  geom_point()

# Cuarta capa: Color por especies

ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = bill_depth_mm, 
                            colour = species)) + 
  geom_point()

# Quinta capa: Apariencia y etiquetas

ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = bill_depth_mm, 
                            colour = species)) + 
  geom_point(size = 2, alpha = 0.7) + 
  labs(
    title = "Dimensiones del pico de los pingüinos", 
    x = "Longitud del pico (mm)", y = "Profundidad del pico (mm)",
    colour = "Especie") + 
  theme_dark()

#---Ejercicio 01: ¿Existe alguna relación entre la longitud del pico y la longitud de la aleta?---
ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = flipper_length_mm,
                            colour = species)) +
  geom_point(size = 2, alpha = 0.7) + 
  labs(
    title = "Relación entre longitud del pico y la aleta de los pingüinos", 
    x = "Longitud del pico (mm)", y = "Longitud de la aleta (mm)",
    colour = "Especie") + 
  theme_dark()

#----------------------------------------------------------------------------------------
p_penguins <- ggplot(
  penguins,
  aes(
    x = bill_len,
    y = bill_dep,
    color = species
  )
) +
  geom_point(size = 2, alpha = 0.8) +
  labs(
    title = "Medidas del pico de los pingüinos",
    x = "Longitud del pico (mm)",
    y = "Profundidad del pico (mm)",
    color = "Especie"
  )

p_penguins

p_penguins +
  scale_color_manual(
    values = c(
      "Adelie" = "grey40",
      "Chinstrap" = "orange",
      "Gentoo" = "skyblue"
    )
  )

ggplot(data = penguins, aes(x = bill_length_mm, 
                            y = bill_depth_mm,
                            colour = body_mass_g)) +
  geom_point(size = 2, alpha = 0.7) + 
  scale_color_gradient2(
    low = "blue", 
    mid = "white",
    high = "red",
    midpoint = mean(penguins$body_mass_g, na.rm = TRUE)
  )
  labs(
    title = "Dimensiones del pico de los pingüinos", 
    x = "Longitud del pico (mm)", y = "Profundidad del pico (mm)",
    colour = "Log10 de Masa corporal (g)") + 
  theme_minimal()


p_penguins

#----------------------------------------------------------------------------------------
pp2_viridis <- p_penguins + scale_color_viridis_d()
pp2_viridis
cvdPlot(pp2_viridis)

#----------------------------------------------------------------------------------------
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, 
                     shape = species, colour = species)) + 
  geom_point()


ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, 
                     size = body_mass_g, 
                     colour = species,
                     shape = species)) + 
  geom_point(alpha = 0.7) +
  xlim(30, 60) +
  ylim(12, 22)


ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point() +
  scale_x_continuous(breaks = seq(30, 60, 5)) + 
  scale_y_continuous(breaks = seq(12, 22, 2)) 

# Estéticas locales
p_penguins + geom_smooth("lm")
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) + 
  geom_point(aes(color = species), alpha = 0.7) + 
  geom_smooth(method = "lm")

p_penguins + 
  theme_minimal() + 
  theme(plot.title = element_text(color = "red", size = 12, facae = "bold.italic"),
        axis.title.x = element_text(color = "blue", size = 10, face = "bold"))


#########################################################################################
#---------------Ejercicio 1: ¿Existe dimorfismo sexual en los pingüinos? ----------------
# Los machos y las hembras de muchas especies presentan diferencias de tamaño. Usando el conjunto de datos penguins, investiga si el peso corporal de los pingüinos parece diferir entre sexos y especies. Construye una gráfica que cumpla con lo siguiente:
# Compara la variable body_mass entre machos y hembras.
# Distingue las especies mediante color.
# Elige una geometría apropiada para comparar distribuciones.
# Separa la gráfica por isla usando facetas.
# Personaliza el título, etiquetas y tema.
# A partir de la gráfica, responde:
# ¿Los machos parecen pesar más que las hembras?
# Sí
# ¿La diferencia es similar en las tres especies?
# No
ggplot(penguins, aes(y = body_mass_g, x = sex, fill = species),
         color = species,
         shape = species) +
  geom_boxplot() +
  facet_wrap(~ island) + 
  labs(
    title = "Body Mass between males and females from different species of penguins", 
    x = "Sex", y = "Body mass (g)", fill = "Specie")
  theme_minimal()

  #---------------Ejercicio 1: ¿Existe dimorfismo sexual en los pingüinos? ----------------











#-------------------------------------Tips------------------------------------------------

# a) Correr una línea de código
# Ctrl + enter en Windows
# cmd + enter en Mac

# b) Nuevo superpoder
# pipe %>%
# Ctrl + Shift + M -> Linux/Windows

# c) Comentar y descomentar cosas rápido
# Ctrl + Shift + c -> Windows y Linux
# cmd + shift + c -> Mac

# d) Ayuda en R
# ?penguins -> por ejemplo para saber como funciona el dataset de penguins