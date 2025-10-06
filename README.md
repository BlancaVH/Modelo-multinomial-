# Modelo-multinomial-
Base de datos de Latinobarómetro en Excel (variables: Año, País, Edad, Sexo, Autoubicación en ideología).

#Guardar y cargar la base en R
# Instalar paquetes 
install.packages("readxl")
install.packages("nnet")
install.packages("marginaleffects")
install.packages("dplyr")

# Cargar librerías
library(readxl)
library(nnet)
library(marginaleffects)
library(dplyr)

# Cargar archivo Excel (ajusta la ruta a donde lo tengas guardado)
data <- read_excel("ideologia.xlsx")

# Ver estructura
head(data)
str(data)


#Revisar variables
#De tu archivo veo que tienes:
	#NUMINVES = Año (numérica, no nos importa ahora).
	#IDENPA = País (numérica, puede servir para agrupar).
	#EDAD = Edad (numérica).
	#SEXO = Sexo (1 = hombre, 2 = mujer).
	#P16ST = Autoubicación en escala izquierda-derecha (0–10, donde a veces se codifica 97 = No sabe).

#Antes de usar, hay que limpiar datos raros como el “97”:
# Reemplazar 97 en ideología por NA
data <- data %>%
  mutate(
    P16ST = ifelse(P16ST == 97, NA, P16ST),
    Sexo = factor(Sexo, levels = c(1,2), labels = c("Hombre", "Mujer"))
  )

# Quitar filas con NA en ideología
data <- na.omit(data)

#Convertir la variable dependiente en categórica
#En modelos multinomiales se necesita que la variable dependiente sea factor con varias categorías (no solo números).
#La escala de 0–10 se puede tratar como categórica:

# Variable dependiente: ideología
data$P16ST <- factor(data$P16ST)

table(data$P16ST) 
# ver distribución

# Ajustar modelo multinomial
modelo <- multinom(P16ST ~ Edad + Sexo, data = data, trace = FALSE)

# Resultados
summary(modelo)

# Odds ratios
exp(coef(modelo))

#Efectos marginales
#Para interpretar más fácil:
ame_edad <- avg_slopes(modelo, variables = "Edad", type = "probs")
ame_edad

ame_sexo <- avg_slopes(modelo, variables = "Sexo", type = "probs")
ame_sexo
