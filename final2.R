# Instalar y cargar librerías necesarias
if (!require("haven")) install.packages("haven", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("caret")) install.packages("caret", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("e1071")) install.packages("e1071", dependencies = TRUE)
if (!require("pROC")) install.packages("pROC", dependencies = TRUE)
if (!require("scales")) install.packages("scales", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)
if (!require(gplots)) install.packages("gplots", dependencies = TRUE)
if (!require(grid)) install.packages("grid", dependencies = TRUE)
if (!require(klaR)) install.packages("klaR", dependencies = TRUE)
if (!require(ROCR)) install.packages("ROCR", dependencies = TRUE)

library(haven)
library(dplyr)
library(caret)
library(ggplot2)
library(e1071)
library(pROC)
library(scales)
library(gridExtra)
library(gplots)
library(grid)
library(klaR)
library(ROCR)

# Cargar los datos
data <- read_dta("Base imputada EFH 2021.dta")

# Exploración inicial de los datos
str(data)
summary(data)

# Selección de variables relevantes (basado en las descripciones en el glosario)
base_varescogidas <- data[,c("t_dhip", "genero_ent", "edad_ent", "educ_ent", "t_pbco", "ytotef")]

# Eliminar filas con valores faltantes
base_varescogidas <- na.omit(base_varescogidas)

# Convertir la variables categoricas en factor con etiquetas
base_varescogidas$t_dhip <- factor(base_varescogidas$t_dhip, levels = c(0, 1), labels = c("No posee deuda hipotecaria", "Si posee deuda hipotecaria"))
base_varescogidas$genero_ent <- factor(base_varescogidas$genero_ent, levels = c(0, 1), labels = c("Mujer", "Hombre"))
base_varescogidas$t_pbco <- factor(base_varescogidas$t_pbco, levels = c(0, 1), labels = c("No posee deuda credito consumo", "Si posee deuda credito consumo"))

##########################################################################################
################Estadística descriptiva para las variables###########

table(base_varescogidas$t_pbco,base_varescogidas$genero_ent,base_varescogidas$t_dhip)
prop.table(table(base_varescogidas$genero_ent))
prop.table(table(base_varescogidas$t_dhip))
prop.table(table(base_varescogidas$t_pbco))
prop.table(table(base_varescogidas$t_pbco,base_varescogidas$genero_ent,base_varescogidas$t_dhip))
summary(base_varescogidas)

# Distribución de la variable dependiente
plot_t_dhip <- ggplot(base_varescogidas, aes(x = t_dhip)) + 
  geom_bar() + 
  ggtitle("Distribución de la variable dependiente (Tenencia de deuda hipotecaria)")

# Distribución de genero_ent
plot_genero_ent <- ggplot(base_varescogidas, aes(x = as.factor(genero_ent))) + 
  geom_bar() + 
  ggtitle("Distribución de genero")

# Distribución de t_pbco
plot_t_pbco <- ggplot(base_varescogidas, aes(x = as.factor(t_pbco))) + 
  geom_bar() + 
  ggtitle("Distribución de Tenencia de deuda bancaria consumo")

# Distribución de edad_ent
plot_edad_ent <- ggplot(base_varescogidas, aes(x = edad_ent)) + 
  geom_histogram(binwidth = 5) + 
  ggtitle("Distribución de años del entrevistado")

# Distribución de educ_ent
plot_educ_ent <- ggplot(base_varescogidas, aes(x = educ_ent)) + 
  geom_histogram(binwidth = 1) + 
  ggtitle("Distribución de años de educación")

# Distribución de ytotef
plot_ytotef <- ggplot(base_varescogidas, aes(x = ytotef)) + 
  geom_histogram(binwidth = 50000, fill = "steelblue", color = "black") + 
  ggtitle("Distribución de Ingreso total familiar") +
  scale_x_continuous(name = "Distribución de Ingreso total familiar mensual", 
                     labels = scales::dollar_format(prefix = "$", big.mark = ".", decimal.mark = ","),
                     breaks = seq(0, 5000000, by = 500000)) +
  coord_cartesian(xlim = c(0, 5000000)) + 
  theme_minimal()

grid.arrange(
  plot_t_dhip,
  plot_genero_ent,
  plot_t_pbco,
  ncol = 1
)

grid.arrange(
plot_edad_ent,
plot_educ_ent,
plot_ytotef,
  ncol = 1
)

################Dividir los datos en conjunto de entrenamiento y prueba (para knn y nb)##########

# Preparación y división de datos para KNN
data_knn <- base_varescogidas
trainIndex_knn <- createDataPartition(data_knn$t_dhip, p = .8, 
                                  list = FALSE, 
                                  times = 1)
dataTrain_knn <- data_knn[trainIndex_knn,]
dataTest_knn <- data_knn[-trainIndex_knn,]

dataTrain_knn <- dataTrain_knn %>% mutate(across(where(is.numeric), scale))
dataTest_knn <- dataTest_knn %>% mutate(across(where(is.numeric), scale))

# Preparación y división de datos para Naive Bayes
data_nb <- base_varescogidas
data_nb$genero_ent <- as.factor(data_nb$genero_ent)
data_nb$genero_ent <- as.factor(data_nb$t_pbco)
data_nb <- base_varescogidas
trainIndex_nb <- createDataPartition(data_nb$t_dhip, p = .8, 
                                      list = FALSE, 
                                      times = 1)
dataTrain_nb <- data_nb[trainIndex_nb,]
dataTest_nb <- data_nb[-trainIndex_nb,]

############################################################
#Inserción semilla 123
set.seed(123)

# Calcular las proporciones en el conjunto de entrenamiento k-NN
train_proportion_knn <- dataTrain_knn %>%
  group_by(t_dhip) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Calcular las proporciones en el conjunto de prueba k-NN
test_proportion_knn <- dataTest_knn %>%
  group_by(t_dhip) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Calcular las proporciones en el conjunto de entrenamiento Naive Bayes
train_proportion_nb <- dataTrain_nb %>%
  group_by(t_dhip) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Calcular las proporciones en el conjunto de prueba Naive Bayes
test_proportion_nb <- dataTest_nb %>%
  group_by(t_dhip) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count))

# Mostrar las proporciones para k-NN
cat("Proporciones en el conjunto de entrenamiento k-NN:\n")
print(train_proportion_knn)
cat("Proporciones en el conjunto de prueba k-NN:\n")
print(test_proportion_knn)

# Mostrar las proporciones para Naive Bayes
cat("Proporciones en el conjunto de entrenamiento Naive Bayes:\n")
print(train_proportion_nb)
cat("Proporciones en el conjunto de prueba Naive Bayes:\n")
print(test_proportion_nb)

####################################################################################################
#Proceso de remuestreo en el conjunto de datos de entrenamiento, siguiendo método de k-Fold Cross-Validation
train_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary
)

# Convertir los niveles de todas las variables categóricas a nombres válidos para la funcion train
convert_categorical_levels <- function(data) {
  data %>% mutate(across(where(is.factor), ~factor(make.names(as.character(.)))))
}

# Convertir los niveles de todas las variables categóricas en los conjuntos de datos de entrenamiento y prueba
dataTrain_knn <- convert_categorical_levels(dataTrain_knn)
dataTest_knn <- convert_categorical_levels(dataTest_knn)
dataTrain_nb <- convert_categorical_levels(dataTrain_nb)
dataTest_nb <- convert_categorical_levels(dataTest_nb)

# Modelo k-NN
model_knn <- train(t_dhip ~ ., data = dataTrain_knn, method = "knn", trControl = train_control, metric = "ROC")
model_knn

# Modelo Naive Bayes
model_nb <- train(t_dhip ~ ., data = dataTrain_nb, method = "nb", trControl = train_control, metric = "ROC")
model_nb

####################################################################################
# Predicciones k-NN
predictions_knn <- predict(model_knn, dataTest_knn)

# Predicciones Naive Bayes
predictions_nb <- predict(model_nb, dataTest_nb)

# Matrices de confusión
conf_matrix_knn<-confusionMatrix(predictions_knn, dataTest_knn$t_dhip)
conf_matrix_nb<-confusionMatrix(predictions_nb, dataTest_nb$t_dhip)
conf_matrix_knn
conf_matrix_nb

# # # # # # # # # # # # # # # Gráfica de matriz de confusión para KNN
# Nombres de los cuadrantes
labels <- c("VN", "FP", "FN", "VP")

# Valores en la matriz de confusión
values <- c(conf_matrix_knn$table[1, 1], conf_matrix_knn$table[1, 2], 
            conf_matrix_knn$table[2, 1], conf_matrix_knn$table[2, 2])

# Crear el mosaicplot
mosaicplot(conf_matrix_knn$table, main = "Modelo KNN", shade = TRUE, 
           colorize = TRUE, gp = gpar(fill = matrix(c("blue", "red", "red", "blue"), 2, 2)))

# Añadir los textos en los cuadrantes
text(x = c(0.25, 0.7, 0.25, 0.7), y = c(0.75, 0.95, 0.01, 0.3),
     labels = paste(labels, "\n", values), col = "black", cex = 1.2)

# # # # # # # # # # # # # # # Gráfica de matriz de confusión para NB

# Valores en la matriz de confusión
values_NB <- c(conf_matrix_nb$table[1, 1], conf_matrix_nb$table[1, 2], 
            conf_matrix_nb$table[2, 1], conf_matrix_nb$table[2, 2])

# Crear el mosaicplot
mosaicplot(conf_matrix_nb$table, main = "Modelo NB", shade = TRUE, 
           colorize = TRUE, gp = gpar(fill = matrix(c("blue", "red", "red", "blue"), 2, 2)))

# Añadir los textos en los cuadrantes
text(x = c(0.25, 0.75, 0.25, 0.75), y = c(0.75, 0.84, 0.04, 0.3),
     labels = paste(labels, "\n", values_NB), col = "black", cex = 1.2)

###########################################################################
#############################CURVAS ROC Y AUC############################

# Convertir las predicciones en objetos ROCR
pred_knn <- prediction(as.numeric(predictions_knn), dataTest_knn$t_dhip)
pred_nb <- prediction(as.numeric(predictions_nb), dataTest_nb$t_dhip)

# Calcular las curvas ROC y el AUC para ambos modelos
perf_knn <- performance(pred_knn, "tpr", "fpr")
auc_knn <- performance(pred_knn, "auc")
auc_knn <- auc_knn@y.values[[1]]

perf_nb <- performance(pred_nb, "tpr", "fpr")
auc_nb <- performance(pred_nb, "auc")
auc_nb <- auc_nb@y.values[[1]]

# Plot de las curvas ROC
plot(perf_knn, col = "blue", main = "Curva ROC k-NN vs Naive Bayes", xlab = "1 - Especificidad", ylab = "Sensibilidad")
plot(perf_nb, col = "red", add = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")

legend("bottomright", 
       legend = c(
         paste("k-NN AUC:", round(auc_knn, 3)),
         paste("k-NN Sensibilidad:", round(sensibility, 3)),
         paste("k-NN Especificidad:", round(specificity, 3)),
         paste("Naive Bayes AUC:", round(auc_nb, 3)),
         paste("Naive Bayes Sensibilidad:", round(sensibility_NB, 3)),
         paste("Naive Bayes Especificidad:", round(specificity_NB, 3))
       ),
       col = c("blue", "blue", "blue", "red", "red", "red"), 
       lwd = 2)

# Evaluar el rendimiento de los modelos
resamples_list <- resamples(list(knn = model_knn, nb = model_nb))
summary(resamples_list)
dotplot(resamples_list)


