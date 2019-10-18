require(e1071)
require(data.table)
require(dplyr)

#preparacion de los datos
path="C:/Users/mcontadora/Desktop/Nueva carpeta/caudal_extra.csv"

df_cuencas <- f_data_preparar(df_cuencas)


df_cuencas_modelo <- df_cuencas %>% select(altura,latitud,longitud,caudal,precip_promedio,temp_max_promedio,caudal_extremo)

df_cuencas_modelo <- df_cuencas_modelo[complete.cases(df_cuencas_modelo), ]
df_cuencas_modelo$Classification <- as.factor(df_cuencas_modelo$caudal_extremo)



### MUESTRA DE ENTRENAMIENTO Y DE PRUEBA ############
set.seed(1)
position <- sample(1:nrow(df_cuencas_modelo), 0.8*nrow(df_cuencas_modelo))
training <- df_cuencas_modelo[position, ]
test <- df_cuencas_modelo[-position, ]


# Ajusta el modelo con la muestra ENTRENAMIENTO
model.svm1 <- svm(caudal_extremo ~ ., data=training, kernel="linear")
model.svm2 <- svm(caudal_extremo ~ ., data=training, kernel="polynomial")
model.svm3 <- svm(caudal_extremo ~ ., data=training, kernel="radial")
model.svm4 <- svm(caudal_extremo ~ ., data=training, kernel="sigmoid")



######################### VALIDACIÓN #########################
# Valores predictivos con la muestra de PRUEBA
predval1 <- predict(model.svm1, test)
predval2 <- predict(model.svm2, test)
predval3 <- predict(model.svm3, test)
predval4 <- predict(model.svm4, test)

# Matriz de confusión
table(test$caudal_extremo, predval1)
table(test$caudal_extremo, predval2)
table(test$caudal_extremo, predval3)
table(test$caudal_extremo, predval4)

# % de error de clasificación
mean(test$caudal_extremo != predval1)
mean(test$caudal_extremo != predval2)
mean(test$caudal_extremo != predval3)
mean(test$caudal_extremo != predval4)