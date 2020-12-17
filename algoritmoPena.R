seleccion_variables <- function(ajuste, datos, umbral = 0.05){
  library(ggm) # correlaciones parciales
  # Aplica (intenta más bien) una selección de variables. Toma como 
  # parámetros el ajuste del modelo completo y los datos a partir de los 
  # cuales se ha formado el ajuste
  
  # Primero vamos a crear unas variables que necesitamos para funcionar
  terminos <- terms(formula(ajuste))
  vars <- as.character(attr(terminos, "variables"))[-1]
  # Todas las variables que entran en el modelo, ahora las clasificamos
  endogena <- vars[attr(terminos, "response")]
  # Nuestras explicativas de momento son ninguna
  explicativas <- c()
  # Y no entran en el modelo todas las que tiene el ajuste
  no.modelo <- as.vector(vars[-attr(terminos, "response")])
  # Creamos ahora el modelo
  aj <- lm(paste(endogena, "1", sep = "~"), data = datos)
  
  # Y empezamos ahora realmente con el algoritmo
  while (TRUE) {
    # Primer paso: calcular las correlaciones parciales entre y 
    #     y cada una de las x no introducidas en el modelo
    corr.par <- sapply(
      no.modelo, function(x){
        parcor(cov(cbind(
          datos[,endogena], datos[,x]
        )))
      }
    )
    # El resultado viene en forma de matriz de matrices tal que cada columna
    # es una matriz,
    # p.e. para la matriz (1, 2)(2, 1) (cada paréntesis es una fila)
    # vendría expresada en nuestra respuesta como:
    #  1
    #  2
    #  2
    #  1
    # Y en cada columna una matriz así. Al ser las matrices de correlaciones parciales
    # simétricas y con diagonal 1, podemos directamente quedarnos con la segunda 
    # fila de la respuesta (que correspondería a la primera fila, segunda columna 
    # de cada matriz de correlaciones parciales).
    correlaciones <- corr.par[2,]
    print(correlaciones) # print temporal
    # Seleccionamos la que tenga la máxima correlación parcial (el índice)
    max.corr <- as.character(names(which.max(correlaciones)))
    
    # Segundo paso: actualizamos el modelo para que contenga esta variable;
    #   asimismo, lo añadimos a las variables que entran en el modelo 
    #   y lo sacamos de las que no entran
    aj <- update(aj, paste("~.", max.corr, sep = "+"), data = datos)
    explicativas <- append(explicativas, max.corr)
    no.modelo <- no.modelo[no.modelo != max.corr]
    
    # Tercer paso: decidir si entra o no: calculamos el estadístico t
    #   para la variable recién introducida
    
    # Lo miramos en el summary
    resumen.aj <- summary(aj)
    coeficientes <- resumen.aj$coefficients
    
    # Y lo comprobamos
    p.max.corr <- coeficientes[max.corr, 4]
    print(p.max.corr) # print temporal
    
    # Y comprobamos si está por debajo de nuestro umbral fijado
    if (p.max.corr < umbral){
      # Cuarto paso: regla de salida. La última variable entra, OK, pero
      #   tenemos que comprobar si ahora hace falta quitar otra
      if (any(coeficientes[,4] > umbral)){
        # Hay una variable que "sobra"
        nombres <- names(which(coeficientes[,4] > umbral, 2))
        # Primer paso, quitarlas del ajuste:
        # (tenemos que tener mucho cuidado con el intercept)
        aj <- update(aj, ifelse(nombres == "(Intercept)", "~.",
                     paste("~.", nombres[nombres != "(Intercept)"],sep="-")))
        # Y las tenemos que añadir a las que no están en el modelo y sacarlas
        # de las que sí están en el modelo
        explicativas <- explicativas[!explicativas %in% nombres]
        no.modelo <- append(no.modelo, nombres[nombres != "(Intercept)"])
        
      }else{
        # Genial, introduciendo otra variable, seguimos ganando
      }
      
    }else{
      # Ya no vamos a introducir más variables, la quitamos del modelo
      # y devolvemos el objeto
      return(update(aj, paste("~.", max.corr, sep = "-")))
    }
  }
}


library(faraway)
# Vamos a probar ahora si funciona

# Vamos con Ozono
# Ajuste completo
ajuste.Ozono <- lm(Ozono~., data = OzonoLA)
(FIV <- faraway::vif(ajuste.Ozono) )# Vemos existencia de multicolinealidad
# Stepwise de R
ajuste.step.Ozono <- step(ajuste.Ozono, data = OzonoLA)
(FIV <- faraway::vif(ajuste.step.Ozono))
# Multicolinealidad moderada en T_ElMonte

# Con el algoritmo de Peña
ajuste.Ozono.pena <- seleccion_variables(ajuste.Ozono, OzonoLA)
(FIV <- faraway::vif(ajuste.Ozono.pena))
# No tenemos ninguna existencia de multicolinealidad


# Vamos a probar con otra base de datos, donde no haya multicolinealidad
# (debería otorgarnos resultados semejante)

correlaciones <- cor(NarizLarga)

det(correlaciones) # Si es muy cercano a cero hay alta multicolinealidad.

ajuste <- lm(fish~., data = NarizLarga)
(FIV <- faraway::vif(ajuste))
# Vemos que no existe multicolinealidad, vamos ahora a hacer stepwise
ajuste.step <- step(ajuste, data = NarizLarga)
summary(ajuste.step)
# Y con nuestra función
ajuste.pena <- seleccion_variables(ajuste, NarizLarga)
summary(ajuste.pena)

# Prácticamente idénticos, pero no del todo
ajuste <- lm(calidad~., data = Vinos)
(FIV <- faraway::vif(ajuste))
# No parece haber multicol
ajuste.step <- step(ajuste, data = Vinos)
# 
ajuste.pena <- seleccion_variables(ajuste, Vinos)

library(DAAG)
val_cruzada <- CVlm(data=Vinos, form.lm=formula(ajuste.pena), m=4, dots=F, 
                    plotit="Observed", legend.pos="topleft",  main="")

AIC(ajuste.step)
AIC(ajuste.pena)

BIC(ajuste.step)
BIC(ajuste.pena)
