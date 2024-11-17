install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("caret") 
library(caret)




Player_15 <- read.csv('../fifa2020/players_15.csv', sep=",")
Player_16 <- read.csv('../fifa2020/players_16.csv', sep=",")
Player_17 <- read.csv('../fifa2020/players_17.csv', sep=",")
Player_18 <- read.csv('../fifa2020/players_18.csv', sep=",")
Player_19 <- read.csv('../fifa2020/players_19.csv', sep=",")
Player_20 <- read.csv('../fifa2020/players_20.csv', sep=",")
Teams_and_leagues <- read.csv('../fifa2020/teams_and_leagues.csv', sep=",")



Players  <- rbind( Player_15,Player_16,Player_17,Player_18,Player_19,Player_20)
View(Players)
View(Teams_and_leagues)

##------------------DATA CLEANING --------------------
#verificar duplicados


# Eliminar las filas duplicadas basadas en la columna 'sofifa_id'
Players_sin_duplicados <- Players[!duplicated(Players$sofifa_id), ]

Players_NA <- colSums(is.na(Players_sin_duplicados))
Teams_and_leagues_NA <- colSums(is.na(Teams_and_leagues))


View(Players_NA)
View(Teams_and_leagues_NA)
#-------------- Limpieza de datos 

# para los datos comunes se les asignara un promedio

# Calcular el promedio de la columna sin los NA de release_clause_eur
promedio_release_clause_eur <- mean(Players_sin_duplicados[["release_clause_eur"]], na.rm = TRUE)
Players_sin_duplicados[["release_clause_eur"]][is.na(Players_sin_duplicados[["release_clause_eur"]])] <- promedio_release_clause_eur

# Calcular el promedio de la columna sin los NA de pace
promedio_pace <- mean(Players_sin_duplicados[["pace"]], na.rm = TRUE)
Players_sin_duplicados[["pace"]][is.na(Players_sin_duplicados[["pace"]])] <- promedio_pace

# Calcular el promedio de la columna sin los NA de shooting
promedio_shooting <- mean(Players_sin_duplicados[["shooting"]], na.rm = TRUE)
Players_sin_duplicados[["shooting"]][is.na(Players_sin_duplicados[["shooting"]])] <- promedio_shooting

# Calcular el promedio de la columna sin los NA de passing
promedio_passing <- mean(Players_sin_duplicados[["passing"]], na.rm = TRUE)
Players_sin_duplicados[["passing"]][is.na(Players_sin_duplicados[["passing"]])] <- promedio_passing

# Calcular el promedio de la columna sin los NA de dribbling
promedio_dribbling <- mean(Players_sin_duplicados[["dribbling"]], na.rm = TRUE)
Players_sin_duplicados[["dribbling"]][is.na(Players_sin_duplicados[["dribbling"]])] <- promedio_dribbling

# Calcular el promedio de la columna sin los NA de defending
promedio_defending <- mean(Players_sin_duplicados[["defending"]], na.rm = TRUE)
Players_sin_duplicados[["defending"]][is.na(Players_sin_duplicados[["defending"]])] <- promedio_defending

# Calcular el promedio de la columna sin los NA de physic
promedio_physic <- mean(Players_sin_duplicados[["physic"]], na.rm = TRUE)
Players_sin_duplicados[["physic"]][is.na(Players_sin_duplicados[["physic"]])] <- promedio_physic

#---------------- remplazar por valores aleatorios

#Paso 1: Extraer los valores existentes en la columna mentality_composure, sin los NA
mentality_composure_sin_NA <- Players_sin_duplicados$mentality_composure[!is.na(Players_sin_duplicados$mentality_composure)]

# Paso 2: Calcular cuántos NA hay que reemplazar
mentality_composure_na <- sum(is.na(Players_sin_duplicados$mentality_composure))


set.seed(123)  
Players_sin_duplicados$mentality_composure[is.na(Players_sin_duplicados$mentality_composure)] <- sample(mentality_composure_sin_NA, mentality_composure_na, replace = TRUE)
View(Players_sin_duplicados)

#-------------------------promedio para datos de portero unicamente

# Calcular el promedio de la columna gk_diving sin los NA
promedio_gk_diving <- mean(Players_sin_duplicados[["gk_diving"]], na.rm = TRUE)

Players_sin_duplicados[["gk_diving"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_diving"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_diving)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_diving"])
  }
})



# ----- Calcular el promedio de la columna gk_handling sin los NA
promedio_gk_handling <- mean(Players_sin_duplicados[["gk_handling"]], na.rm = TRUE)

Players_sin_duplicados[["gk_handling"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_handling"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_handling)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_handling"])
  }
})


# ----- Calcular el promedio de la columna gk_handling sin los NA
promedio_gk_kicking <- mean(Players_sin_duplicados[["gk_kicking"]], na.rm = TRUE)

Players_sin_duplicados[["gk_kicking"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_kicking"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_kicking)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_kicking"])
  }
})


# ----- Calcular el promedio de la columna gk_reflexes sin los NA
promedio_gk_reflexes <- mean(Players_sin_duplicados[["gk_reflexes"]], na.rm = TRUE)

Players_sin_duplicados[["gk_reflexes"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_reflexes"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_reflexes)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_reflexes"])
  }
})

# ----- Calcular el promedio de la columna gk_speed sin los NA
promedio_gk_speed <- mean(Players_sin_duplicados[["gk_speed"]], na.rm = TRUE)

Players_sin_duplicados[["gk_speed"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_speed"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_speed)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_speed"])
  }
})

# ----- Calcular el promedio de la columna gk_positioning sin los NA
promedio_gk_positioning <- mean(Players_sin_duplicados[["gk_positioning"]], na.rm = TRUE)

Players_sin_duplicados[["gk_positioning"]] <- sapply(1:nrow(Players_sin_duplicados), function(i) {
  if (is.na(Players_sin_duplicados[i, "gk_positioning"])) {
    # Si el jugador es GK, se reemplaza con el promedio
    if (grepl("GK", Players_sin_duplicados[i, "player_positions"], ignore.case = TRUE)) {
      return(promedio_gk_positioning)
    } else {
      # Si no es GK, se reemplaza con 0
      return(0)
    }
  } else {
    # Si no es NA, se deja el valor original
    return(Players_sin_duplicados[i, "gk_positioning"])
  }
})
#_____________________

#--------- en contrados se coloca 9999 como indefinido ya que pueda ser que el futbolista tenga contrato indefinido
Players_sin_duplicados$contract_valid_until[is.na(Players_sin_duplicados$contract_valid_until)] <- 9999

#------------------------------poner numero de camisola que no se repita

Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
  
    jersey_numbers_NA = list(team_jersey_number[!is.na(team_jersey_number)])
  ) %>%
  ungroup()


Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
    #
    jersey_numbers_disponibles = ifelse(
      length(jersey_numbers_NA[[1]]) == 0,  
      list(1:99), 
      list(setdiff(1:99, jersey_numbers_NA[[1]])) 
    )
  ) %>%
  ungroup()

# Rellenar los valores NA en team_jersey_number con los números disponibles
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
    team_jersey_number = ifelse(
      is.na(team_jersey_number), 
     
      mapply(function(available_numbers, na_count) {
       
        if (length(available_numbers) < na_count) {
          sample(available_numbers, na_count, replace = TRUE)
        } else {
          sample(available_numbers, na_count, replace = FALSE)
        }
      }, jersey_numbers_disponibles, sum(is.na(team_jersey_number))),
      team_jersey_number
    )
  ) %>%
  ungroup()

#--------------------------

Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
    
    jersey_numbers_NA_nation = list(nation_jersey_number[!is.na(nation_jersey_number)])
  ) %>%
  ungroup()


Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
   
    jersey_numbers_disponibles_nation = ifelse(
      length(jersey_numbers_NA_nation[[1]]) == 0,  # 
      list(1:99),  
      list(setdiff(1:99, jersey_numbers_NA_nation[[1]]))  
    )
  ) %>%
  ungroup()

# Rellenar los valores NA en nation_jersey_number con los números disponibles

Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
    nation_jersey_number = ifelse(
      is.na(nation_jersey_number), 

      mapply(function(available_numbers, na_count) {
   
        if (length(available_numbers) < na_count) {
          sample(available_numbers, na_count, replace = TRUE)
        } else {
          sample(available_numbers, na_count, replace = FALSE)
        }
      }, jersey_numbers_disponibles_nation, sum(is.na(nation_jersey_number))),
      nation_jersey_number
    )
  ) %>%
  ungroup()

#-------------------------- verifica que los datos esten si NA
#Eliminación de columans mutadas
Players_sin_duplicados <- Players_sin_duplicados %>%
  select(-jersey_numbers_NA, -jersey_numbers_disponibles, -jersey_numbers_NA_nation, -jersey_numbers_disponibles_nation)

Players_NA <- colSums(is.na(Players_sin_duplicados))


View(Players_NA)
View(Teams_and_leagues_NA)

#------ verificación de formatos incosistentes
structure_output <- capture.output(str(Players_sin_duplicados))
cat(structure_output, sep = "\n")

#--- cambio de tipo para fecha
Players_sin_duplicados$dob <- as.Date(Players_sin_duplicados$dob, format = "%Y-%m-%d")
str(Players_sin_duplicados$dob)

####---------------------------Data Wrangling----------------------------------------

summary(Players_sin_duplicados)

# - crear nuevas variables
  # edad en 2024
Players_sin_duplicados$Edad_2024 <- as.integer(format(Sys.Date(), "%Y")) - as.integer(format(Players_sin_duplicados$dob, "%Y"))




# -- columna potencial de crecimiento 
Players_sin_duplicados$potencial_crecimento <- Players_sin_duplicados$potential - Players_sin_duplicados$overall

#Crea un índice que combine el valor de mercado y la calificación general del jugador para obtener una medida del "valor relativo" o el rendimiento en relación al costo.

Players_sin_duplicados <- Players_sin_duplicados %>%
  mutate(
    valor_relativo = value_eur / overall
  )

# Promedio de habilidades técnicas del jugador
#Combina las habilidades técnicas del jugador (por ejemplo, dribbling, shooting, passing, etc.) para obtener una métrica general de sus habilidades técnicas.

Players_sin_duplicados <- Players_sin_duplicados %>%
  mutate(
    Promedio_habilidad = rowMeans(select(Players_sin_duplicados, shooting, passing, dribbling), na.rm = TRUE)
  )

#Valor por edad (Value per Age)
#Relaciona el valor de mercado con la edad del jugador para ver cuán eficiente es un jugador en relación a su valor y edad.
Players_sin_duplicados <- Players_sin_duplicados %>%
  mutate(
    valor_por_edad = value_eur / age
  )



Players_sin_duplicados <- Players_sin_duplicados %>%
  mutate(valor_mercado_categoria = case_when(
    value_eur < 300000 ~ "Low",
    value_eur >= 300000 & value_eur < 10000000 ~ "Medium",
    value_eur >= 10000000 ~ "High"
  ))


View(Players_sin_duplicados)




# - convertir ancho y largo
# Convertir las habilidades en un formato largo
Players_sin_duplicados <- Players_sin_duplicados %>% 
  pivot_longer(cols = c("shooting", "passing", "dribbling", "defending", "physic"),
               names_to = "habilidad",
               values_to = "valor")
Players_sin_duplicados_long <- Players_sin_duplicados_long %>%
  select(-skill)



View(Players_sin_duplicados)



##-----------Data Transformation: 


# Seleccionamos solo las columnas numéricas
numeric_columns <- c("age", "height_cm", "weight_kg", "overall", "potential", "value_eur", "wage_eur", 
                     "pace", "gk_diving", "gk_handling", "gk_kicking", "gk_reflexes", "gk_speed", "gk_positioning", 
                     "potencial_crecimento", "valor_relativo", "Promedio_habilidad", "valor_por_edad", "valor", 
                     "valor_mercado_categoria")

# Filtrar solo las columnas numéricas
Players_num <- Players_sin_duplicados %>%
  select(all_of(numeric_columns))

#Normalización: Para normalizar los datos, usaremos la función preProcess() del paquete caret, que permite aplicar técnicas de preprocesamiento como la normalización. Aquí se transforman los datos para que estén en el rango de 0 a 1.

# Normalización de los datos usando preProcess() de caret
preprocess_model <- preProcess(Players_num, method = c("range"))

# Aplicamos la normalización al dataset
Players_normalized <- predict(preprocess_model, Players_num)

# Verifica el resultado
View(Players_normalized)

# Estandarización de los datos (media 0, desviación estándar 1)
preprocess_model_std <- preProcess(Players_num, method = c("center", "scale"))

# Aplicar la estandarización al dataset
Players_standardized <- predict(preprocess_model_std, Players_num)

# Ver los primeros resultados
View(Players_standardized)



# Estadísticas descriptivas de los datos originales
summary(Players_num)

# Estadísticas descriptivas de los datos normalizados
summary(Players_normalized)

# Estadísticas descriptivas de los datos estandarizados
summary(Players_standardized)


