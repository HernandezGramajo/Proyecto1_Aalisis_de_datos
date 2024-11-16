install.packages("dplyr")
library(dplyr)


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

#verificar duplicados
# Convertir la columna a minúsculas y contar las repeticiones
Players %>%
  mutate(player_url_lower = tolower(sofifa_id)) %>%  # Convertir la columna a minúsculas
  group_by(player_url_lower) %>%  # Agrupar por el valor transformado
  filter(n() > 1) %>%  # Filtrar los duplicados
  count(player_url_lower) %>%  # Contar la cantidad de duplicados
  View()  # M

# Eliminar las filas duplicadas basadas en la columna 'player_url_lower'
Players_sin_duplicados <- Players[!duplicated(Players$sofifa_id), ]

# Ver el dataframe sin duplicados
View(Players_sin_duplicados)

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


set.seed(123)  # Para reproducibilidad
Players_sin_duplicados$mentality_composure[is.na(Players_sin_duplicados$mentality_composure)] <- sample(mentality_composure_sin_NA, mentality_composure_na, replace = TRUE)
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
# Paso 1: Crear una lista de los números de camiseta ocupados por club
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
    # Paso 1: Identificar los números de camiseta ocupados (no NA)
    jersey_numbers_NA = list(team_jersey_number[!is.na(team_jersey_number)])
  ) %>%
  ungroup()

# Paso 2: Crear una lista de los números de camiseta disponibles por club
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
    # Paso 2: Si no hay números ocupados (todos son NA), asignamos un rango completo de números disponibles
    jersey_numbers_disponibles = ifelse(
      length(jersey_numbers_NA[[1]]) == 0,  # Si no hay números ocupados
      list(1:99),  # Rango completo de números disponibles
      list(setdiff(1:99, jersey_numbers_NA[[1]]))  # Eliminar los números ocupados
    )
  ) %>%
  ungroup()

# Paso 3: Rellenar los valores NA en team_jersey_number con los números disponibles
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(club) %>%
  mutate(
    team_jersey_number = ifelse(
      is.na(team_jersey_number), 
      # Verifica si hay suficientes números disponibles para cubrir todos los NA
      mapply(function(available_numbers, na_count) {
        # Si hay menos números disponibles que los NA, usamos reemplazo
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
# Paso 1: Identificar los números de camiseta ocupados en la selección por cada país
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
    # Paso 1: Identificar los números de camiseta ocupados (no NA) por país
    jersey_numbers_NA_nation = list(nation_jersey_number[!is.na(nation_jersey_number)])
  ) %>%
  ungroup()

# Paso 2: Crear una lista de los números de camiseta disponibles por país
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
    # Paso 2: Si no hay números ocupados (todos son NA), asignamos un rango completo de números disponibles
    jersey_numbers_disponibles_nation = ifelse(
      length(jersey_numbers_NA_nation[[1]]) == 0,  # Si no hay números ocupados en la selección
      list(1:99),  # Rango completo de números disponibles
      list(setdiff(1:99, jersey_numbers_NA_nation[[1]]))  # Eliminar los números ocupados
    )
  ) %>%
  ungroup()

# Paso 3: Rellenar los valores NA en nation_jersey_number con los números disponibles
Players_sin_duplicados <- Players_sin_duplicados %>%
  group_by(nationality) %>%
  mutate(
    nation_jersey_number = ifelse(
      is.na(nation_jersey_number), 
      # Verifica si hay suficientes números disponibles para cubrir todos los NA
      mapply(function(available_numbers, na_count) {
        # Si hay menos números disponibles que los NA, usamos reemplazo (replace = TRUE)
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

#--------------------------
Players_NA <- colSums(is.na(Players_sin_duplicados))
Teams_and_leagues_NA <- colSums(is.na(Teams_and_leagues))


View(Players_NA)
View(Teams_and_leagues_NA)

View(Players_sin_duplicados)

#----------- pruebas
# Obtener los valores únicos de la columna 'nationality'

valores_unicos <-  unique(toupper(Players_sin_duplicados$team_jersey_number))

# Ver los valores únicos
valores_unicos


# Ver el resultado
print(df)

gk_players <- Players[grepl("GK", Players$player_positions, ignore.case = TRUE), ]

# Mostrar el resultado
View(gk_players)
