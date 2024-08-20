n <- 1

# TEST: todos los md5sums de los csvs para sucursales son distintos ----
message(n, " --- TEST: todos los md5sums de los csvs para sucursales son distintos")
lista_md5sums <- fs::dir_map("datos/csvs/", function(.x) { fs::dir_map(.x, tools::md5sum)}) |> unlist() 
lista_md5sums <- fs::dir_ls("datos/csvs")[c(14:18)] |> lapply( function(.x) { fs::dir_map(.x, tools::md5sum)}) |> unlist() 
# lista_md5sums2 <- fs::dir_map("datos/csvs/", function(.x) { tools::md5sum(.x |> fs::dir_ls())}) |> unlist() 
archivos <- lista_md5sums |> names()
md5sums <- lapply(names(lista_md5sums), function(.x) {lista_md5sums[[.x]]}) |> unlist()
archivo_md5sum_df <- tibble(archivo = archivos,
                            md5sum = md5sums) 
all(archivo_md5sum_df |> group_by(md5sum) |> mutate(n=n()) |> _$n == 1)

# TEST: sucursales_df - hay sucursales en todas las provincias? ----
message(n, " --- TEST: sucursales_df - hay sucursales en todas las provincias?")
sucursales_df <- feather::read_feather(paste0(datos_path,
                                              "sucursales_df.feather"))
all(sucursales_df |> group_by(fecha) |> distinct(provincia) |> summarise(x=n()) |> _$x == 52) |> message()
n <- n+1

# TEST: sucursales_df - hay sucursales en todas las ccaa y ciudades autónomas? ----
message(n, " --- TEST: sucursales_df - hay sucursales en todas las ccaa y ciudades autónomas?")
sucursales_df <- feather::read_feather(paste0(datos_path,
                                              "sucursales_df.feather"))
all(sucursales_df |> group_by(fecha) |> distinct(ccaa) |> summarise(x=n()) |> _$x == 19)  |> message()
n <- n+1

# TEST: sucursales_df - la variación del total de sucursales nunca es 0? ----
message(n, " --- TEST: sucursales_df - la variación del total de sucursales nunca es 0?")
sucursales_df <- feather::read_feather(paste0(datos_path,
                                              "sucursales_df.feather"))
all(sucursales_df |> group_by(fecha) |> summarise(x=n()) |> mutate(var= x -lag(x)) |> filter(!is.na(var)) |> _$var != 0 ) |> message()
n <- n+1

# TEST: evolucion_sucursales_poblacion_municipio_df: el número total de sucursales es igual al calculado simplemente con sucursales_df? ----
message(n, " --- TEST: evolucion_sucursales_poblacion_municipio_df: el número total de sucursales es igual al calculado simplemente con sucursales_df?")
evolucion_sucursales_poblacion_municipio_df <- feather::read_feather(paste0(datos_path,
                                                                            "evolucion_sucursales_poblacion_municipio_df.feather"))
sucursales_df <- feather::read_feather(paste0(datos_path,
                                              "sucursales_df.feather"))
all(evolucion_sucursales_poblacion_municipio_df |> group_by(fecha) |> summarise(x=sum(n_sucursales)) |> _$x ==
      sucursales_df |> group_by(fecha) |> summarise(x=n()) |> _$x) |> message()
n <- n+1

#TEST: evolucion_sucursales_poblacion_municipio_df: la población total es igual al calculado con población_df? ----
message(n, " --- TEST: evolucion_sucursales_poblacion_municipio_df: la población total es igual al calculado con población_df?")

evolucion_sucursales_poblacion_municipio_df <- feather::read_feather(paste0(datos_path,
                                                                            "evolucion_sucursales_poblacion_municipio_df.feather"))

identical(evolucion_sucursales_poblacion_municipio_df |> group_by(fecha) |> summarise(x=sum(poblacion_total, na.rm=T)) |> filter(month(fecha) == 12) |> _$x,
          poblacion_df |> group_by(fecha) |> summarise(x=sum(poblacion_total, na.rm=T)) |> filter(fecha %in% sucursales_df$fecha) |> filter(month(fecha) == 12) |> _$x) |> message()
n <- n+1


# TEST: el neto de las nuevas cerradas y las nuevas abiertas debe ser igual a la variación trimestral calculada con sucursales_df
message(n, " --- TEST: el neto de las nuevas cerradas y las nuevas abiertas debe ser igual a la variación trimestral calculada con sucursales_df?")


n <- n+1