library(tidyverse)

datos_path <- "datos/"

# Códigos INE de provincias ----
# https://www.ine.es/daco/daco42/codmun/cod_ccaa_provincia.htm
codigos_ine_provincia_df <- tibble(
  provincia = c("Almería", "Cádiz", "Córdoba", "Granada", "Huelva", "Jaén", "Málaga", "Sevilla", "Huesca", "Teruel", "Zaragoza","Asturias", "Baleares, Islas","Palmas, Las", "Santa Cruz de Tenerife","Cantabria", "Ávila", "Burgos", "León", "Palencia","Salamanca","Segovia",  "Soria", "Valladolid", "Zamora","Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo", "Barcelona","Gerona","Lérida","Tarragona", "Alicante", "Castellón", "Valencia","Badajoz","Cáceres", "Coruña, La", "Lugo", "Orense", "Pontevedra", "Madrid", "Murcia","Navarra","Álava", "Vizcaya","Guipúzcoa","Rioja, La", "Ceuta", "Melilla"),
  codigo_ccaa  = c("01", "01", "01", "01", "01", "01", "01", "01", "02","02","02", "03", "04", "05","05", "06", "07","07","07","07","07","07","07","07","07", "08", "08", "08", "08", "08", "09", "09", "09", "09", "10", "10", "10", "11", "11", "12", "12", "12", "12", "13", "14", "15", "16", "16", "16", "17", "18", "19"),
  codigo_provincia = c("04", "11", "14", "18", "21", "23", "29", "41", "22", "44", "50", "33", "07", "35", "38", "39", "05", "09", "24", "34", "37", "40", "42", "47", "49", "02", "13", "16", "19", "45", "08", "17", "25", "43", "03", "12", "46", "06", "10", "15", "27", "32", "36", "28", "30", "31", "01", "48", "20", "26", "51", "52")
) |>
  mutate(ccaa = case_when(
    codigo_ccaa == "01" ~ "Andalucía",
    codigo_ccaa == "02" ~ "Aragón",
    codigo_ccaa == "03" ~ "Asturias",
    codigo_ccaa == "04" ~ "Baleares",
    codigo_ccaa == "05" ~ "Canarias",
    codigo_ccaa == "06" ~ "Cantabria",
    codigo_ccaa == "07" ~ "Castilla y León",
    codigo_ccaa == "08" ~ "Castilla-La Mancha",
    codigo_ccaa == "09" ~ "Cataluña",
    codigo_ccaa == "10" ~ "Comunidad Valenciana",
    codigo_ccaa == "11" ~ "Extremadura",
    codigo_ccaa == "12" ~ "Galicia",
    codigo_ccaa == "13" ~ "Comunidad de Madrid",
    codigo_ccaa == "14" ~ "Murcia",
    codigo_ccaa == "15" ~ "Navarra",
    codigo_ccaa == "16" ~ "País Vasco",
    codigo_ccaa == "17" ~ "La Rioja",
    codigo_ccaa == "18" ~ "Ceuta",
    codigo_ccaa == "19" ~ "Melilla"
  )
  )


feather::write_feather(codigos_ine_provincia_df,
                       paste0(datos_path,
                              "codigos_ine_provincia_df.feather"))

# Funciones ----
cargar_directorio_xls_ine_poblacion_municipios <- function(archivo_poblacion_xls_path) {
  message("Cargando: ", archivo_poblacion_xls_path)
  
  año <- stringr::str_extract(archivo_poblacion_xls_path, "[0-9][0-9]") |> 
    paste0("-01-01") |> 
    as.Date("%y-%m-%d") 
  
  message("año: ", año)
  
  
  archivo_poblacion_df <- readxl::read_excel(archivo_poblacion_xls_path, 
                                             skip=0, 
                                             .name_repair = tolower) |>
    mutate(fecha = año)
  
  vars_seleccionar <- c()
  
  if("varones" %in% names(archivo_poblacion_df)) {
    vars_seleccionar <- c(vars_seleccionar, "varones")
  }
  else if("hombres" %in% names(archivo_poblacion_df)) {
    vars_seleccionar <- c(vars_seleccionar, "hombres")
  }
  if("nombre" %in% names(archivo_poblacion_df)) {
    vars_seleccionar <- c(vars_seleccionar, "nombre")
  }
  else if("municipio" %in% names(archivo_poblacion_df)) {
    vars_seleccionar <- c(vars_seleccionar, "municipio")
  }
  
  archivo_poblacion_df <- archivo_poblacion_df |>
    select(fecha, cpro, cmun, !!!vars_seleccionar, mujeres) 
  
  if("varones" %in% names(archivo_poblacion_df)) {
    archivo_poblacion_df <- archivo_poblacion_df |>
      rename(hombres = varones)
  }
  
  if("nombre" %in% names(archivo_poblacion_df)) {
    archivo_poblacion_df <- archivo_poblacion_df |>
      rename(municipio = nombre)
  }
  
  
  return(archivo_poblacion_df |>
           select(fecha, 
                  cpro,
                  cmun,
                  municipio,
                  hombres,
                  mujeres))
}
cargar_directorio_csv_bde <- function(directorio) {
  message(directorio)
  
  sucursales_df <- vroom::vroom(fs::dir_ls(path=directorio, 
                                           glob="*.csv"), 
                                delim=";",
                                locale=locale(encoding="latin1", 
                                              decimal_mark = ","),
                                col_types = list("Cod. Tipo Entidad" = col_character(),
                                                 #"Cód. Tipo Entidad" = col_character(),
                                                 "Cod CCAA" = col_character(),
                                                 "Tipo Entidad" = col_character(),
                                                 "Cód. Entidad" = col_character(),
                                                 "Entidad" = col_character(),
                                                 "Cód. País" = col_character(),
                                                 "País" = col_character(),
                                                 "Cod CCAA" = col_character(),
                                                 "CCAA" = col_character(),
                                                 "Cod. Provincia" = col_character(),
                                                 "Provincia" = col_character(),
                                                 "Cód. Municipio" = col_character(),
                                                 "Municipio/Población" = col_character(),
                                                 "Domicilio" = col_character(),
                                                 "CP" = col_character(),
                                                 "Tipo" = col_character()),
                                col_select = list(codigo_tipo_entidad = "Cod. Tipo Entidad",
                                                  tipo_entidad = "Tipo Entidad",
                                                  codigo_entidad = "Cód. Entidad",
                                                  entidad = "Entidad",
                                                  codigo_pais = "Cód. País",
                                                  pais = "País",
                                                  codigo_ccaa = "Cod CCAA",
                                                  ccaa = "CCAA",
                                                  codigo_provincia = "Cod. Provincia",
                                                  provincia = "Provincia",
                                                  codigo_municipio = "Cód. Municipio",
                                                  municipio = "Municipio/Población",
                                                  domicilio = "Domicilio",
                                                  cp = "CP",
                                                  tipo = "Tipo"))
  
  fecha_directorio = as.Date(str_split(directorio, "/")[[1]][3], format="%Y-%m-%d")
  print(fecha_directorio)
  
  sucursales_df <- sucursales_df %>%
    mutate(fecha = as.Date(fecha_directorio)) %>%
    relocate(fecha) %>%
    filter(!grepl("###", codigo_tipo_entidad) | !grepl("###", codigo_tipo_entidad) | !grepl("Comunidad", codigo_tipo_entidad) | !grepl("Fecha de obtención", codigo_tipo_entidad))
  
  return(sucursales_df)
  
}
cargar_en_vez_de_generar_toggle <- ifelse(
  tolower(
    readline("Cargar datos ya generados? (s/[n]): ")) %in% c("yes", "y", "sí", "si", "s"),
  TRUE,
  FALSE)

if(cargar_en_vez_de_generar_toggle) {
  sucursales_df <- feather::read_feather(paste0(datos_path,
                                                "sucursales_df.feather"))
  
  poblacion_df <- feather::read_feather(paste0(datos_path,
                                               "poblacion_df.feather"))
} else {
  
  ## Sucursales_df -----
  sucursales_df <- dplyr::bind_rows(fs::dir_map(paste0(datos_path, "csv_raw"), # a cada directorio en datos_path/csvs, aplicamos cargar_directorio_csv_bde()
                                                cargar_directorio_csv_bde)) |> 
    filter(!is.na(ccaa),
           !is.na(provincia),
           !is.na(municipio)) |>
    mutate(año = lubridate::floor_date(fecha, "year")) |>
    rename(cmun = codigo_municipio) |> 
    distinct() |>
    mutate(fecha = lubridate::ceiling_date(fecha, "quarter") - days(1)) |>
    mutate(cmun = paste0(codigo_provincia, cmun)) |> 
    group_by(domicilio, municipio, provincia, entidad) |> 
    mutate(sucursal_id = cur_group_id()) |>
    ungroup()
  
  # limpiamos nombres de provincias y CCAA con nuestra propia tabla de nombres asociadas a los respectivos códigos
  sucursales_df <- sucursales_df |>
    select(-ccaa,
           -provincia,
           -codigo_ccaa) |>
    full_join(codigos_ine_provincia_df,
              by=c("codigo_provincia")) |>
    relocate(fecha, provincia, codigo_provincia, ccaa, codigo_ccaa) |>
    # limpiamos nombre de entidad
    mutate(entidad = entidad |> 
             stringr::str_remove("\\([^)]+\\)") |>
             stringr::str_remove(",.*") |>
             stringr::str_to_title() |>
             stringr::str_replace_all("\\bDe\\b"
                                      , "de")
    ) |>
    mutate(entidad = case_when(entidad == "Banco Bilbao Vizcaya Argentaria" ~ "BBVA",
                               entidad == "Ibercaja Banco" ~ "Ibercaja",
                               entidad == "Abanca Corporacion Bancaria" ~ "Abanca",
                               entidad == "Cajamar Caja Rural" ~ "Cajamar",
                               entidad == "Unicaja Banco" ~ "Unicaja",
                               entidad == "Banco Santander" ~ "Santander",
                               entidad == "Banco de Sabadell" ~ "Sabadell",
                               TRUE ~ entidad)) 
  
  # separamos en una tabla para hacerlo más rápido: sólo una vez por cada id de sucursal
  direcciones_sucursales_df <- sucursales_df |>
    select(sucursal_id, domicilio, municipio, provincia, entidad) |>
    distinct(sucursal_id, domicilio, municipio, provincia, entidad) |> 
    mutate(direccion = paste(entidad, domicilio, municipio, sep=" ")) |>
    mutate(direccion = stringr::str_squish(direccion)) |>
    mutate(domicilio = stringr::str_replace(domicilio, "PZ PL", "Plaza ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "PZ ", "Plaza ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "CL ", "Calle ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "AV AV. ", "Avenida ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "PS ", "Paseo ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "PS PO. ", "Paseo ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "Pº ", "Paseo ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "RB ", "Rambla ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "AV ", "Avenida ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "CR CTRA. ", "Carretera ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "CR ", "Carretera ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "CM CAMINO ", "Camino ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "AVDA. ", "Avenida ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, " S/N", " Banco")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "BU. ", "Bulevar ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "TR TRAVESIA ", "Travesía ")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "(OF PRAL)", "")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "(OFIC. PRAL)", "")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "ZZ ", "")) |>
    mutate(domicilio = stringr::str_replace(domicilio, "\\d{5}", "")) |> # eliminamos 5 números seguidos
    mutate(domicilio = stringr::str_replace(domicilio, "\\d{4}", "")) |> # eliminamos 4 números seguidos
    mutate(entidad = stringr::str_replace(entidad, "\\(\\d{4}\\)", "")) |> # eliminamos 5 números seguidos en la entidad
    mutate(entidad = stringr::str_replace(entidad, " SA$", "")) |>
    mutate(entidad = stringr::str_replace(entidad, " S\\.A\\.E\\.", "")) |>
    mutate(entidad = stringr::str_replace(entidad, " S\\.A\\.", "")) |>
    mutate(entidad = stringr::str_replace(entidad, " S\\.C\\.C\\.", "")) |>
    mutate(entidad = stringr::str_replace(entidad, ", E\\.F\\.C", "")) |>
    mutate(entidad = stringr::str_replace(entidad, " E\\.F\\.C", "")) |>
    mutate(entidad = stringr::str_replace(entidad, ",E\\.F\\.C", "")) |>
    mutate(entidad = stringr::str_replace(entidad, ",SCAC", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "S\\\\.E$", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "S\\.C\\.V\\.C", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "\\,SCCV", "")) |>
    mutate(entidad = stringr::str_replace(entidad, " SCC", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "\\,SCVV", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "s\\.C\\.C\\.V", "")) |>
    mutate(entidad = stringr::str_replace(entidad, "\\,V", "")) |>
    mutate(entidad = stringr::str_replace(entidad, ", ", "")) |> 
    mutate(entidad = stringr::str_replace(entidad, "BANCO BILBAO VIZCAYA ARGENTARIASA ", "BBVA")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,CCV$", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,SCA$", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,SCA $", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,SCC $", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,EFC$", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\,SC$", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "\\, S.L. $", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, " N.V.. $", "")) |> 
    mutate(entidad=stringr::str_replace(entidad, "EFC $", "")) |> 
    mutate(domicilio = stringr::str_remove(domicilio, " 0$")) |>
    mutate(domicilio = stringr::str_remove(domicilio, " Banco$")) |>
    mutate(direccion = paste(entidad, ",", domicilio, ",", municipio, ",", provincia, sep=" ")) |>
    mutate(direccion = stringr::str_squish(direccion)) |>
    mutate(direccion = paste0("Banco ", direccion)) |>
    mutate(direccion = paste0(direccion, ", España"))
  
  sucursales_df <- sucursales_df |> 
    right_join(direcciones_sucursales_df |> select(sucursal_id, direccion), by="sucursal_id")
  
  feather::write_feather(x=sucursales_df, path=paste0(datos_path, "sucursales_df.feather"))
  
  ## Población_df -----
  poblacion_df <- dplyr::bind_rows(fs::dir_map(paste0(datos_path, "poblacion"),
                                               cargar_directorio_xls_ine_poblacion_municipios)) |>
    filter(!grepl("Total", "PROVINCIA")) |>
    mutate(poblacion_total = hombres + mujeres) |>
    distinct() |>
    arrange(fecha) |>
    mutate(fecha = lubridate::ceiling_date(fecha, "year") - days(1)) |>
    mutate(cmun = paste0(cpro, cmun)) |>
    rename(codigo_provincia = cpro) |> full_join(codigos_ine_provincia_df, by=c("codigo_provincia")) |>
    # a partir de aquí, extendemos el df para los trimestres entre cada trimestre final del año
    group_by(cmun) |>
    complete(fecha = (seq.Date(min(fecha), max(fecha), by="3 month") - days(1)) |> lubridate::ceiling_date("month") - days(1)) |>
    filter(fecha >= min(sucursales_df$fecha)) |>
    fill(codigo_provincia, municipio, hombres, mujeres, poblacion_total, provincia, codigo_ccaa, ccaa, .direction="up") |>
    ungroup() |>
    relocate(fecha, municipio, cmun, provincia, codigo_provincia, ccaa, codigo_ccaa)    
  
  feather::write_feather(x=poblacion_df, path=paste0(datos_path, "poblacion_df.feather"))
}

### sucursales_todos_municipios_df -----
sucursales_todos_municipios_df <- sucursales_df |> 
  full_join(poblacion_df |> select(fecha, cmun, poblacion_total, hombres, mujeres),
            by=c("fecha", "cmun")) |>
  group_by(fecha, cmun) |>
  mutate(
    n_sucursales_municipio = sum(!is.na(entidad), na.rm=T)
  ) |>
  group_by(fecha, entidad) |>
  mutate(
    n_sucursales_entidad = sum(!is.na(entidad),na.rm=T)
  )

feather::write_feather(
  sucursales_todos_municipios_df,
  paste0(
    datos_path,
    "sucursales_todos_municipios_df.feather"
  )
)


# Limpiamos direcciones de las sucursales para que la API de google maps entienda el máximo % posible
domicilios_sucursales_ultimo_mes_df <- sucursales_df |>
  filter(fecha == max(fecha)) |>
  distinct(sucursal_id, domicilio, municipio, provincia, entidad) |> 
  mutate(direccion = paste(entidad, domicilio, municipio, sep=" ")) |>
  mutate(direccion = stringr::str_squish(direccion)) |>
  mutate(domicilio = stringr::str_replace(domicilio, "PZ PL", "Plaza ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "PZ ", "Plaza ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "CL ", "Calle ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "AV AV. ", "Avenida ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "PS ", "Paseo ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "PS PO. ", "Paseo ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "Pº ", "Paseo ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "RB ", "Rambla ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "AV ", "Avenida ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "CR CTRA. ", "Carretera ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "CR ", "Carretera ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "CM CAMINO ", "Camino ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "AVDA. ", "Avenida ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, " S/N", " Banco")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "BU. ", "Bulevar ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "TR TRAVESIA ", "Travesía ")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "(OF PRAL)", "")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "(OFIC. PRAL)", "")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "ZZ ", "")) |>
  mutate(domicilio = stringr::str_replace(domicilio, "\\d{5}", "")) |> # eliminamos 5 números seguidos
  mutate(domicilio = stringr::str_replace(domicilio, "\\d{4}", "")) |> # eliminamos 4 números seguidos
  mutate(entidad = stringr::str_replace(entidad, "\\(\\d{4}\\)", "")) |> # eliminamos 5 números seguidos en la entidad
  mutate(entidad = stringr::str_replace(entidad, " SA$", "")) |>
  mutate(entidad = stringr::str_replace(entidad, " S\\.A\\.E\\.", "")) |>
  mutate(entidad = stringr::str_replace(entidad, " S\\.A\\.", "")) |>
  mutate(entidad = stringr::str_replace(entidad, " S\\.C\\.C\\.", "")) |>
  mutate(entidad = stringr::str_replace(entidad, ", E\\.F\\.C", "")) |>
  mutate(entidad = stringr::str_replace(entidad, " E\\.F\\.C", "")) |>
  mutate(entidad = stringr::str_replace(entidad, ",E\\.F\\.C", "")) |>
  mutate(entidad = stringr::str_replace(entidad, ",SCAC", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "S\\\\.E$", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "S\\.C\\.V\\.C", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "\\,SCCV", "")) |>
  mutate(entidad = stringr::str_replace(entidad, " SCC", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "\\,SCVV", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "s\\.C\\.C\\.V", "")) |>
  mutate(entidad = stringr::str_replace(entidad, "\\,V", "")) |>
  mutate(entidad = stringr::str_replace(entidad, ", ", "")) |> 
  mutate(entidad = stringr::str_replace(entidad, "BANCO BILBAO VIZCAYA ARGENTARIASA ", "BBVA")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,CCV$", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,SCA$", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,SCA $", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,SCC $", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,EFC$", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\,SC$", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "\\, S.L. $", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, " N.V.. $", "")) |> 
  mutate(entidad=stringr::str_replace(entidad, "EFC $", "")) |> 
  mutate(domicilio = stringr::str_remove(domicilio, " 0$")) |>
  mutate(domicilio = stringr::str_remove(domicilio, " Banco$")) |>
  mutate(direccion = paste(entidad, ",", domicilio, ",", municipio, ",", provincia, sep=" ")) |>
  mutate(direccion = stringr::str_squish(direccion))

feather::write_feather(domicilios_sucursales_ultimo_mes_df, 
                       path=paste0(datos_path,
                                   "domicilios_sucursales_ultimo_mes_df.feather"))

domicilios_sucursales_desaparecidas_df <- sucursales_df |>
  filter(!sucursal_id %in% domicilios_sucursales_ultimo_mes_df$sucursal_id) |>
  distinct(sucursal_id, domicilio, municipio, provincia, entidad) |> 
  mutate(direccion = paste(entidad, domicilio, municipio, sep=" ")) |>
  mutate(direccion = stringr::str_squish(direccion))

feather::write_feather(domicilios_sucursales_desaparecidas_df, 
                       path=paste0(datos_path,
                                   "domicilios_sucursales_desaparecidas_df.feather"))

# Datos geográficos de municipios del IGN ----
## Provincias -----
peninsula_sf <-  sf::st_read(paste0(datos_path,"shapefiles/SHP_ETRS89/recintos_provinciales_inspire_peninbal_etrs89"))
canarias_sf <- sf::st_read(paste0(datos_path, "shapefiles/SHP_REGCAN95/recintos_provinciales_inspire_canarias_regcan95"))

crs <- sf::st_crs(peninsula_sf) #CRS Península, Baleares, Ceuta y Melilla

canarias_modified_sf <- canarias_sf %>% 
  sf::st_transform(crs) %>% #Transformamos las geometrías de Canarias
  sf::st_geometry() + #Extraemos las geometrías.
  c(5, 7) #Movemos Canarias

canarias_sf <- canarias_sf %>% 
  sf::st_set_geometry(canarias_modified_sf) %>%  #Cambiamos las geometrías de Canarias
  sf::st_set_crs(crs) #Volvemos a especificar el CRS de la Península para Canarias.

provincias_sf <- rbind(peninsula_sf, canarias_sf) #Unimos

# Simplificamos los límites provinciales.
provincias_sf <- rmapshaper::ms_simplify(provincias_sf, keep = 0.00050, weighting = 6)

provincias_sf <- provincias_sf |> 
  mutate(NAMEUNIT = case_when(
    NAMEUNIT == "Alacant/Alicante" ~ "Alicante",
    NAMEUNIT == "Illes Balears" ~ "Baleares, Islas",
    NAMEUNIT == "Castelló/Castellón" ~ "Castellón",
    NAMEUNIT == "A Coruña" ~ "Coruña, La",
    NAMEUNIT == "Girona" ~ "Gerona",
    NAMEUNIT == "Gipuzkoa" ~ "Guipúzcoa",
    NAMEUNIT == "Lleida" ~ "Lérida",
    NAMEUNIT == "La Rioja" ~ "Rioja, La",
    NAMEUNIT == "Ourense" ~ "Orense",
    NAMEUNIT == "València/Valencia" ~ "Valencia",
    NAMEUNIT == "Bizkaia" ~ "Vizcaya",
    NAMEUNIT == "Araba/Álava" ~ "Álava",
    NAMEUNIT == "Las Palmas" ~ "Palmas, Las",
    TRUE ~ NAMEUNIT)) |> 
  mutate(codigo_provincia = stringr::str_sub(INSPIREID, 18, 19)) |> 
  relocate(codigo_provincia)

tryCatch({
  sf::st_write(provincias_sf,
               paste0(datos_path, "shapefiles/provincias_sf.shp"),
               delete_dsn=F)
},
error = function(e) message("Cannot write provincias_sf: ",e ))

## CCAA ----
peninsula_sf <-  sf::st_read(paste0(datos_path,"shapefiles/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89"))
canarias_sf <- sf::st_read(paste0(datos_path, "shapefiles/SHP_REGCAN95/recintos_autonomicas_inspire_canarias_regcan95"))

crs <- sf::st_crs(peninsula_sf) #CRS Península, Baleares, Ceuta y Melilla

canarias_modified_sf <- canarias_sf %>% 
  sf::st_transform(crs) %>% #Transformamos las geometrías de Canarias
  sf::st_geometry() + #Extraemos las geometrías.
  c(5, 7) #Movemos Canarias


canarias_sf <- canarias_sf %>% 
  sf::st_set_geometry(canarias_modified_sf) %>%  #Cambiamos las geometrías de Canarias
  sf::st_set_crs(crs) #Volvemos a especificar el CRS de la Península para Canarias.

ccaa_sf <- rbind(peninsula_sf, canarias_sf) #Unimos

# Simplificamos los límites provinciales.
ccaa_sf <- rmapshaper::ms_simplify(ccaa_sf, keep = 0.00050, weighting = 6)

ccaa_sf <- ccaa_sf |> 
  mutate(NAMEUNIT = case_when(
    NAMEUNIT == "Principado de Asturias" ~ "Asturias",
    NAMEUNIT == "Illes Balears" ~ "Baleares",
    NAMEUNIT == "Cataluña/Catalunya" ~ "Cataluña",
    NAMEUNIT == "Comunitat Valenciana" ~ "Comunidad Valenciana",
    NAMEUNIT == "Región de Murcia" ~ "Murcia",
    NAMEUNIT == "Comunidad Foral de Navarra" ~ "Navarra",
    NAMEUNIT == "País Vasco/Euskadi" ~ "País Vasco",
    NAMEUNIT == "Ciudad Autónoma de Ceuta" ~ "Ceuta",
    NAMEUNIT == "Ciudad Autónoma de Ceuta" ~ "Ceuta",
    NAMEUNIT == "Ciudad Autónoma de Melilla" ~ "Melilla",
    TRUE ~ NAMEUNIT)
  ) |> 
  mutate(codigo_ccaa = stringr::str_sub(INSPIREID, 16,17)) |> 
  relocate(codigo_ccaa)

tryCatch({
sf::st_write(ccaa_sf,
             paste0(datos_path, "shapefiles/ccaa_sf.shp"),
             overwrite=T)
},
error = function(e) message("Cannot write provincias_sf: ",e ))

## Municipios ----
peninsula_sf <-  sf::st_read(paste0(datos_path,"shapefiles/SHP_ETRS89/recintos_municipales_inspire_peninbal_etrs89"))
canarias_sf <- sf::st_read(paste0(datos_path, "shapefiles/SHP_REGCAN95/recintos_municipales_inspire_canarias_regcan95"))

crs <- sf::st_crs(peninsula_sf) #CRS Península, Baleares, Ceuta y Melilla

canarias_modified_sf <- canarias_sf %>% 
  sf::st_transform(crs) %>% #Transformamos las geometrías de Canarias
  sf::st_geometry() + #Extraemos las geometrías.
  c(5, 7) #Movemos Canarias


canarias_sf <- canarias_sf %>% 
  sf::st_set_geometry(canarias_modified_sf) %>%  #Cambiamos las geometrías de Canarias
  sf::st_set_crs(crs) #Volvemos a especificar el CRS de la Península para Canarias.

municipios_sf <- rbind(peninsula_sf, canarias_sf) #Unimos


# Simplificamos los límites provinciales.
municipios_sf <- rmapshaper::ms_simplify(municipios_sf, keep = 0.00050, weighting = 6)

municipios_sf <- municipios_sf |> 
mutate(cmun = stringr::str_sub(INSPIREID, 20, 24)) |>
  relocate(cmun)

tryCatch({
sf::st_write(municipios_sf,
             paste0(datos_path, "shapefiles/municipios_sf.shp"),
             overwrite=T)
},
error = function(e) message("Cannot write provincias_sf: ",e ))

# Descargado de: https://centrodedescargas.cnig.es/CentroDescargas/catalogo.do?Serie=NGBES
# Nomenclátor Geográfico de Municipios y Entidades de Población
entidades_ign <- readr::read_csv2(paste0(datos_path, "ign/ENTIDADES.csv"),
                                  skip=1,
                                  col_names=c("codigo_ine", 
                                              "nombre", 
                                              "cod_prov",
                                              "provincia",
                                              "tipo",
                                              "poblacion",
                                              "cmun",
                                              "hoja_mtn25",
                                              "longitud_etrs89",
                                              "latitud_Etrs89",
                                              "origencoor",
                                              "altitud",
                                              "origenaltitud",
                                              "suprimida_ine",
                                              "discrepante_ine"),
                                  col_types=c("cccccdccddcdcc"),
                                  locale=readr::locale("es", encoding="latin1")) |>
  group_by(nombre) |>
  # filtramos observaciones de una misma entidad para tomar aquellas con la mayor cifra de población,
  # y dentro de estas, la primera que aparezca. De esta forma eliminamos observaciones con subconjuntos de la población
  # que expresan conceptos como población dispersa y similares.
  filter(poblacion == max(poblacion)) |> 
  filter(codigo_ine == first(codigo_ine)) |>
  group_by(codigo_ine) |>
  distinct() |>
  ungroup() |>
  # corregimos entidades con datos de altitud malformados
  mutate(altitud = case_when(altitud > 3000 ~ NA,
                             TRUE ~ altitud))

feather::write_feather(entidades_ign, paste0(datos_path,
                                             "entidades_ign.feather"))

municipios_ign <- readr::read_csv2(paste0(datos_path, "ign/MUNICIPIOS.csv"),
                                   skip=1,
                                   col_names=c("cmun",
                                               "id_rel",
                                               "cod_prov",
                                               "provincia",
                                               "municipio",
                                               "poblacion",
                                               "superficie",
                                               "perimetro",
                                               "cod_ine_capital",
                                               "capital",
                                               "poblacion_capital",
                                               "hoja_mtn25_etrs89",
                                               "longitud",
                                               "latitud",
                                               "origencoor",
                                               "altitud",
                                               "origenaltitud"),
                                   col_types=c("cdcccdddccdccccdc"),
                                   # col_types=c("cccccdccddccc"),
                                   locale=readr::locale("es", encoding="latin1")) |>
  mutate(cmun = stringr::str_sub(cmun,1,5))

feather::write_feather(municipios_ign, paste0(datos_path,
                                              "municipios_ign.feather"))

## Centroides de municipios ----

centroides_municipios_df <- mapSpain::esp_get_capimun(
  moveCAN = FALSE # so that we apply our own lat+lon displacement to north-east 
) |>
  # filter(codauto == codigos_ine_provincia_df |> filter(ccaa == selected_ccaa) |> _$codigo_ccaa |> unique()) |>
  mutate(cmun = LAU_CODE) |>
  rename(codigo_ccaa = codauto)

centroides_municipios_df <- centroides_municipios_df |>
  mutate(
    lon = sf::st_coordinates(centroides_municipios_df)[,1],
    lat = sf::st_coordinates(centroides_municipios_df)[,2]
  ) |>
  mutate(
    lon = case_when(
      codigo_ccaa == "05" ~ lon + 5,
      TRUE ~ lon
    ),
    lat= case_when(
      codigo_ccaa == "05" ~ lat + 7,
      TRUE ~ lat
    )
  ) |> 
  as_tibble() |>
  select(-geometry, -LAU_CODE)


feather::write_feather(
  centroides_municipios_df,
  paste0(
    datos_path,
    "shapefiles/",
    "centroides_municipios_df.feather"
  )
)

# Secciones censales ----

## Población por sexo y edad ----
poblacion_seccion_censal_long_df <- readr::read_delim(
  paste0(
    datos_path,
    "secciones_censales_raw/",
    "poblacion_seccion_censal_2023.csv"
  ),
  delim =";",
  name_repair = tolower
) |>
  rename(
    grupo_edad = `edad (grupos quinquenales)`,
  ) |>
  filter(
    !is.na(secciones),
    sexo == "Ambos sexos",
    grupo_edad != "Total") |>
  # rowwise() |> 
  separate(
    municipios, c("cmun", "municipio"), extra="merge"
  ) |>
  separate(
    secciones, c("seccion_censal", "municipio_seccion"), extra="merge"
  ) |>
  select(
    # -Municipios, 
    # -Secciones, 
    # -Distritos, 
    -Periodo, 
    municipio_seccion
    ) |>
  # rowwise() |>
  mutate(
    # cmun = stringr::str_split(municipios, " ")[[1]][[1]],
    # municipio = stringr::str_split(municipios, " ")[[1]][[2]],
    # seccion_censal = stringr::str_split(secciones, " ")[[1]][[1]],
    # codigo_provincia = stringr::str_sub(provincias, 1,2),
    edad_maxima_grupo = case_when(
      stringr::str_detect(grupo_edad, "De") ~ stringr::str_split(grupo_edad, " ")[[1]][[4]] |> as.numeric(),
      TRUE ~ 100),
    valores = stringr::str_remove(total, ",") |> as.numeric()
  ) |> 
  group_by(seccion_censal) |>
  mutate(poblacion_total = sum(valores, na.rm=T))  |>
  ungroup()  |>
  select(-municipios, -secciones, -provincias, -`total nacional`, -sexo) |>
  group_by(seccion_censal) |>
  mutate(poblacion_mayor_umbral = sum(valores[edad_maxima_grupo > 64], na.rm=T)) |>
  ungroup()

poblacion_seccion_censal_df <- poblacion_seccion_censal_long_df |>
  pivot_wider(
    id_cols = c(
      "seccion_censal",
      "cmun",
      "municipio",
      "codigo_provincia",
      "poblacion_total",
      "poblacion_mayor_umbral"
      ),
    names_from="grupo_edad",
    values_from="valores"
  ) |>
  rename(
    years_0_4 = `De 0 a 4 años`,
    years_5_9 = `De 5 a 9 años`,
    years_10_14 = `De 10 a 14 años`,
    years_15_19 = `De 15 a 19 años`,
    years_20_24 = `De 20 a 24 años`,
    years_25_29 = `De 25 a 29 años`,
    years_30_34 = `De 30 a 34 años`,
    years_35_39 = `De 35 a 39 años`,
    years_40_44 = `De 40 a 44 años`,
    years_45_49 = `De 45 a 49 años`,
    years_50_54 = `De 50 a 54 años`,
    years_55_59 = `De 55 a 59 años`,
    years_60_64 = `De 60 a 64 años`,
    years_65_69 = `De 65 a 69 años`,
    years_70_74 = `De 70 a 74 años`,
    years_75_79 = `De 75 a 79 años`,
    years_80_84 = `De 80 a 84 años`,
    years_85_89 = `De 85 a 89 años`,
    years_90_94 = `De 90 a 94 años`,
    years_95_99 = `De 95 a 99 años`,
    years_100_mas = `100 y más años`,
  ) |>
  select(-municipio) |>
  full_join(
    poblacion_df |> 
      select(municipio, cmun) |>
      distinct(),
    by="cmun"
  )
  

feather::write_feather(poblacion_seccion_censal_df,
                       path = paste0(datos_path, "poblacion_seccion_censal_df.feather"))


## Renta ----

renta_seccion_censal_long_df <- readr::read_delim(
  paste0(
    datos_path,
    "secciones_censales_raw/",
    "renta_seccion_censal.csv"
  ),
  delim =";"
) |>
  rename(nombres = `Indicadores de renta media`,
         valores = Total)  |>
  mutate(
    fecha = as.numeric(Periodo)
  ) |>
  filter(
    fecha == max(as.numeric(fecha)),
    !is.na(Secciones)) |> 
  separate(
    Municipios, c("cmun", "municipio"), extra="merge"
  ) |>
  separate(
    Secciones, c("seccion_censal", "municipio_seccion"), extra="merge"
  ) |>
  select(
    # -Municipios, 
    # -Secciones, 
    -Distritos, 
    -Periodo, 
    municipio_seccion
    ) |>
  mutate(valores = stringr::str_remove(valores, "\\.") |> as.numeric())
  
renta_seccion_censal_long_df <- renta_seccion_censal_long_df |>
  full_join(
    tibble(
     nombres=renta_seccion_censal_long_df |> _$nombres |> unique(), 
     nuevos_nombres = c(
       "renta_neta_media_persona",
       "renta_neta_media_hogar",
       "renta_media_unidadconsumo",
       "renta_mediana_unidadconsumo",
       "renta_bruta_media_persona",
       "renta_bruta_media_hogar"
     )
    ),
    by="nombres"
  )

renta_seccion_censal_df <- renta_seccion_censal_long_df |>
  select(-fecha) |>
  filter(!is.na(valores)) |>
  select(-nombres) |>
  pivot_wider(
    id_cols = c("seccion_censal", "cmun", "municipio"),
    names_from="nuevos_nombres",
    values_from="valores"
  ) |>
  select(-municipio) |>
  full_join(
    poblacion_df |> 
      select(municipio, cmun) |>
      distinct(),
    by="cmun"
  )

feather::write_feather(renta_seccion_censal_df,
                       path = paste0(datos_path, "renta_seccion_censal_df.feather"))

## Datos de sección censal: renta+poblacion -----

variable_renta_seleccionada <- "Renta neta media por persona "

datos_secciones_censales_df <- full_join(
  feather::read_feather(paste0(datos_path, "renta_seccion_censal_df.feather")),
  feather::read_feather(paste0(datos_path, "poblacion_seccion_censal_df.feather")),
  by=c("seccion_censal", "cmun", "municipio")
) |>
  mutate(prc_mayor_umbral = poblacion_mayor_umbral / poblacion_total) |>
  full_join(codigos_ine_provincia_df |> select(provincia, codigo_provincia), by="codigo_provincia") |>
  relocate(
    seccion_censal,
    cmun,
    municipio,
    codigo_provincia,
    provincia
  )

feather::write_feather(
  datos_secciones_censales_df,
  paste0(
    datos_path,
    "datos_secciones_censales_df.feather"
  )
)

## Shapefiles de secciones censales - Calcular centroides ----

# Shapefiles con secciones censales y datos asociados -----
## Cargar datos necesarios -----



sucursales_202312_df <- feather::read_feather(
  paste0(
    datos_path,
    "geocoded_clean/",
    "sucursales_geocoded_202312_df.feather"
  )
) |> 
  filter(!is.na(lat) & !is.na(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs="WGS84") 

sucursales_ultimo_trimestre_sf <- sucursales_202312_df
 
## secciones_censales_shapefile -- solo provincias seleccionadas ----- 

provincias_secciones_censales <- c(
  "28", # Madrid
  "08", # Barcelona
  "46", # Valencia/Valéncia
  "41", # Sevilla
  "29", # Málaga
  "50", # Zaragoza
  "48" # Vizcaya
)

secciones_censales_shapefile <- sf::st_read(
  paste0(
    datos_path,
    "shapefiles/secciones_censales/SECC_CE_20240101.shp"
  )
) |>
  filter(CPRO %in% provincias_secciones_censales) |>
  # filter(CPRO %in% mayores_municipios$codigo_provincia) |>
  select(-NPRO) |> 
  left_join(codigos_ine_provincia_df |> select(codigo_provincia,  provincia) |> 
              rename(NPRO=provincia, CPRO=codigo_provincia), 
            by="CPRO") |>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  full_join(
    # datos_secciones_censales_df |>
    feather::read_feather(
      paste0(
        datos_path,
        "datos_secciones_censales_df.feather"
      )
    )|>
      # filter(codigo_provincia %in% provincias_secciones_censales) |>
  filter(codigo_provincia %in% mayores_municipios$codigo_provincia) |>
      rename(CUSEC=seccion_censal),
      # select(
        # CUSEC, 
        # prc_mayor_umbral
        # n_sucursales, 
        # renta
        # ), 
    by="CUSEC") |>
  filter(!is.na(NPRO))


# necesario para evitar un error en el cálculo de intersecciones con sf::st_intersects()
sf::sf_use_s2(FALSE)

secciones_censales_shapefile$n_sucursales <- sf::st_intersects(
  secciones_censales_shapefile,
  sucursales_ultimo_trimestre_sf 
) |> lengths()

secciones_censales_shapefile$area_m2 <- sf::st_area(secciones_censales_shapefile)


### centroides  ----

secciones_censales_centroides_sf <- secciones_censales_shapefile |>
  select(
    CUSEC,
    geometry,
    NPRO
  ) |>
  sf::st_centroid() 

secciones_censales_centroides_df <- secciones_censales_centroides_sf %>% 
  mutate(
    lon_centroide = sf::st_coordinates(.)[,1],
    lat_centroide = sf::st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(CUSEC,NPRO,lon_centroide, lat_centroide)


### calcular distancias de sucursales ----

message("Calculando distancias mínimas entre centroides de secciones censales y sucursales...")
asignar_cero_si_sucursal_en_seccion_censal <- FALSE

distancias_df <- lapply(
  
  X=codigos_ine_provincia_df |> 
    filter(codigo_provincia %in% provincias_secciones_censales) |> 
    _$provincia,
  
  FUN=function(.provincia) {
    
    secciones_censales_centroides_sf_provincia <- secciones_censales_centroides_sf |>
      filter(NPRO == .provincia)
    sucursales_ultimo_trimestre_sf_provincia <- sucursales_ultimo_trimestre_sf |> filter(provincia == .provincia)
    
    message("Procesando provincia: ", secciones_censales_centroides_sf_provincia$NPRO |> unique())
    message("Número de sucursales: ", nrow(sucursales_ultimo_trimestre_sf_provincia))
    message("Número de secciones censales: ", nrow(secciones_censales_centroides_sf_provincia))
    
    distancias <- sf::st_distance(
      secciones_censales_centroides_sf_provincia,
      sucursales_ultimo_trimestre_sf_provincia
    )
    
    # Calculamos mínimo de cada fila de la matrix. 
    # Cada fila contiene las distancias de una sección sensal dada, respecto a todas las sucursales
    distancias_minimas <- apply(
      X=distancias,
      MARGIN=1, # rows
      FUN=min 
    )
    
    distancias_minimas_df <- tibble(
     CUSEC = secciones_censales_centroides_sf_provincia$CUSEC,
     distancia_minima_sucursal_m = distancias_minimas
    )  
    
    return(distancias_minimas_df)
  }
) |> bind_rows()


secciones_censales_shapefile <- secciones_censales_shapefile |>
  full_join(
    distancias_df,
    by="CUSEC"
  )

## secciones_censales_mayores_municipios_shapefile -----

n_mayores_municipios <- 30

mayores_municipios <- poblacion_df |>
  filter(fecha == max(fecha)) |>
  arrange(poblacion_total) |>
  tail(n_mayores_municipios) |>
  select(municipio, cmun, codigo_provincia)
 
## secciones_censales_mayores_municipios_shapefile -- solo provincias seleccionadas ----- 
secciones_censales_mayores_municipios_shapefile <- sf::st_read(
  paste0(
    datos_path,
    "shapefiles/secciones_censales/SECC_CE_20240101.shp"
  )
) |>
  filter(CUMUN %in% mayores_municipios$cmun) |>
  select(-NPRO) |> 
  left_join(codigos_ine_provincia_df |> select(codigo_provincia,  provincia) |> 
              rename(NPRO=provincia, CPRO=codigo_provincia), 
            by="CPRO") |>
  sf::st_transform('+proj=longlat +datum=WGS84') |>
  full_join(
    # datos_secciones_censales_df |>
    feather::read_feather(
      paste0(
        datos_path,
        "datos_secciones_censales_df.feather"
      )
    )|>
  filter(codigo_provincia %in% mayores_municipios$codigo_provincia) |>
      rename(CUSEC=seccion_censal),
      # select(
        # CUSEC, 
        # prc_mayor_umbral
        # n_sucursales, 
        # renta
        # ), 
    by="CUSEC") |>
  filter(!is.na(NPRO))

# necesario para evitar un error en el cálculo de intersecciones con sf::st_intersects()
sf::sf_use_s2(FALSE)

secciones_censales_mayores_municipios_shapefile$n_sucursales <- sf::st_intersects(
  secciones_censales_mayores_municipios_shapefile,
  sucursales_ultimo_trimestre_sf 
) |> lengths()

secciones_censales_mayores_municipios_shapefile$area_m2 <- sf::st_area(secciones_censales_mayores_municipios_shapefile)



### centroides  ----

secciones_censales_mayores_municipios_centroides_sf <- secciones_censales_mayores_municipios_shapefile |>
  select(
    CUSEC,
    geometry,
    NPRO
  ) |>
  sf::st_centroid() 



secciones_censales_mayores_municipios_centroides_df <- secciones_censales_mayores_municipios_centroides_sf %>% 
  mutate(
    lon_centroide = sf::st_coordinates(.)[,1],
    lat_centroide = sf::st_coordinates(.)[,2]
  ) |>
  as_tibble() |>
  select(CUSEC,NPRO,lon_centroide, lat_centroide)


### calcular distancias de sucursales ----

message("Calculando distancias mínimas entre centroides de secciones censales y sucursales...")
asignar_cero_si_sucursal_en_seccion_censal <- FALSE

distancias_df <- lapply(
  # X=codigos_ine_provincia_df |> 
  #   filter(codigo_provincia %in% provincias_secciones_censales) |> 
  #   _$provincia,
  X=secciones_censales_mayores_municipios_shapefile |>
    _$NPRO |>
    unique(),
  FUN=function(.provincia) {
    
    secciones_censales_mayores_municipios_centroides_sf_provincia <- secciones_censales_mayores_municipios_centroides_sf |>
      filter(NPRO == .provincia)
    sucursales_ultimo_trimestre_sf_provincia <- sucursales_ultimo_trimestre_sf |> filter(provincia == .provincia)
    
    message("Procesando provincia: ", secciones_censales_mayores_municipios_centroides_sf_provincia$NPRO |> unique())
    message("Número de sucursales: ", nrow(sucursales_ultimo_trimestre_sf_provincia))
    message("Número de secciones censales: ", nrow(secciones_censales_mayores_municipios_centroides_sf_provincia))
    
    distancias <- sf::st_distance(
      secciones_censales_mayores_municipios_centroides_sf_provincia,
      sucursales_ultimo_trimestre_sf_provincia
    )
    
    # Calculamos mínimo de cada fila de la matrix. 
    # Cada fila contiene las distancias de una sección sensal dada, respecto a todas las sucursales
    distancias_minimas <- apply(
      X=distancias,
      MARGIN=1, # rows
      FUN=min 
    )
    
    distancias_minimas_df <- tibble(
     CUSEC = secciones_censales_mayores_municipios_centroides_sf_provincia$CUSEC,
     distancia_minima_sucursal_m = distancias_minimas
    )  
    
    return(distancias_minimas_df)
  }
) |> bind_rows()


secciones_censales_mayores_municipios_shapefile <- secciones_censales_mayores_municipios_shapefile |>
  full_join(
    distancias_df,
    by="CUSEC"
  )


# guardar GEOJSON -----
## secciones_censales_shapefile -----
secciones_censales_shapefile |> 
  select(
    -CUMUN,
    -CMUN
  ) |>
  filter(!is.na(CUSEC)) |>
  # distinct() |>
  sf::st_write(
    paste0(
      datos_path,
      "geojson/",
      "secciones_censales_sf.geojson"
    ),
    # append = FALSE 
  )

feather::write_feather(
  secciones_censales_centroides_df,
  paste0(
    datos_path,
    "secciones_censales_centroides_df.feather"
  )
)

sf::st_write(
  secciones_censales_centroides_sf,
  paste0(
    datos_path, 
    "geojson/secciones_censales_centroides_sf.geojson"
  ),
  append=FALSE
)

## secciones_censales_mayores_municipios_shapefile -----
secciones_censales_mayores_municipios_shapefile |> 
  select(
    -CUMUN,
    -CMUN
  ) |>
  filter(!is.na(CUSEC)) |>
  # distinct() |>
  sf::st_write(
    paste0(
      datos_path,
      "geojson/",
      "secciones_censales_mayores_municipios_sf.geojson"
    ),
    # append = FALSE 
  )

feather::write_feather(
  secciones_censales_mayores_municipios_centroides_df,
  paste0(
    datos_path,
    "secciones_censales_mayores_municipios_centroides_df.feather"
  )
)

sf::st_write(
  secciones_censales_mayores_municipios_centroides_sf,
  paste0(
    datos_path, 
    "geojson/secciones_censales_mayores_municipios_centroides_sf.geojson"
  ),
  append=FALSE
)

