# actualizar.R

setwd("\\\\fas12\\VOL3\\ANALISIS-FINANCIERO\\bancos_panel")

Sys.setlocale("LC_ALL", "Spanish_Spain.UTF8")

datos_path <- "datos/"

bot_telegram <- tesorotools::iniciar_telegram()
quiet_toggle <- TRUE
telegram_message_string <- ""

tesorotools::mensaje_telegram(bot_telegram, paste0("[ ", Sys.time(), " ] ", "Resultados bancos - Actualizando..."))

tesorotools::ejecutar_r(bot_telegram, "add_tesoroseries.R")

# source_return_value <- tryCatch({
#   rmarkdown::render("Compraventa_valores_dashboard.Rmd")
#   file.copy("Compraventa_valores_dashboard.html", "C:/Users/SecretAnalisisFinanc/OneDrive - MINECO/General - SG Análisis Financiero-Teams/Dashboards", overwrite=T)
#   source_return_value <- TRUE
# }, error = function(e) {tesorotools::mensaje_telegram(bot_telegram, paste0("Error compilando Rmds: ", e))})
# 
# if(source_return_value) {
#   tesorotools::mensaje_telegram(bot_telegram, "\U2705 - Compraventa_valores_dashboard.Rmd compilado")
# }
