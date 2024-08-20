# Hacer diccionarios ----

diccionario_bancos_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_bancos_df.xlsx"), col_names = TRUE)
diccionario_ibex_df <- readxl::read_xlsx(path = paste0(datos_path, "diccionario_ibex_df.xlsx"), col_names = TRUE)


# diccionario_bancos_df <- tibble::tibble(
#   nombres = c("BBVA", "Santander", "CaixaBank", "Bankia", "Sabadell", "Bankinter", # Lo organizamos por países
#               "BNP Paribas", "Société Générale", "Crédit Agricole",
#               "HSBC Holdings", "Barclays", "Lloyds Banking Group", "Royal Bank of Scotland", "Standard Chartered",
#               "Deutsche Bank", "Commerzbank",
#               "JPMorgan Chase & Co.", "Bank of America", "Wells Fargo & Co.", "Citigroup Inc.", "Goldman Sachs Group Inc.", "Morgan Stanley", "U.S. Bancorp", "PNC Financial Services", "Capital One Financial", "Bank of New York Mellon"),
#   
#   codigos = c("BBVA.MC", "SAN.MC", "CABK.MC", "BKIA.MC", "SAB.MC", "BKT.MC", # ESP
#               "BNP.PA", "GLE.PA", "ACA.PA", # Francia
#               "HSBA.L", "BARC.L", "LLOY.L", "NWG.L", "STAN.L", # UK
#               "DBK.DE", "CBK.DE", # Alemania
#               "JPM", "BAC", "WFC", "C", "GS", "MS", "USB", "PNC", "COF", "BK") # US
# )





# Pendiente. Función para Generar gráficos de lineas
generar_densidad_plot <- function(
    .data,
    .valores,
    .nombres="",
    .title="",
    .xscale=1,
    .xaccuracy=1,
    .xsuffix="",
    .yscale=1,
    .yaccuracy=0.01,
    .ysuffix="",
    .trans="",
    .debug = F
) {
  
  plot_gg <- ggplot(data=.data |>
                      group_by(!!as.symbol(.nombres)) |>
                      mutate(
                        media = mean(!!as.symbol(.valores), na.rm = T),
                        mediana = median(!!as.symbol(.valores), na.rm=T),
                        p25 = quantile(!!as.symbol(.valores), probs = c(0.25,0.75), na.rm=T) |> _[["25%"]],
                        p75 = quantile(!!as.symbol(.valores), probs = c(0.25,0.75), na.rm=T) |> _[["75%"]]
                      )) +
    geom_density(
      mapping=aes(
        x=!!as.symbol(.valores),
        color=!!as.symbol(.nombres)
      )
    ) +
    ggiraph::geom_vline_interactive(
      mapping=aes(xintercept=media,
                  color=!!as.symbol(.nombres),
                  tooltip=paste0(
                    "Conjunto: ", !!as.symbol(.nombres), "\n",
                    "Media: ", scales::number_format(big.mark=".", decimal.mark=",", accuracy=0.01)(media))
      ),
      linetype="solid",
      linewidth=0.5
    ) +
    ggiraph::geom_vline_interactive(
      mapping=aes(xintercept=mediana,
                  color=!!as.symbol(.nombres),
                  tooltip=paste0(
                    "Conjunto: ", !!as.symbol(.nombres), "\n" ,
                    "Mediana: ", scales::number_format(big.mark=".", decimal.mark=",", accuracy=0.01)(mediana)
                  )
      ),
      linetype="twodash",
      linewidth=0.5
    )+
    ggiraph::geom_vline_interactive(
      mapping=aes(xintercept=p25,
                  color=!!as.symbol(.nombres),
                  tooltip=paste0(
                    "Conjunto: ", !!as.symbol(.nombres), "\n",
                    "P25: ", scales::number_format(big.mark=".", decimal.mark=",", accuracy=0.01)(p25))
      ),
      linetype="dashed",
      linewidth=0.5
    )+
    ggiraph::geom_vline_interactive(
      mapping=aes(xintercept=p75,
                  color=!!as.symbol(.nombres),
                  tooltip=paste0(
                    "Conjunto: ", !!as.symbol(.nombres), "\n",
                    "P75: ", scales::number_format(big.mark=".", decimal.mark=",", accuracy=0.01)(p75)),
      ),
      linetype="dashed",
      linewidth=0.5
    )+
    tesorotools::tema_gabinete +
    theme(
      title = element_text(size=18),
      text = element_text(size=18)
    ) +
    scale_y_continuous(
      label=scales::number_format(
        scale=.yscale,
        accuracy=.yaccuracy,
        suffix=.ysuffix
      )
    ) +
    ggtitle(.title)
  
  
  if(.trans != "") {
    plot_gg <- plot_gg +
      scale_x_continuous(
        label=scales::number_format(
          scale=.xscale,
          suffix=.xsuffix,
          accuracy=.xaccuracy
        ),
        trans=.trans
      )
  } else {
    plot_gg <- plot_gg +
      scale_x_continuous(
        label=scales::number_format(
          scale=.xscale,
          suffix=.xsuffix,
          accuracy=.xaccuracy
        )
      )
  }
  
  ggiraph::girafe(ggobj=plot_gg)
}