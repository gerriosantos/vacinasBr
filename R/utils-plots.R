# Objetivo \u00e9 criar fun\u00e7\u00f5es que v\u00e3o alimentar o APP
## vers\u00e3o com .data \u00e9 para usar com aspas no paramentro

barras <- function(da, var){
  da %>%
    dplyr::group_by(data) %>%
    dplyr::summarise(total = sum(.data[[var]])) %>%
    ggplot2::ggplot()+
    ggplot2::aes(data, total)+
    ggplot2::geom_col()+
    ggplot2::theme_minimal(12)
}

# Versao para usar sem aspas -- menos usual em packages
barras_ <- function(da, var){
  da %>%
    dplyr::group_by(data) %>%
    dplyr::summarise(total = sum({{var}})) %>%
    ggplot2::ggplot()+
    ggplot2::aes(data, total)+
    ggplot2::geom_col()+
    ggplot2::theme_minimal(12)

}




tabela <- function(da){
  da %>%
    dplyr::group_by(regiao_nm, uf_nm) %>%
    dplyr::summarise(
      total = sum(n),
      total_80 = sum(n_80),
      .groups = 'drop'
      ) %>%
    reactable::reactable(
      groupBy = 'regiao_nm',
      columns = list(
        regiao_nm = reactable::colDef('Regi\u00e3o'),
        uf_nm = reactable::colDef('UF'),
        total = reactable::colDef('Total',
                                  aggregate = 'sum'
                                  ),
        total_80 = reactable::colDef('Total > 80',
                                     aggregate = 'sum'
                                     )
      ))
}


mapa <- function(da, var){

  f <- as.formula(paste0('~', var))
  da %>%
    dplyr::filter(uf_sigla == 'CE') %>%
    dplyr::group_by(muni_id, muni_nm) %>%
    dplyr::summarise(
      dplyr::across(c(lat,lon), dplyr::first),
      n = sum(.data[[var]]),
      .groups = 'drop'
    ) %>%
    dplyr::mutate(
      lab = paste(muni_nm, ':', n)
      ) %>%
    leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircles(
      lat = ~lat,
      lng = ~lon,
      radius = ~n,
      stroke = T,
      weight = 2,
      color = 'purple',
      popup = ~lab
      #label = ~as.character(muni_nm)
    )
     #leaflet::addProviderTiles("CartoDB.Positron") #%>%
    # leaflet::addCircleMarkers(
    #   #color = ~pallete('muni_nm'),
    #   stroke = FALSE,
    #   fillOpacity = 0.05,
    #   lat = ~lat,
    #   lng = ~lon,
    #   label = ~as.character(muni_nm),
    # )
}






