## server.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(bigrquery)
library(RMySQL)
library(scales)
library(ggthemes)
library(DT)
library(directlabels)
# library(RODBC)

server <- function(input, output){

# ANALISIS VENTAS=============================
## VENTAS=====================================
  creditos <- reactive({ 
    con <- dbConnect(MySQL(), host="192.168.100.17", port= 3306, user="financiero",password = "3fzcix8x5g", dbname="creditos")
    if (input$analisisVentas_producto == 'Libranza') {
      df <- dbGetQuery(con, paste("SELECT DATE_FORMAT(periodo_contable ,'%Y-%m-01') AS periodo, SUM(vlr_libranza) AS bruto, (SUM(vlr_libranza)-SUM(colocacion)) AS neto, COUNT(credito_id_ant) AS cantidad FROM y_credito WHERE estado_id NOT IN (33, 22, 20) AND (periodo_contable BETWEEN '", input$analisisVentas_rangoFecha[1], "' AND '", input$analisisVentas_rangoFecha[2], "') GROUP BY DATE_FORMAT(periodo_contable ,'%Y-%m-01')")
      )
    }
    
    if (input$analisisVentas_producto == 'CrediHuy') {
      df <- dbGetQuery(con, paste("SELECT DATE_FORMAT(fechareg ,'%Y-%m-01') AS periodo, SUM(vlr_libranza) AS bruto, SUM(vlr_libranza) AS neto, COUNT(id_credito_sufi) AS cantidad FROM y_credito WHERE estado_id = 33 AND (fechareg BETWEEN '", input$analisisVentas_rangoFecha[1], "' AND '", input$analisisVentas_rangoFecha[2], "') GROUP BY DATE_FORMAT(fechareg ,'%Y-%m-01')"))
    }
    dbDisconnect(con)
    df
  })

  creditos_ofic <- reactive({
    con <- dbConnect(MySQL(), host="192.168.100.17", port= 3306, user="financiero",password = "3fzcix8x5g", dbname="creditos")
    if (input$analisisVentas_producto == 'Libranza') {
      df2 <- dbGetQuery(con, paste("SELECT DATE_FORMAT(periodo_contable ,'%Y-%m-01') AS periodo, (SELECT nombre FROM empresa WHERE empresa_id=empresa.id) AS oficina, SUM(vlr_libranza) AS bruto, SUM(colocacion) AS neto, COUNT(credito_id_ant) AS cantidad FROM y_credito WHERE estado_id NOT IN (33, 22, 20) AND (periodo_contable BETWEEN '", input$analisisVentas_rangoFecha[1], "' AND '", input$analisisVentas_rangoFecha[2], "') GROUP BY DATE_FORMAT(periodo_contable ,'%Y-%m-01'), empresa_id"))
    }
    if (input$analisisVentas_producto == 'CrediHuy') {
      df2 <- dbGetQuery(con, paste("SELECT DATE_FORMAT(fechareg ,'%Y-%m-01') AS periodo, (SELECT nombre FROM empresa WHERE id = y_credito.empresa_id) AS oficina, SUM(vlr_libranza) AS bruto, SUM(vlr_libranza) AS neto, COUNT(id_credito_sufi) AS cantidad FROM y_credito WHERE estado_id = 33 AND (fechareg BETWEEN '", input$analisisVentas_rangoFecha[1], "' AND '", input$analisisVentas_rangoFecha[2], "') GROUP BY DATE_FORMAT(fechareg ,'%Y-%m-01'), empresa_id ")) %>%
        filter(tolower(oficina) == tolower(input$analisisVentas_oficinas))
    }
    dbDisconnect(con)
    df2
  })
  
  output$analisisVentas_ventasPlot <- renderPlot({
    if (input$analisisVentas_oficinas == 'Todas') {
      creditos() %>%
        mutate(periodo = ymd(periodo), bruto = bruto/1000000, neto = neto/1000000) %>%
        ggplot(aes(x = periodo, y = bruto, label = format(round(bruto, 1), big.mark = ','))) +
        geom_point(size = 4, color = '#000080') +
        geom_line(linetype = "dotted", color = '#1b75bc', size = 1) +
        geom_text(aes(y = bruto + (bruto*0.15)), size = 4, check_overlap = TRUE) +
        scale_x_date(labels = date_format("%m-%Y")) +
        labs(y = 'Valor en Millones', x = 'Periodo', title = 'Ventas en Millones de Pesos')
    } else {
      creditos_ofic() %>%
        filter(tolower(oficina) == tolower(input$analisisVentas_oficinas)) %>%
        mutate(periodo = ymd(periodo), bruto = bruto/1000000, neto = neto/1000000) %>%
        ggplot(aes(x = periodo, y = bruto, label = format(round(bruto, 1), big.mark = ','))) +
        geom_point(size = 4, color = '#000080') +
        geom_line(linetype = "dotted", color = '#1b75bc', size = 1) +
        geom_text(aes(y = bruto + (bruto*0.15)), size = 4, check_overlap = TRUE) +
        scale_x_date(labels = date_format("%m-%Y")) +
        labs(y = 'Valor en Millones', x = 'Periodo', title = 'Ventas en Millones de Pesos')
    }
  })
  
  output$analisisVentas_cantidadPlot <- renderPlot({
    if(input$analisisVentas_oficinas == 'Todas'){
      creditos() %>% 
        mutate(periodo=ymd(periodo)) %>% 
        ggplot(aes(x = periodo, y = cantidad)) +
        geom_line(aes(x=periodo, y=cantidad), linetype = "dotted", size = 1, color = '#1D8348') +
        geom_point(aes(x=periodo, y=cantidad), size = 4, color = '#1D8348') +
        geom_text(aes(x=periodo, y=cantidad+(cantidad*0.1), label = cantidad), size = 4,
                  check_overlap = TRUE) +
        scale_x_date(labels = date_format("%m-%Y")) +
        labs(y='', x='Periodo', title='Ventas en Número de Créditos')
    } else {
      creditos_ofic() %>% 
        filter(tolower(oficina) == tolower(input$analisisVentas_oficinas)) %>% 
        mutate(periodo=ymd(periodo)) %>% 
        ggplot(aes(x = periodo, y = cantidad)) +
        geom_line(aes(x=periodo, y=cantidad), linetype = "dotted", size = 1, color = '#1D8348') +
        geom_point(aes(x=periodo, y=cantidad), size = 4, color = '#1D8348') +
        geom_text(aes(x=periodo, y=cantidad+(cantidad*0.1), label = cantidad), size = 4,
                  check_overlap = TRUE) +
        scale_x_date(labels = date_format("%m-%Y")) +
        labs(y='', x='Periodo', title='Ventas en Número de Créditos')
    }
  })
  
  output$analisisVentas_ventasTable <- renderTable({
  if(input$analisisVentas_oficinas == 'Todas'){
    creditos() %>% 
      mutate(bruto = format(bruto, big.mark = ',' ), neto = format(neto, big.mark = ',' ),
             cantidad = as.integer(cantidad), Prepagos = NA, Cant.Prepagos = NA, Cant.Retanqueos = NA,
             Retanqueos = NA, Tasa = NA,
             Plazo = NA, Cuota = NA) %>% 
      rename(Fecha = periodo, Lib.Bruta = bruto, Lib.Neta = neto, Cantidad = cantidad)
  } else {
    creditos_ofic() %>% 
      mutate(bruto = format(bruto, big.mark = ',' ), neto = format(neto, big.mark = ',' ),
             cantidad = as.integer(cantidad), Prepagos = NA, Cant.Prepagos = NA, Cant.Retanqueos = NA,
             Retanqueos = NA, Tasa = NA,
             Plazo = NA, Cuota = NA) %>% 
      rename(Fecha = periodo, Lib.Bruta = bruto, Lib.Neta = neto, Cantidad = cantidad)
    }
  })
## //VENTAS===================================

## ASESORES===================================
  base_ind_gestion <- reactive({
    if (input$analisisVentas_producto == 'Libranza') {
      #channel<-odbcConnect(dsn='dashboard', uid = "financiero", pwd = '3fzcix8x5g')
      con <- dbConnect(MySQL(), host="192.168.100.17", port= 3306, user="financiero",password = "3fzcix8x5g", dbname="creditos")
      base <- dbGetQuery(con, paste("(SELECT vendedor, concepto, UPPER(oficina) AS oficina, COUNT(credito_id_ant) AS cantidad, fecha FROM (SELECT UPPER((SELECT nombre FROM usuarios WHERE id = y_credito.vendedor_id)) AS vendedor, 'Prepagados' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = y_credito.empresa_id)) AS oficina, credito_id_ant, vlr_libranza, n_cuotas, fecha_finCre, vendedor_id, (SELECT no_cuota FROM y_cierre_cartera WHERE credito_id = y_credito.credito_id_ant) AS n_pagos, LAST_DAY(fecha_finCre) AS fecha FROM y_credito WHERE YEAR(fecha_finCre) = ", input$analisisVentas_asesores_year, " AND MONTH(fecha_finCre) = ", input$analisiVentas_asesores_mes, " AND estado_id = 14 AND pagaduria_id != 7) AS temp WHERE n_pagos<n_cuotas GROUP BY vendedor, fecha) UNION  (SELECT UPPER((SELECT nombre FROM usuarios WHERE id = creditos.id_vendedor)) AS vendedor, 'Visitados' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = creditos.empresa_id)) AS oficina, COUNT(DISTINCT(doc_profesor)) AS cantidad, LAST_DAY(fechareg) AS fecha FROM creditos WHERE YEAR(fechareg) = ", input$analisisVentas_asesores_year, " AND MONTH(fechareg) = ", input$analisiVentas_asesores_mes, "GROUP BY vendedor, fecha) UNION (SELECT UPPER((SELECT nombre FROM usuarios WHERE id=y_credito.vendedor_id)) AS vendedor, 'Desembolsado' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = y_credito.empresa_id)) AS oficina, COUNT(id) AS cantidad, LAST_DAY(periodo_contable) AS fecha FROM y_credito WHERE YEAR(periodo_contable) = ", input$analisisVentas_asesores_year, " AND MONTH(periodo_contable) = ", input$analisiVentas_asesores_mes, " GROUP BY vendedor, periodo_contable)"))
      dbDisconnect(con)
      base} else {
        0
    }
  })
  
  base_ind_gestion2 <- reactive({
    #channel<-odbcConnect(dsn='dashboard', uid = "financiero", pwd = '3fzcix8x5g')
    con <- dbConnect(MySQL(), host="192.168.100.17", port= 3306, user="financiero",password = "3fzcix8x5g", dbname="creditos")
    base <- dbGetQuery(con, paste("(SELECT vendedor, concepto, UPPER(oficina) AS oficina, COUNT(credito_id_ant) AS cantidad, fecha FROM (SELECT  UPPER((SELECT nombre FROM usuarios WHERE id = y_credito.vendedor_id)) AS vendedor, 'Prepagados' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = y_credito.empresa_id)) AS oficina, credito_id_ant, vlr_libranza, n_cuotas, fecha_finCre, vendedor_id, (SELECT no_cuota FROM y_cierre_cartera WHERE credito_id = y_credito.credito_id_ant) AS n_pagos, LAST_DAY(fecha_finCre) AS fecha FROM y_credito WHERE (fecha_finCre BETWEEN '", as.character(input$analisisVentas_rangoFecha[1]), "' AND '", as.character(input$analisisVentas_rangoFecha[2]), "') AND estado_id = 14 ) AS temp WHERE n_pagos<n_cuotas GROUP BY vendedor, fecha) UNION (SELECT UPPER((SELECT nombre FROM usuarios WHERE id = creditos.id_vendedor)) AS vendedor, 'Visitados' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = creditos.empresa_id)) AS oficina, COUNT(DISTINCT(doc_profesor)) AS cantidad, LAST_DAY(fechareg) AS fecha FROM creditos WHERE (fechareg BETWEEN '", as.character(input$analisisVentas_rangoFecha[1]), "' AND '", as.character(input$analisisVentas_rangoFecha[2]), "') GROUP BY vendedor, fecha) UNION (SELECT UPPER((SELECT nombre FROM usuarios WHERE id=y_credito.vendedor_id)) AS vendedor, 'Desembolsado' AS concepto, UPPER((SELECT nombre FROM empresa WHERE id = y_credito.empresa_id)) AS oficina, COUNT(id) AS cantidad, LAST_DAY(periodo_contable) AS fecha FROM y_credito WHERE (periodo_contable BETWEEN '", as.character(input$analisisVentas_rangoFecha[1]), "' AND '",  as.character(input$analisisVentas_rangoFecha[2]), "') GROUP BY vendedor, periodo_contable)"))
    dbDisconnect(con)
    base
  })
  
  ases_nombres <- reactive({
    if(input$analisisVentas_oficinas == 'Todas'){
      base_ind_gestion() %>% 
        filter(concepto == input$analisisVentas_asesores_actividad) %>% 
        filter(!vendedor %in% c('ALVARO JOSE PABON', 'DANIELA BULA', 'GOWER CHACON', 'JORGE ALBERTO VARGAS MEJIA', 'JEAN PIERRE GROSSO'), !is.na(vendedor)) %>% 
        distinct(vendedor)
    } else {
      base_ind_gestion() %>% 
        filter(tolower(oficina) == tolower(input$analisisVentas_oficinas), concepto == input$analisisVentas_asesores_actividad) %>% 
        filter(!vendedor %in% c('ALVARO JOSE PABON', 'DANIELA BULA', 'GOWER CHACON', 'JORGE ALBERTO VARGAS MEJIA', 'JEAN PIERRE GROSSO'), !is.na(vendedor)) %>% 
        distinct(vendedor)
    }
  })
  
  output$analisisVentas_asesoresPlot <- renderPlot({
    if(input$analisisVentas_oficinas == 'Todas'){
      base_ind_gestion() %>% 
        filter(concepto == input$analisisVentas_asesores_actividad) %>% 
        filter(!vendedor %in% c('ALVARO JOSE PABON', 'DANIELA BULA', 'GOWER CHACON', 'JORGE ALBERTO VARGAS MEJIA',
                                'JEAN PIERRE GROSSO'), !is.na(vendedor)) %>% 
        group_by(vendedor) %>% 
        summarize(total_cantidad = as.integer(sum(cantidad))) %>% 
        arrange(desc(total_cantidad)) %>% 
        top_n(10) %>% 
        ggplot() + 
        geom_bar(mapping = aes(x = reorder(vendedor, total_cantidad), y = total_cantidad, fill = vendedor), stat = 'identity') +
        geom_text(aes(label = total_cantidad, y = total_cantidad + 1, x = vendedor), check_overlap = TRUE) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x = '', y = 'Cantidad', title = 'Top Asesores') +
        coord_flip()
    } else {
      base_ind_gestion() %>% 
        filter(tolower(oficina) == tolower(input$analisisVentas_oficinas), concepto == input$analisisVentas_asesores_actividad) %>% 
        filter(!vendedor %in% c('ALVARO JOSE PABON', 'DANIELA BULA', 'GOWER CHACON', 'JORGE ALBERTO VARGAS MEJIA',
                                'JEAN PIERRE GROSSO'), !is.na(vendedor)) %>% 
        group_by(vendedor) %>% 
        summarize(total_cantidad = as.integer(sum(cantidad))) %>% 
        arrange(desc(total_cantidad)) %>% 
        top_n(10) %>% 
        ggplot() + 
        geom_bar(mapping = aes(x = reorder(vendedor, total_cantidad), y = total_cantidad, fill = vendedor), stat = 'identity') +
        geom_text(aes(label = total_cantidad, y = total_cantidad + 1, x = vendedor), check_overlap = TRUE) +
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
              legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        labs(x = '', y = 'Cantidad', title = 'Top Asesores') +
        coord_flip()
    }
  })
  
  output$analisisVentas_asesoresSelect <- renderUI({
    selectInput(
      inputId = 'ases_list',
      label = 'Analizar histórico por asesor',
      multiple = TRUE,
      width = '100%',
      choices = ases_nombres()$vendedor
    )
  })
  
  output$grafica_hist <- renderPlot({
    
    if(input$analisisVentas_oficinas == 'Todas'){
      if(length((base_ind_gestion2() %>% filter(vendedor %in% (input$ases_list)))$vendedor)){ 
        base_ind_gestion2() %>% 
          mutate(fecha = ymd(fecha), concepto = factor(concepto, levels = c('Visitados', 'Desembolsado', 'Prepagados'))) %>% 
          filter(vendedor %in% (input$ases_list)) %>% 
          ggplot(aes(x=fecha, y=as.integer(cantidad), group=vendedor)) +
          geom_line(aes(color=vendedor), linetype = 'dashed') +
          geom_point(aes(color=vendedor), size = 1.5) +
          geom_text(aes(label = cantidad, y = cantidad + 4, x = fecha, color=vendedor), check_overlap = TRUE) +
          facet_wrap(~concepto, nrow = 3) +
          labs(x='Fecha', y='Cantidad') +
          #theme(legend.position="right", legend.direction='vertical', panel.grid.major = element_blank(),
          #      panel.grid.minor = element_blank()) +
          scale_x_date(labels = date_format("%m-%Y")) 
        
      } else{NULL}
      
    }else{
      if(length((base_ind_gestion2() %>% filter(vendedor %in% (input$ases_list)))$vendedor)){ 
        base_ind_gestion2() %>% 
          mutate(fecha = ymd(fecha), concepto = factor(concepto, levels = c('Visitados', 'Desembolsado', 'Prepagados'))) %>% 
          filter(tolower(oficina) == tolower(input$oficinas), vendedor %in% (input$ases_list)) %>% 
          ggplot(aes(x=fecha, y=as.integer(cantidad), group=vendedor)) +
          geom_line(aes(color=vendedor), linetype = 'dashed') +
          geom_point(aes(color=vendedor), size = 1.5) +
          geom_text(aes(label = cantidad, y = cantidad + 4, x = fecha, color=vendedor), check_overlap = TRUE) +
          facet_wrap(~concepto, nrow = 3) +
          labs(x='Fecha', y='Cantidad') +
          #theme(legend.position="right", legend.direction='vertical', panel.grid.major = element_blank(),
          #      panel.grid.minor = element_blank()) +
          scale_x_date(labels = date_format("%m-%Y"))
      } else{NULL}
    }  
    
  })
  
  output$analisisVentas_asesores_nomAsesores <- renderText({
    y <- base_ind_gestion() %>% 
      filter(tolower(oficina) == tolower(input$analisisVentas_oficinas), concepto == input$analisisVentas_asesores_actividad) %>% 
      filter(!vendedor %in% c('ALVARO JOSE PABON', 'DANIELA BULA', 'GOWER CHACON', 'JORGE ALBERTO VARGAS MEJIA',
                              'JEAN PIERRE GROSSO'), !is.na(vendedor)) %>% 
      group_by(vendedor) %>% 
      summarize(total_cantidad = as.integer(sum(cantidad))) %>% 
      arrange(desc(total_cantidad)) %>% 
      top_n(10) %>% 
      select(vendedor) %>% 
      as.data.frame()
    
    x <- as.data.frame(ases_nombres())
    
    z <- anti_join(x, y, by = 'vendedor')
    
    #x <- paste(ases_nombres(), collapse=" ")
    paste('Nombre de los asesores que no aparecen: ', z)
  })
## //ASESORES=================================
# //ANALISIS VENTAS===========================
  
# INTELIGENCIA DE MERCADO=====================
## COMPETIDORES===============================
  competencia_recaudo <- read_csv("competencia_recaudo.csv")
  competencia_ventas <- read_csv("competencia_ventas.csv")
  
  output$inteligenciaMercado_competidores_recaudoPlot <- renderPlot({
    competencia_recaudo %>%
      filter(entidad %in% c('B. AV VILLAS', 'BBVA', 'FINSOCIAL', 'DAVIVIENDA', 
                            'B. POPULAR')) %>% 
      ggplot(aes(x=fecha, y=total/1000000, color=entidad), hjust = -.1) +
      geom_line() +
      geom_point() +
      geom_dl(aes(label = entidad), method = list(dl.trans(x = x-0.3, y = y+0.3), "last.points")) +
      labs(y='Recaudo en millones', x='', title = 'Recaudos Principales Entidades', subtitle= 'Pagaduria Barranquilla') +
      theme(legend.position="none") +
      scale_x_date(expand = c(0.15, 0), breaks = date_breaks("years"), labels = date_format("%Y/%m")) + 
      scale_y_continuous(breaks=seq(0, 2500, 100))
    
  })
  
  output$inteligenciaMercado_competidores_competenciaoTable <- renderDataTable({
    competencia_ventas %>% 
      separate(trimestre, c("year", "trim")) %>% 
      filter(year==as.integer(year(today()))) %>% 
      arrange(trim, desc(total)) %>% 
      mutate(total=format(total, big.mark = ',')) %>% 
      rename(Año=year, Trim=trim, Entidad=entidad, Recaudo=total)
    
  })
## //COMPETIDORES=============================

## RECAUDO====================================
  df <- eventReactive(
    input$inteligenciaMercado_recaudos_aplicar,
    {
      project <- 'finso-analytic'
      entidades <- 'finsocial'
      fecha_max <- query_exec('SELECT MAX(fecha) as fecha_max FROM `Info_Clientes.egresos_clientes`', project = project, use_legacy_sql = FALSE)
      entidades <- unlist(strsplit(input$inteligenciaMercado_recaudos_entidad, ','))
      n <- length(entidades)
      n_stop <- entidades[n]
      for (entidad in entidades){
        if (entidad != n_stop){
          filtro <- paste(filtro, paste0("LOWER(entidad) LIKE ('%", tolower(entidad), "%') OR "))
        }else{
          filtro <- paste(filtro, paste0("LOWER(entidad) LIKE ('%", tolower(entidad), "%') "))
        }
      }
      
      sql <- paste0("SELECT * FROM `Info_Clientes.egresos_clientes` WHERE ", filtro, "AND FECHA ='", fecha_max$fecha_max, "'")
      query_exec(sql, project = project, use_legacy_sql = FALSE, max_pages = Inf)
    }
  )
  
  output$inteligenciaMercado_recaudos_resultados <- renderText({
    entidades2 <- unlist(strsplit(input$inteligenciaMercado_recaudo_entidad2, ","))
    n <- format(nrow(df() %>% filter(!entidad %in% entidades2)), big.mark = ',')
    tot <- format(df() %>% filter(!entidad %in% entidades2) %>% summarize(total = sum(as.numeric(valor))), big.mark = ',')
    paste('Se encontraron', n, 'registros por un valor total de $', tot)
  })
  
  output$inteligenciaMercado_recaudos_table <- renderDataTable({ 
    entidades2 <- unlist(strsplit(input$inteligenciaMercado_recaudo_entidad2, ","))
    df() %>% 
      filter(!entidad %in% entidades2) %>% 
      group_by(Pagaduria = pagaduria, Entidad = entidad) %>%
      summarise(Cantidad = n(), Recaudo = sum(as.numeric(valor))) %>% 
      arrange(Pagaduria) %>% 
      mutate(Recaudo = format(round(Recaudo,0), big.mark = ","))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("reporte_recaudos-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df() %>% filter(!entidad %in% unlist(strsplit(input$inteligenciaMercado_recaudo_entidad2, ","))),
                file, row.names = FALSE)
  })
  
  output$inteligenciaMercado_recaudos_clientesEspecificos_contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    entidades2 <- unlist(strsplit(input$inteligenciaMercado_recaudo_entidad2, ","))
    inFile <- input$inteligenciaMercado_recaudo_file1
    if (is.null(inFile))
      return(NULL)
    
    cedulas <- read.csv(inFile$datapath, header = TRUE)
    df() %>% filter(!entidad %in% entidades2) %>% 
      semi_join(cedulas, by = c('cod_empleado' = 'cedula')) %>% 
      rename(Cedula = cod_empleado, Nombre = nombre, Valor = valor,
             Entidad = entidad, Pagaduria = pagaduria, Fecha = fecha)
  })
  output$entidades <- renderTable({
    entidades2 <- unlist(strsplit(input$inteligenciaMercado_recaudo_entidad2, ","))
    df() %>% filter(!entidad %in% entidades2) %>% distinct(entidad)})
## //RECAUDO==================================
# //INTELIGENCIA DE MERCADO===================
  
# CLIENTES POTENCIALES========================
## BASES DE DATOS=============================
  output$clientesPot_db_table <- DT::renderDataTable({
    read_csv("disponibles_Abril.csv") %>%
      filter(edad == input$clientesPot_db_edad, pagaduria == input$clientesPot_db_pagaduria, municipio == input$clientesPot_db_ciudades)
  })

## //BASES DE DATOS===========================
# //CLIENTES POTENCIALES======================
    
}