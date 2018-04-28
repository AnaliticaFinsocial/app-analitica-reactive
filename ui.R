## ui.R ##
library(shiny)
library(shinydashboard)
library(lubridate)
library(shinycssloaders)

options(spinner.color="#0771B4")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      'Análisis  de Ventas',
      tabName = 'analisis-ventas',
      icon = icon('bar-chart-o')
    ),
    # menuItem(
    #   'Inteligencia de Mercado',
    #   icon = icon('globe'),
    #   menuSubItem(
    #     'Competidores',
    #     tabName = 'competidores'
    #   ),
    #   menuSubItem(
    #     'Recaudos',
    #     tabName = 'recaudos'
    #   )
    # ),
    menuItem(
      'Clientes Potenciales',
      icon = icon('crosshairs'),
      tabName = 'bases-datos',
      badgeLabel = 'Nuevo',
      badgeColor = 'maroon'
      # menuSubItem(
      #   'Bases de Datos',
      #   tabName = 'bases-datos'
      #   # badgeLabel = 'Nuevo',
      #   # badgeColor = 'blue'
      # )
      # menuSubItem(
      #   'Segmentación',
      #   tabName = 'segmentacion'
      # )
    )
  )
)

body <- dashboardBody(
  tags$head(
    tags$link( rel = 'stylesheet', type = 'text/css', href = './css/app.css' )
  ),
  tabItems(
    tabItem(
      tabName = 'analisis-ventas',
      fluidRow(
        box(
          width = 12,
          title = 'Parámetros',
          solidHeader = TRUE,
          collapsible = TRUE,
          column(
            width = 3,
            selectInput(
              inputId = 'analisisVentas_producto',
              label = 'Linea de Crédito',
              choices = c('Libranza', 'CrediHuy')
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = 'analisisVentas_oficinas',
              label = 'Ofinica',
              choices = c('Todas',
                          'Apartadó'='apartadó',
                          'Armenia',
                          'Barranquilla',
                          'Bogotá'='bogota',
                          'Cali',
                          'Cartagena',
                          'Ibagué'='ibague',
                          'Magangué'='magangue',
                          'Medellín'='medellin',
                          'Montería'='monteria',
                          'Pereira',
                          'Santa Marta',
                          'Sucre',
                          'Valledupar',
                          'Yopal')
            )
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          solidHeader = TRUE,
          tabsetPanel(
            tabPanel(
              'Ventas',
              fluidRow(
                column(
                  width = 6,
                  dateRangeInput(
                    inputId = 'analisisVentas_rangoFecha',
                    label = 'Fechas',
                    separator = '-',
                    start = '2017-01-01',
                    end = rollback(today())
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput(
                    outputId = 'analisisVentas_tabla'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = 'center',
                  withSpinner(
                    plotOutput(
                      outputId = 'analisisVentas_ventasPlot'
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = 'center',
                  withSpinner(
                    plotOutput(
                      outputId = 'analisisVentas_cantidadPlot'
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = 'center',
                  tableOutput(
                    outputId = 'analisisVentas_ventasTable'
                  )
                )
              )
            ),
            tabPanel(
              'Asesores',
              fluidRow(
                column(
                  width = 12,
                  fluidRow(
                    width = 12,
                    column(
                      width = 3,
                      selectInput(
                        inputId = 'analisisVentas_asesores_year',
                        label = 'Año',
                        choices = 2012:year(today()),
                        selected = year(today())
                      )
                    ),
                    column(
                      width = 3,
                      selectInput(
                        inputId = 'analisiVentas_asesores_mes',
                        label = 'Mes',
                        choices = c('Enero' = 1,
                                    'Febrero' = 2,
                                    'Marzo' = 3,
                                    'Abril' = 4,
                                    'Mayo' = 5,
                                    'Junio' = 6,
                                    'Julio' = 7,
                                    'Agosto' = 8,
                                    'Septiembre' = 9,
                                    'Octubre' = 10,
                                    'Noviembre' = 11,
                                    'Diciembre' = 12),
                        selected = month(today())
                      )
                    ),
                    column(
                      width = 3,
                      selectInput(
                        inputId = 'analisisVentas_asesores_actividad',
                        label = 'Actividad',
                        choices = c('Visitados', 'Desembolsado', 'Prepagados'),
                        selected = 'Desembolsado'
                      )
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = 'center',
                  plotOutput(
                    outputId = 'analisisVentas_asesoresPlot'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  align = 'center',
                  verbatimTextOutput(
                    outputId = 'analisisVentas_asesores_nomAsesores'
                  )
                )
              ),
              fluidRow(
                column(
                  width = 12,
                  uiOutput(
                    'analisisVentas_asesoresSelect'
                    )
                  )
                ),
              fluidRow(
                column(
                  width = 3,
                  dateRangeInput(
                    inputId = 'rango_fecha2',
                    label = 'Fechas',
                    separator = '-',
                    start = '2017-01-01'
                    )
                  )
                ),
              plotOutput('grafica_hist')
            )
          )
        )
      )
    ),
    #' tabItem(
    #'   tabName = 'competidores',
    #'   fluidRow(
    #'     box(
    #'       width = 12,
    #'       title = 'Parámetros',
    #'       solidHeader = TRUE,
    #'       status = 'primary',
    #'       collapsible = TRUE,
    #'       column(
    #'         width = 3,
    #'         selectInput(
    #'           inputId = 'inteligenciaMercado_competidores_pagadurias',
    #'           label = 'Pagadurías',
    #'           choices = c(#'Antioquia',
    #'                       #'Apartadó',
    #'                       #'Armenia',
    #'                       # 'Atlántico',
    #'                       'Barranquilla',
    #'                       #'Bello',
    #'                       #'Bolívar',
    #'                       #'Buga',
    #'                       #'Caldas',
    #'                       #'Cali',
    #'                       #'Cartagena',
    #'                       #'Casanare',
    #'                       #'Cesar',
    #'                       #'Cienaga',
    #'                       #'Colpensiones',
    #'                       #'Cordoba',
    #'                       #'Cundinamarca',
    #'                       #'Dosquebradas',
    #'                       #'Fiduprevisora',
    #'                       #'Fopep',
    #'                       #'Ibagué',
    #'                       #'Jamundi',
    #'                       #'Lorica',
    #'                       #'Magangue',
    #'                       #'Magdalena',
    #'                       #'Medellin',
    #'                       #'Montería',
    #'                       #'Pereira',
    #'                       #'Risaralda',
    #'                       #'Sabaneta',
    #'                       #'Sahagun',
    #'                       #'Sincelejo',
    #'                       #'Soledad',
    #'                       #'Sucre',
    #'                       #'Tolima',
    #'                       #'Tulua',
    #'                       #'Turbo',
    #'                       #'Valle Del Cauca',
    #'                       #'Valledupar',
    #'                       #'Yopal',
    #'                       'Yumbo')
    #'         )
    #'       )
    #'     )
    #'   ),
    #'   fluidRow(
    #'     box(
    #'       width = 12,
    #'       plotOutput(
    #'         outputId = 'inteligenciaMercado_competidores_recaudoPlot'
    #'       ),
    #'       plotOutput(
    #'         outputId = 'inteligenciaMercado_competidores_ventasPlot'
    #'       ),
    #'       dataTableOutput(
    #'         outputId = 'inteligenciaMercado_competidores_competenciaoTable'
    #'       )
    #'     )
    #'   )
    #' ),
    #' tabItem(
    #'   'recaudos',
    #'   fluidRow(
    #'     box(
    #'       width = 12,
    #'       title = 'Parámetros',
    #'       solidHeader = TRUE,
    #'       status = 'primary',
    #'       collapsible = TRUE,
    #'       column(
    #'         width = 3,
    #'         textInput(
    #'           inputId = 'inteligenciaMercado_recaudos_entidad',
    #'           label = 'Buscar entidad(es):',
    #'           value = ''
    #'         ),
    #'         actionButton(
    #'           inputId = 'inteligenciaMercado_recaudos_aplicar',
    #'           label = 'Revisar...'
    #'         )
    #'       ),
    #'       column(
    #'         width = 3,
    #'         textInput(
    #'           inputId = 'inteligenciaMercado_recaudo_entidad2',
    #'           label = 'Retirar entidad(es)'
    #'         ),
    #'         fileInput(
    #'           inputId = 'inteligenciaMercado_recaudo_file1',
    #'           label = 'Cargar archivo CSV para clientes:',
    #'           buttonLabel = 'Buscar...',
    #'           placeholder = 'No hay archivo',
    #'           accept = c('text/csv',
    #'                      'text/comma-separated-values',
    #'                      '.csv')
    #'         )
    #'       ),
    #'       column(
    #'         width = 3,
    #'         p('Entidades: Para utilizar multiples nombres, estos deben estar separados por "," y sin espacios.'),
    #'         p('Archivo cedulas: Se debe importar un archivo csv con una sola columna llamada "cedula" y sus respectivos números.')
    #'       ),
    #'       column(
    #'         width = 3,
    #'         p('Powered By: '),
    #'         img(class = 'logo-imagen', src = './assets/gbq-logo.png', alt = 'Google BigQuery', width = '200')
    #'       )
    #'     )
    #'   ),
    #'   fluidRow(
    #'     box(
    #'       width = 12,
    #'       solidHeader = TRUE,
    #'       tabBox(
    #'         width = 12,
    #'         tabPanel(
    #'           'Resultado',
    #'           column(
    #'             width = 12,
    #'             verbatimTextOutput(
    #'               outputId = 'inteligenciaMercado_recaudos_resultados',
    #'               placeholder = TRUE)
    #'             ),
    #'           column(
    #'             width = 12,
    #'             dataTableOutput(
    #'               outputId = 'inteligenciaMercado_recaudos_table'
    #'             ),
    #'             downloadButton(
    #'               inputId = 'inteligenciaMercado_recaudos_downloadData',
    #'               'Descargar detalle'
    #'             )
    #'           )
    #'         ),
    #'         tabPanel(
    #'           'Clientes Específicos',
    #'             column(
    #'               width = 12,
    #'               tableOutput(
    #'                 'inteligenciaMercado_recaudos_clientesEspecificos_contents'
    #'               )
    #'             )
    #'           )
    #'         )
    #'       )
    #'     )
    #' ),
    tabItem(
      tabName = 'bases-datos',
      fluidRow(
        box(
          width = 12,
          title = 'Parámetros',
          solidHeader = TRUE,
          # status = 'primary',
          collapsible = TRUE,
          column(
            width = 3,
            sliderInput(
              inputId = 'clientesPot_db_edad',
              label = 'Edad',
              min = 20,
              max = 70,
              value = 45
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = 'clientesPot_db_pagaduria',
              label = 'Pagaduría',
              choices = c(#'Antioquia',
                          #'Apartadó',
                          #'Armenia',
                          # 'Atlántico',
                          'Barranquilla',
                          #'Bello',
                          #'Bolívar',
                          #'Buga',
                          #'Caldas',
                          #'Cali',
                          #'Cartagena',
                          #'Casanare',
                          #'Cesar',
                          #'Cienaga',
                          #'Colpensiones',
                          #'Cordoba',
                          #'Cundinamarca',
                          #'Dosquebradas',
                          #'Fiduprevisora',
                          #'Fopep',
                          #'Ibagué',
                          #'Jamundi',
                          #'Lorica',
                          #'Magangue',
                          #'Magdalena',
                          #'Medellin',
                          #'Montería',
                          #'Pereira',
                          #'Risaralda',
                          #'Sabaneta',
                          #'Sahagun',
                          #'Sincelejo',
                          #'Soledad',
                          #'Sucre',
                          #'Tolima',
                          #'Tulua',
                          #'Turbo',
                          #'Valle Del Cauca',
                          #'Valledupar',
                          #'Yopal',
                          'Yumbo')
            )
          ),
          column(
            width = 3,
            selectInput(
              inputId = 'clientesPot_db_ciudades',
              label = 'Ciudad',
              choices = c(#'Antioquia',
                          #'Apartadó',
                          #'Armenia',
                          # 'Atlántico',
                          'Barranquilla',
                          #'Bello',
                          #'Bolívar',
                          #'Buga',
                          #'Caldas',
                          #'Cali',
                          #'Cartagena',
                          #'Casanare',
                          #'Cesar',
                          #'Cienaga',
                          #'Colpensiones',
                          #'Cordoba',
                          #'Cundinamarca',
                          #'Dosquebradas',
                          #'Fiduprevisora',
                          #'Fopep',
                          #'Ibagué',
                          #'Jamundi',
                          #'Lorica',
                          #'Magangue',
                          #'Magdalena',
                          #'Medellin',
                          #'Montería',
                          #'Pereira',
                          #'Risaralda',
                          #'Sabaneta',
                          #'Sahagun',
                          #'Sincelejo',
                          #'Soledad',
                          #'Sucre',
                          #'Tolima',
                          #'Tulua',
                          #'Turbo',
                          #'Valle Del Cauca',
                          #'Valledupar',
                          #'Yopal',
                          'Yumbo')
            )
          )
        )
      ),
      
      fluidRow(
        box(
          width = 12,
          solidHeader = TRUE,
          div(
            class = 'clientes-pot-db-table',
            withSpinner(
              dataTableOutput(
                outputId = 'clientesPot_db_table'
              )
            )
          ),
          downloadButton(
            outputId = 'clientesPot_db_downloadData',
            label = 'Descargar detalle'
          )
        )
      )
    )
  )
)

dashboardPage(
  dashboardHeader(
    title = img(class = 'main-logo', src = './assets/logotipo_finso_analitica.svg')
    ),
  skin = 'black',
  sidebar,
  body
)