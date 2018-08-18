# SPI Tool based on PRISM data
# 12/21/15

# SPI Explorer Tool v1.2 8/18/17
# added SPI selector 1-48 mos
# added multi-scale plot
# temps on site summary
# gantt chart, other minor labels

# v 1.2.1 December 12/20/17 ...added table download...
# v 1.2.2 01/02/18 fixed Jan bug with current year on download, line 130

## NON-INTERACTIVE
library(RColorBrewer)
library(plotly)
library(RCurl)
library(jsonlite)
library(SPEI)
library(reshape)
library(shiny)
library(ggplot2)
library(leaflet)
library(zoo)
library(gridExtra)

# initial point for marker
latIn<-32
lonIn<--110

# tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
#  includeCSS("styles.css"),
## UI section  

ui<-tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),  
  tags$head(HTML(
    "<!-- Global site tag (gtag.js) - Google Analytics -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=UA-108499551-1'></script>
    <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag('js', new Date());
    
    gtag('config', 'UA-108499551-1');
    </script>"
  )),
  navbarPage(strong("Standardized Precipitation Index Explorer Tool"),
             
             tabPanel("About Tool",
                      sidebarLayout(
                        sidebarPanel(HTML('<body>
                                          Links to other SPI resources:<br>
                                          <ul>
                                          <li><a href="http://www.wrcc.dri.edu/spi/explanation.html">Western
                                          Regional Climate Center</a></li>
                                          <li><a
                                          href="http://www.wrcc.dri.edu/wwdt/index.php?folder=spi1">WestWide
                                          Drought Tracker</a></li>
                                          <li><a
                                          href="http://iridl.ldeo.columbia.edu/maproom/Global/Precipitation/SPI.html">International
                                          Research Institute for Climate and Society</a></li>
                                          <li><a
                                          href="https://www.drought.gov/drought/content/products-current-drought-and-monitoring-drought-indicators/standardized-precipitation-index">Drought.gov</a></li>
                                          <li><a
                                          href="http://drought.unl.edu/Planning/Monitoring/ComparisonofIndicesIntro/SPI.aspx">National
                                          Drought Mitigation Center</a></li>
                                          </ul>
                                          </body>'
                        )
                        ),
                        mainPanel(
                          h4("About the SPI Explorer Tool"),
                          HTML('<body>
                               The Standardized Precipitation Index (SPI) is a widely used drought
                               index that has several strenghts including the ability to calculate
                               precipitation anomalies at different timescales and the ability to
                               interpret SPI units (standard deviations) in probabilistic terms (<a
                               href="http://drought.unl.edu/monitoringtools/climatedivisionspi.aspx">click
                               here for more info on the SPI</a>). This tool was created to
                               explore SPI values at specific locations by using a gridded climate
                               dataset (<a href="http://www.prism.oregonstate.edu/">PRISM
                               Climate</a>) to estimate local precipitation time series. Data
                               are accessed through the <a
                               href="http://www.rcc-acis.org/docs_webservices.html">Applied
                               Climate Information Web Service</a> and analyzed and plotted
                               using several R based packages.&nbsp;
                               </body>
                               '),
                          hr(),
                          HTML('<table
                               style="width: 90%; height: 75px; text-align: left; margin-left: auto; margin-right: auto;"
                               border="0" cellpadding="2" cellspacing="2">
                               <tbody>
                               <tr>
                               <td style="text-align: center; width: 373px;"><a
                               href="http://cals.arizona.edu/climate"><img
                               style="border: 0px solid ; width: 121px; height: 50px;"
                               alt="cals"
                               src="cals.jpg"></a></td>
                               <td style="text-align: center; width: 405px;"><a
                               href="http://www.climas.arizona.edu"><img
                               style="border: 0px solid ; width: 62px; height: 58px;"
                               alt="climas"
                               src="climas.png"></a></td>
                               <td style="text-align: center; width: 442px;"><a
                               href="http://www.rcc-acis.org/"><img
                               style="border: 0px solid ; width: 195px; height: 25px;"
                               alt="acis"
                               src="acis_logo.png"></a></td>
                               </tr>
                               </tbody>
                               </table>'
                          ),
                          HTML('<div style="text-align: center;">Contact Mike Crimmins (<a
                               href="mailto:crimmins@email.arizona.edu">crimmins@email.arizona.edu</a>)
                               with questions or comments. SPI Explorer Tool v1.2.2 01/02/18</div>'
                          )
                          )
                          )
                          ),                   
             tabPanel("Set location/time period",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Set location and time period"),
                          p("1. Click map to select location (use +/- buttons to zoom, use cursor to pan)"),
                          p("2. Adjust years to time period of interest"),
                          p("3. Click download (this may take a couple of seconds, look to upper right corner for progress message)"),
                          numericInput("yearFirst","First year:",value = 1895, min=1895, max=(format(Sys.Date()-32, "%Y")), step=1, width = "100px"),
                          numericInput("yearLast","Last year:",value = format(Sys.Date()-32, "%Y"), min=1896, max=format(Sys.Date()-32, "%Y"), step=1, width = "100px"),
                          actionButton("refresh","Download data"),
                          hr(),
                          p("All statistics and figures on other pages are calculated based on the location and time period specified here. Time periods of at least 30 years will have more stable estimates of long-term climate statistics.")
                          
                        ),
                        mainPanel(
                          leafletOutput("MyMap",width = "700px", height = "700px"),
                          verbatimTextOutput("latSel"),
                          verbatimTextOutput("lonSel")
                        )
                      )
             ),
             tabPanel("Site Climate Summary",
                      sidebarLayout(
                        sidebarPanel(h4("Climate Summary"),
                                     hr(),
                                     tags$b("Site Description"),
                                     p(textOutput("elev")),
                                     p(textOutput("annavg")),
                                     p(textOutput("annavgTemp")),
                                     hr(),
                                     p("The annual total precipitation for the selected location and time period are depicted in this
                                       figure as a simple way to visualize longer-term climate variability including wet periods and droughts.
                                       The data table below the figure can be re-sorted to find driest and wettest years. Click on
                                       the arrows next to the column names to change the sort order for that column."),
                                     hr(),
                                     downloadButton("downloadAnnData", "Download Data Table"),
                                     hr()
                                     ),
                        mainPanel(
                          plotOutput("annPlot"),
                          plotOutput("annPlotTemp"),
                          dataTableOutput('annTable')
                        )
                        )
                      ),
             tabPanel("SPI Timescale Comparison",
                      sidebarLayout(
                        sidebarPanel(h4("Comparing different SPI timescales"),
                                     p("The monthly SPI values for the specified location and time period are depicted in the bar plots on the right.
                                       The top figure is the 1-month SPI which has no moving window used in the calculation.
                                       The middle (3-month SPI) and bottom (12-month SPI) figures use moving windows of 3 and 12 months
                                       respectively to quantify precipitation anomalies at these longer timescales. For example,
                                       the 3 month SPI would be sensitive to seasonally varying amounts of precipitation helping
                                       to detect short-term changes in drought conditions that may occur over a summer season while
                                       the 12-month SPI would smooth out seasonal changes and better capture variability at the annual scale."),
                                     p("The heatmap below shows all SPI values for timescales from 1-48 months for the specified location and time period.
                                       Use the cursor to hover over the plot to find the monthly SPI value for any timescale and date. Note how longer
                                       timescale SPI values vary much slower over time with longer wet spells and drought events. Shorter timescales
                                       vary much more rapidly over time and can be a good indicator of short-term drought conditions on the order
                                       of months to seasons. This multiscale-SPI plot can also depict when there is a disconnect between short and long
                                       term drought conditions. Sometimes drought conditions at longer SPI timescales can persist even when short
                                       term conditions have improved or vice versa. By looking at all SPI timescales for a given date, you can diagnose 
                                       whether short or long term drought conditions are present or a combination of both.")
                                     ),
                        mainPanel(
                          plotOutput("spiPlots"),
                          hr(),
                          plotlyOutput("plotMultiSPI")
                        )
                                     )
                        ),
             tabPanel("SPI-Precip Comparison",
                      sidebarLayout(
                        sidebarPanel(
                          # radioButtons("precipSum","Choose SPI timescale",
                          #              c("1 month"="1","3 month"="2","12 month"="3"), selected = ),
                          sliderInput("precipSum", "Choose SPI timescale (1-48 mos)", min=1,max=48,value=3,step=1),
                          sliderInput("mo1sum",
                                      "Choose end month",
                                      min = 1,
                                      max = 12,
                                      value = 7,
                                      step=1),
                          hr(),
                          downloadButton("downloadMoData", "Download Data Table"),
                          hr()
                        ),
                        mainPanel(
                          plotOutput("monthSPIplot"),
                          plotOutput("monthPlot"),
                          plotOutput("histoPlot"),
                          plotOutput("ecdfPlot"),
                          hr(),
                          h4("Precipitation Statistics"),
                          textOutput("avgPrecip"),
                          tags$head(tags$style("#avgPrecip{color:black;font-size: 16px;font-style: bold;}")),
                          textOutput("minPrecip"),
                          tags$head(tags$style("#minPrecip{color:black;font-size: 16px;font-style: bold;}")),
                          textOutput("maxPrecip"),
                          tags$head(tags$style("#maxPrecip{color:black;font-size: 16px;font-style: bold;}")),
                          h4("Precipitation terciles - use to interpret seasonal climate outlooks"),
                          textOutput("belowPrecip"),
                          tags$head(tags$style("#belowPrecip{color:#663300;font-size: 16px;font-style: bold;}")),
                          textOutput("medianPrecip"),
                          tags$head(tags$style("#medianPrecip{color:black;font-size: 16px;font-style: bold;}")),
                          textOutput("abovePrecip"),
                          tags$head(tags$style("#abovePrecip{color:#006600;font-size: 16px;font-style: bold;}")),
                          hr(),
                          dataTableOutput('moTable')
                        )
                      )
             ),
             tabPanel("Drought Category Transitions",
                      sidebarLayout(
                        sidebarPanel(
                          # radioButtons("SPIState1","Period 1 SPI timescale:",
                          #              c("1 month"="8","3 month"="9","12 month"="10"), selected = ),
                          sliderInput("SPIState1", "Period 1 SPI timescale (1-12 mos):", min=1,max=12,value=3,step=1),
                          sliderInput("mo1",
                                      "Period 1 - End Month:",
                                      min = 1,
                                      max = 12,
                                      value = 7,
                                      step=1),
                          # radioButtons("SPIState2","Period 2 SPI timescale:",
                          #              c("1 month"="8","3 month"="9","12 month"="10")),
                          sliderInput("SPIState2", "Period 2 SPI timescale (1-12 mos):", min=1,max=12,value=3,step=1),
                          sliderInput("mo2",
                                      "Period 2 - End Month:",
                                      min = 1,
                                      max = 12,
                                      value = 9,
                                      step=1),
                          hr(),
                          p("This tool examines the probability of moving from one SPI based drought 
                            category to another based on the historical precipitation record at this
                            location. The chart is read as the row indicating the drought category for
                            the initial month and the column as 'forecast' month. For example, if you were
                            interested in how often a very wet July was followed by an overall very wet summer (July-Aug-Sep) 
                            precipitation total, you would set Month 1 to 1-month SPI and July and Month 2 to 3-month SPI and September.
                            Reading across the first row (month 1-very wet) to the last column (month 2-very wet) indicates the probability
                            of this outcome based on historical occurrences. A longer period of record yields more stable results.")
                          ),
                        mainPanel(
                          plotOutput("transPlot"),
                          #plotOutput("gantt"),
                          hr(),
                          p(""),
                          fluidRow(
                            column(12, align="center",
                                   tableOutput('spiBoundsTable')
                            )
                          )
                        )
                          )
                      )
             
             
             
             )
             )
## Server Section

server <- function(input, output, session) {
  # add in leaflet map, overlay PRISM avg precip map or DEM grid...
  output$MyMap <- renderLeaflet({
    m <- leaflet() %>% setView(lng = -111.740979, lat = 34.394685, zoom = 7)
    m %>% addProviderTiles("Esri.WorldTopoMap")
  })
  
  #  output$out <- renderPrint({
  #    validate(need(input$MyMap_click, FALSE))
  #    str(input$MyMap_click)
  #     })
  
  observeEvent(input$MyMap_click, {
    leafletProxy("MyMap")%>% clearMarkers() %>%
      addMarkers(input$MyMap_click$lng, input$MyMap_click$lat)
    latIn<-input$MyMap_click$lat
    lonIn<-input$MyMap_click$lng
    output$latSel<-renderText({paste("Latitude: ",latIn)})
    output$lonSel<-renderText({paste("Longitude: ",lonIn)})
  })
  
  
  # download and process data  
  observeEvent(input$refresh, {
    withProgress(message = 'Downloading data set', style="old",
                 detail = 'Please wait...',{
                   lat=input$MyMap_click$lat # input from map
                   lon=input$MyMap_click$lng # input from map
                   firstYR=as.numeric(input$yearFirst)
                   lastYR=as.numeric(input$yearLast)
                   jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"',firstYR,'01","edate":"',lastYR,'12","grid":"21",
                                    "elems":[{"name":"mly_maxt","units":"degreeF"},{"name":"mly_mint","units":"degreeF"},
                                    {"name":"mly_avgt","units":"degreeF"},{"name":"mly_pcpn","units":"inch"}]}')
                   out<-postForm("http://data.rcc-acis.org/GridData", 
                                 .opts = list(postfields = jsonQuery, 
                                              httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                   out<-fromJSON(out)
                   
                   # meta output
                   jsonQuery=paste0('{"loc":"',lon,',',lat,'","grid":"21","elems":"4","meta":"ll,elev","date":"2010-01-01"}')
                   metaOut<-postForm("http://data.rcc-acis.org/GridData", 
                                     .opts = list(postfields = jsonQuery, 
                                                  httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                   metaOut<-fromJSON(metaOut)
                   output$elev<-renderText({paste("Elevation (ft): ",as.character(metaOut$meta$elev))})
                   
                   # get data from list
                   data<-data.frame(out$data)
                   colnames(data)<-c("date","t_max","t_min","t_mean","precip")
                   data$date<-as.Date(paste(data$date,"-01",sep=""))
                   # set -999 to NA
                   data[data == -999] <- NA
                   
                   # convert columns to numeric
                   unfactorize<-c("t_max","t_min","t_mean","precip")
                   data[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(data[,x])))
                   
                   # get months and years
                   data$month<-as.numeric(format(data$date, "%m"))
                   data$year<-as.numeric(format(data$date, "%Y"))
                   
                   ## Loop thru full SPI set
                   for(i in 1:48){
                     tempSPI <- spi(data$precip,i, na.rm = TRUE)
                     data[[paste('spi',i,sep="")]] <-tempSPI$fitted
                   }
                   
                   # # calculate SPI
                   # spi1<-spi(data$precip,1, na.rm = TRUE)
                   # data$spi1<-spi1$fitted
                   # spi3<-spi(data$precip,3, na.rm = TRUE)
                   # data$spi3<-spi3$fitted
                   # spi12<-spi(data$precip,12, na.rm = TRUE)
                   # data$spi12<-spi12$fitted
                   
                   # # calculate raw precipitation totals, % of avg
                   # data$precip1sum<-data$precip
                   # temp<-rollapply(data$precip,3, sum)
                   # data$precip3sum= c(rep(NA, nrow(data) - length(temp)),temp)
                   # temp<-rollapply(data$precip,12, sum)
                   # data$precip12sum= c(rep(NA, nrow(data) - length(temp)),temp)
                   
                   ## Loop thru full raw precip totals
                   data$precip1sum<-data$precip
                   for(i in 2:48){
                     tempSum<-rollapply(data$precip,i, sum)
                     data[[paste('precip',i,'sum',sep="")]] <-c(rep(NA, nrow(data) - length(tempSum)),tempSum)
                   }
                   
                   # choose SPI windows, create groups
                   breaks<-c(-999,-1,0,1,999) # 4 breaks
                   lblText<-c("1"="very dry (<-1)","2"="dry (-1 to 0)","3"="wet (0 to 1)","4"="very wet(>1)")
                   lims<-c(1:4) # lims for plot
                   
                   # trim data without precip data = NA
                   dataTrim<-data
                   if (lastYR==as.numeric(format(Sys.Date(), "%Y"))){
                     data<-data[-(min(which(is.na(data$precip))):nrow(data)), ] # breaks Drought Category tool past current month
                     # trim again to previous full year
                     dataTrim<-data[-(which(data$year==as.numeric(format(Sys.Date(), "%Y")))),]
                   }else{
                     
                   }
                   
  })
    
    ### END INTERACTIVE
    
    # annual precip time series and table & temperature
    
    # ann temp
    tempAvgAnn<-data[,c(6,4)]
    tempAvg<-rollmean(tempAvgAnn$t_mean,12)
    tempAvgAnn$annTemp <-c(rep(NA, nrow(tempAvgAnn) - length(tempAvg)),tempAvg)
    annTemp<-tempAvgAnn[ which(tempAvgAnn$month==12), ]
    
    # combined
    annData<- data[ which(data$month==12), ]
    annData<- annData[,c("year","precip12sum")]
    annData$annAvgTemp<- annTemp$annTemp
    output$annavg<-renderText({paste("Avg Annual Precip (in): ",round(mean(annData[,c("precip12sum")], na.rm = TRUE),1))})
    output$annavgTemp<-renderText({paste("Avg Annual Temp (F): ",round(mean(annData$annAvgTemp, na.rm = TRUE),1))})
    
    output$annPlot<-renderPlot({
      ggplot(annData,aes(x=year, y=annData[,c("precip12sum")]))+
        geom_bar(stat='identity', fill='springgreen4', color='grey')+
        labs(y="precip (in.)", title='Annual Total Precipitation')+
        geom_hline(aes(yintercept=(mean(annData[,c("precip12sum")], na.rm = TRUE))))+
        geom_text(aes(firstYR,(mean(annData[,c("precip12sum")], na.rm = TRUE)),label ='average', vjust = -1)) 
    })
    
    output$annPlotTemp<-renderPlot({
      ggplot(annData,aes(x=year, y=annData$annAvgTemp))+
        geom_bar(stat='identity', fill='coral3', color='grey')+
        labs(y="deg F", title='Annual Average Temperature')+
        geom_hline(aes(yintercept=(mean(annData$annAvgTemp, na.rm = TRUE))))+
        geom_text(aes(firstYR,(mean(annData$annAvgTemp, na.rm = TRUE)),label ='average', vjust = -1))+
        coord_cartesian(ylim=c(min(annData$annAvgTemp, na.rm = TRUE)-1, max(annData$annAvgTemp, na.rm = TRUE)+1))
      
    })
    
    annDataTemp<-annData
    annDataTemp$annAvgTemp<-round(annDataTemp$annAvgTemp,1)
    colnames(annDataTemp)[2] <- "Total Precip"
    colnames(annDataTemp)[3] <- "Avg Temp"
    output$annTable<-renderDataTable({annDataTemp})
    
    ## --- Add Download Annual Data 
    # Downloadable csv of selected dataset ----
    output$downloadAnnData <- downloadHandler(
      filename = function() {
        paste0("AnnualClimateData_",lat,"_",lon, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(annDataTemp, file, row.names = FALSE)
      }
    )    
    
    
    ## SPI monthly time series
    tempSPI<-melt.data.frame(data, id.vars="date", variable.name = "year", measure.vars =c("spi1", "spi3","spi12"))
    tempSPI$pos<-tempSPI$value >=0
    levels(tempSPI$variable) <- c("1-month SPI", "3-month SPI", "12-month SPI")
    output$spiPlots<-renderPlot({
      ggplot(tempSPI, aes(x=date,y=value, fill=pos))+
        geom_bar(stat = "identity", position = "identity")+
        scale_fill_manual(values = c("#8c510a","#01665e"), guide=FALSE)+
        facet_wrap(~ variable, ncol = 1)+
        labs(x='month/year',y='SPI', title='Standardized Precipitation Index - 1/3/12 month')+
        theme_bw()
    })
    
    
    # multiscale SPI ggplot
    tempSPIall<-melt.data.frame(data, id.vars="date", variable.name = "year", measure.vars =c(8:55))
    tempSPIall$variable<-as.numeric(gsub("\\D", "", tempSPIall$variable))
    output$plotMultiSPI <- renderPlotly({
      plot_ly(tempSPIall, x = ~date, y = ~variable, z = ~value, colors=brewer.pal(11,'BrBG'), type = "heatmap", zmin=-3, zmax=3) %>%
        layout(title = "Multi-scale SPI Plot",
               xaxis=list(title="Month-Year"),
               yaxis=list(title="Scale(mos)")
        )
      
    })
    
    # Explore SPI/month sum 
    # subset month -- outside of PRISM refresh
    observe({
      dataSubset <- data[ which(data$month==input$mo1sum), ] # set to input
      # further reduce dataframe based on 1,3,12 month selection
      monthSelect<-as.integer(input$precipSum)  # "1 month"="1","3 month"="2","12 month"="3"
      
      # plot of precip time series
      output$monthPlot<-renderPlot({
        ggplot(dataSubset,aes(x=year, y=dataSubset[,55+monthSelect]))+
          geom_bar(stat='identity', fill='springgreen4', color='grey')+
          labs(y="precip (in.)", title='Total Precipitation')+
          geom_hline(aes(yintercept=(mean(dataSubset[,55+monthSelect], na.rm = TRUE))))+
          geom_text(aes(firstYR,(mean(dataSubset[,55+monthSelect], na.rm = TRUE)),label ='average', vjust = -1))+
          theme(text = element_text(size = 20))
      })
      
      # plot of month SPI
      tempMO<-melt.data.frame(dataSubset, id.vars = "date", variable.name ="year", measure.vars = (7+monthSelect))
      tempMO$pos<-tempMO$value >=0
      output$monthSPIplot<-renderPlot({
        ggplot(tempMO, aes(x=date,y=value, fill=pos))+
          geom_bar(stat = "identity", position = "identity")+
          scale_fill_manual(values = c("orange4","chartreuse4"), guide=FALSE)+
          labs(y="SPI", title='Standardized Precipitation Index')+
          theme(text = element_text(size = 20))
      })
      
      # plot of distribution
      output$histoPlot<-renderPlot({
        ggplot(dataSubset,aes(x=dataSubset[,55+monthSelect]))+
          geom_histogram(fill='grey',color='black')+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.33, na.rm=TRUE),color="chocolate4", size=1.5)+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.50, na.rm=TRUE), size=1.5)+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.66, na.rm=TRUE), color="chartreuse4", size=1.5)+
          scale_x_continuous(expand = c(0, 0))+ 
          scale_y_continuous(expand = c(0, 0))+
          labs(x='precip(in.)', y='count',title='Frequency of Precipitation Values')+
          theme(text = element_text(size = 20))
      })
      
      # plot of ecdf
      output$ecdfPlot<-renderPlot({
        ggplot(dataSubset,aes(x=dataSubset[,55+monthSelect]))+
          stat_ecdf()+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.33, na.rm=TRUE),color="chocolate4", size=1.5)+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.50, na.rm=TRUE), size=1.5)+
          geom_vline(xintercept=quantile(dataSubset[,55+monthSelect],0.66, na.rm=TRUE), color="chartreuse4", size=1.5)+
          labs(x='precip(in.)', y='cumulative probability',title='Cumulative probability of precip values')+
          theme(text = element_text(size = 20))
      })    
      
      # write out percentiles
      output$belowPrecip<-renderText({paste("Below median (33%tile, inches): ",round(quantile(dataSubset[,55+monthSelect],0.33, na.rm=TRUE),1))})
      output$medianPrecip<-renderText({paste("Median (50%tile, inches): ",round(quantile(dataSubset[,55+monthSelect],0.50, na.rm=TRUE),1))})
      output$abovePrecip<-renderText({paste("Above median (66%tile, inches): ",round(quantile(dataSubset[,55+monthSelect],0.66, na.rm=TRUE),1))})
      output$avgPrecip<-renderText({paste("Average (inches): ",round(mean(dataSubset[,55+monthSelect], na.rm = TRUE),2))})
      output$maxPrecip<-renderText({paste("Max (inches): ",round(max(dataSubset[,55+monthSelect], na.rm = TRUE),2))})
      output$minPrecip<-renderText({paste("Min (inches): ",round(min(dataSubset[,55+monthSelect], na.rm = TRUE),2))})
      
      # output data table
      tempMonthTable<-cbind(dataSubset[,7],dataSubset[,55+monthSelect],round(dataSubset[,7+monthSelect],2), 
                            round(dataSubset[,55+monthSelect]/mean(dataSubset[,55+monthSelect], na.rm = TRUE)*100,0))
      colnames(tempMonthTable)<-c("year","Precip","SPI","% of Avg Precip")
      output$moTable<-renderDataTable({tempMonthTable})
      
      ## --- Add Download Monthly Data 
      # Downloadable csv of selected dataset ----
      output$downloadMoData <- downloadHandler(
        filename = function() {
          paste0(month.abb[input$mo1sum],"_",monthSelect,"mo_","ClimateData_",lat,"_",lon, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(tempMonthTable, file, row.names = FALSE)
        }
      )   
      
      
    })
    
    
    # Transition Prob Plot -- replaced with dataTrim 
    output$transPlot<- renderPlot({
      
      state1spi<-as.integer(input$SPIState1) # 8=spi1, 9=spi3, 10=spi12
      state2spi<-as.integer(input$SPIState2) # 8=spi1, 9=spi3, 10=spi12
      state1<-cut(as.numeric(dataTrim[,state1spi+7]),breaks, labels=c(1:4))  # SET SPI state 1, labels
      state2<-cut(as.numeric(dataTrim[,state2spi+7]),breaks, labels=c(1:4))  # SET SPI state 2, labels
      
      # choose months to compare
      #spiStates<-cbind.data.frame(data$month, data$year, state1, state2)
      spiStates<-cbind.data.frame(dataTrim$month, dataTrim$year, state1, state2, dataTrim[,state1spi+7], dataTrim[,state2spi+7], dataTrim[,state1spi+55],dataTrim[,state2spi+55])
      colnames(spiStates)[5] <- "spi1"
      colnames(spiStates)[6] <- "spi2"
      colnames(spiStates)[7] <- "precip1"
      colnames(spiStates)[8] <- "precip2"
      
      # select months    
      mo1=input$mo1
      mo2=input$mo2
      state1<-spiStates[which(dataTrim$month==mo1),] # SET month 1
      state1<-state1["state1"]
      state2<-spiStates[which(dataTrim$month==mo2),] # SET month 2
      state2<-state2["state2"]
      
      # date label logic for labels
      
      if (mo1 > mo2){
        startYr<-"2016-01-01"  
      }
      else{
        startYr<-"2017-01-01"
      }
      
      p1en<-as.Date(format(as.yearmon(startYr)+((mo1)/12), "%Y-%m-%d"))-1
      p1st<-as.Date(format(as.yearmon(startYr)+((mo1)/12)-((state1spi)/12), "%Y-%m-%d"))
      
      p2en<-as.Date(format(as.yearmon("2017-01-01")+((mo2)/12), "%Y-%m-%d"))-1
      p2st<-as.Date(format(as.yearmon("2017-01-01")+((mo2)/12)-((state2spi)/12), "%Y-%m-%d"))
      
      # dynamic labels
      ylabText<-paste0("Period 1: SPI-",state1spi," (", format(p1st,"%b"),"-", format(p1en,"%b"),")")
      xlabText<-paste0("Period 2: SPI-",state2spi," (", format(p2st,"%b"),"-", format(p2en,"%b"),")")
      
      # gantt chart for SPI periods ----
      task1 <- c('Period 1', format(p1st,"%Y-%m-%d"),format(p1en,"%Y-%m-%d"))
      task2 <- c('Period 2', format(p2st,"%Y-%m-%d"),format(p2en,"%Y-%m-%d"))
      #task2 <- c('Period 2', paste0('2017-',mo2-(state2spi)+1,'-01') ,paste0('2017-',mo2+1,'-01'))
      
      df <- as.data.frame(rbind(task1, task2))
      names(df) <- c('task', 'start', 'end')
      df$task <- factor(df$task, levels = c("Period 2","Period 1"))
      df$start <- as.Date(df$start)
      df$end <- as.Date(df$end)
      df_melted <- melt(df, measure.vars = c('start', 'end'))
      
      # starting date to begin plot
      start_date <- p1st-30
      
      # ggplot(df_melted, aes(value, task)) +
      #   geom_line(size = 10) +
      #   labs(x = '', y = '', title = 'SPI Timescales') +
      #   theme_bw(base_size = 20) +
      #   theme(plot.title = element_text(hjust = 0.5),
      #         panel.grid.major.x = element_line(colour="black", linetype = "dashed"),
      #         panel.grid.major = element_blank(),
      #         panel.grid.minor = element_blank(),
      #         axis.text.x = element_text(angle = 0)) +
      #   scale_x_date(date_labels = "%b", limits = c(start_date, NA), date_breaks = '1 month')
      # ---- end gantt chart
      
      # new code - write out SPI bounds in inches
      state1temp<-spiStates[which(dataTrim$month==mo1),] # SET month 1 
      state2temp<-spiStates[which(dataTrim$month==mo2),] # SET month 2
      spiBounds<-data.frame()
      temp<-state1temp[which.min(abs(state1temp$spi1-1)), ]
      spiBounds[1,1]<-temp[1,7]
      temp<-state1temp[which.min(abs(state1temp$spi1-0)), ]
      spiBounds[2,1]<-temp[1,7]
      temp<-state1temp[which.min(abs(state1temp$spi1-(-1))), ]
      spiBounds[3,1]<-temp[1,7]    
      temp<-state2temp[which.min(abs(state2temp$spi2-1)), ]
      spiBounds[1,2]<-temp[1,8]
      temp<-state2temp[which.min(abs(state2temp$spi2-0)), ]
      spiBounds[2,2]<-temp[1,8]
      temp<-state2temp[which.min(abs(state2temp$spi2-(-1))), ]
      spiBounds[3,2]<-temp[1,8]      
      # format output table    
      spiBoundsOut<-data.frame()
      spiBoundsOut[1:4,1]<-c("very dry (<-1)","dry (-1 to 0)","wet (0 to 1)","very wet(>1)") 
      spiBoundsOut[1,2]<-paste("< ",as.character(spiBounds[3,1])," in.")
      spiBoundsOut[2,2]<-paste(as.character(spiBounds[3,1])," to ",as.character(spiBounds[2,1])," in.")
      spiBoundsOut[3,2]<-paste(as.character(spiBounds[2,1])," to ",as.character(spiBounds[1,1])," in.")
      spiBoundsOut[4,2]<-paste("> ",as.character(spiBounds[1,1])," in.")
      spiBoundsOut[1,3]<-paste("< ",as.character(spiBounds[3,2])," in.")
      spiBoundsOut[2,3]<-paste(as.character(spiBounds[3,2])," to ",as.character(spiBounds[2,2])," in.")
      spiBoundsOut[3,3]<-paste(as.character(spiBounds[2,2])," to ",as.character(spiBounds[1,2])," in.")
      spiBoundsOut[4,3]<-paste("> ",as.character(spiBounds[1,2])," in.")
      colnames(spiBoundsOut)<-c("Category","Period 1","Period 2")
      output$spiBoundsTable<-renderTable({spiBoundsOut})
      
      
      ## NEW BUG FIX deal with use case mo1>mo2...not quite working
      if (mo1 > mo2){
        state2<-as.data.frame(state2[2:nrow(state2),])
        state2[nrow(state2)+1,]<-NA
        colnames(state2)<-"state2"
      }
      ##  
      # combine and convert to factors, cross tabulate
      states<-cbind.data.frame(state1,state2)
      states<-table(states) # counts
      states<-prop.table(states,1) # row proportions 1, col=2
      states<-states*100
      
      # data table for reference?
      
      # dynamic title
      droughtTitle<-paste0("Drought Category Transition Probabilities: ",min(spiStates$`dataTrim$year`),"-",max(spiStates$`dataTrim$year`))
      
      # plot table
      plotStates<-as.data.frame(melt(states))
      p1<-ggplot(data=plotStates)+
        geom_tile(aes(x=state2,y=state1,fill=value))+
        geom_text(aes(x=state2,y=state1,fill=value, label = round(value, 1)), size=8)+
        scale_fill_gradient2(low="white", mid="yellow",high="orangered", midpoint = 25,
                             guide = guide_legend(title="probability(%)"))+
        scale_x_discrete(labels=lblText, limits=lims)+
        scale_y_discrete(labels=lblText, limits=lims)+
        labs(x = xlabText, y=ylabText, title=droughtTitle)+
        theme(axis.text = element_text(size =14))+
        theme(axis.title = element_text(size=14))+
        theme(axis.text.x = element_text(size=14))+
        theme(axis.text.y = element_text(size=14))+
        theme(legend.position = "none") # can change to left or right
      
      # output$gantt<-renderPlot({
      p2<-ggplot(df_melted, aes(value, task)) +
        geom_line(size = 5, aes(color=task)) +
        scale_colour_manual(values=c("Period 1"="grey56", "Period 2"="grey19"))+
        labs(x = '', y = '', title = 'SPI Timescales') +
        theme_bw(base_size = 20) +
        theme(plot.title = element_text(hjust = 0.5, size = 12),
              panel.grid.major.x = element_line(colour="grey73", linetype = "dashed"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 0, size=14),
              axis.text.y = element_text(size=14),
              legend.position="none") +
        scale_x_date(date_labels = "%b", date_breaks = '1 month') #limits = c(NA, NA)
      
      #})
      grid.arrange(p1, p2, ncol = 1, heights = unit(c(0.65, 0.35),"npc")) # heights = c(3, 1)
      
    })
    
    
})
  
}


## Call app
shinyApp(ui=ui, server=server)