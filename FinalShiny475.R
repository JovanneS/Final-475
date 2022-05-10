library(shiny)
library(shinythemes)
library(fpp3)
library(dplyr)
library(shinyWidgets)
library(shiny)
library(ggeasy)
library(ggplot2)
library(shinydashboard)
library(forecast)
library(plotly)

data("tourism")

library(shiny)


ui <- navbarPage(strong("Interactive Tourism App for the Regions in Australia", style = "color:White"), fluid = TRUE,
                 theme = bslib::bs_theme(bootswatch = "darkly"),
                 tabPanel("Time Series Plots",
                          sidebarLayout(
                              sidebarPanel(
                                  awesomeRadio(
                                      inputId = "Choice",
                                      label = "Choose Plot to View", 
                                      choices = c("Seasonality", 
                                                  "Autocorrelation", "Decomposition"),
                                      selected = "Seasonality"
                                  ),
                                  pickerInput(
                                      inputId = "Region",
                                      label = "Pick a Region", 
                                      choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula") 
                                  ),
                                  
                                  
                                  pickerInput(
                                      inputId = "Purpose",
                                      label = "Pick a Purpose", 
                                      choices = c("Business", "Holiday", "Other", "Visiting" )
                                      
                                  ), 
                              ),
                              mainPanel(
                                  p("Welcome! Displayed below is the time series plot as well as a secondary plot that can display a plot for the seasonality, autocorrelation and decomposition. All you have to do is click on which secondary plot you would like to view. You can change the region and the purpose of a trip that will change the plots accordingly. Also, I have provided interpretations for the seasonality, decomposition, and autocorrelation plots. The interpertations are only given for the region Adelaide and purpose of the trip being for business. Thank you and enjoy."),
                                  plotOutput("Hist"),
                                  
                                  
                                  plotOutput("Hist2"),
                                  
                                  textOutput("text1"),
                              ),
                          )
                 ),
                 tabPanel("Simple Models",
                          sidebarLayout(
                              sidebarPanel(
                                  radioGroupButtons(
                                      inputId = "Model",
                                      label = "Which model would you look to see?", 
                                      choices = c("Naive", "Seasonal Naive", "Mean", "Drift"),
                                      status = "primary"
                                  ),
                                  
                                  
                                  pickerInput(
                                      inputId = "purpose1",
                                      label = "Pick a Purpose", 
                                      choices = c("Business", "Holiday", "Other", "Visiting" )
                                      
                                  ),
                                  pickerInput(
                                      inputId = "region1",
                                      label = "Pick a Region", 
                                      choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula") 
                                  ),
                              ),
                              mainPanel(
                                  p("Below you have the option to choose between four different simple models: the Naive model,the Seasonal Naive model, the Mean model, and the Drift model. As before, you can change the region and the purpose of a trip that will change the plots accordingly. Thank you and enjoy!"), 
                                  plotOutput("Hist3"),
                              ),
                              
                          )
                 ),
                 tabPanel("Holts and Holts/Winters",
                     sidebarLayout(
                         sidebarPanel(
                             radioGroupButtons(
                                 inputId = "Holts1",
                                 label = "Which plot would you look to see?", 
                                 choices = c("Holts", "Holts/Winters"),
                                 status = "primary"
                             ),
                             
                             
                             pickerInput(
                                 inputId = "purpose",
                                 label = "Pick a Purpose", 
                                 choices = c("Business", "Holiday", "Other", "Visiting" )
                                 
                             ),
                             pickerInput(
                                 inputId = "region",
                                 label = "Pick a Region", 
                                 choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula") 
                             ),
                         ),
                        mainPanel(                                  
                            p("Below you have the option to choose between the Holts model and the Holts/Winters Model. As before, you can change the region and the purpose of a trip that will change the plots accordingly. Thank you and enjoy!"), 
                            plotOutput("Hist4"),
                        ),
                             
                         )
                     ),
                 tabPanel("Arima",
                          sidebarLayout(
                              sidebarPanel(
                                  radioGroupButtons(
                                      inputId = "Arima",
                                      label = "Please choose a parameter:", 
                                      choices = c("Manual Perameter", "Auto Perameter"),
                                      status = "primary"
                                  ),
                                  
                                  
                                  pickerInput(
                                      inputId = "purpose2",
                                      label = "Pick a Purpose", 
                                      choices = c("Business", "Holiday", "Other", "Visiting" )
                                      
                                  ),
                                  pickerInput(
                                      inputId = "region2",
                                      label = "Pick a Region", 
                                      choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula") 
                                  ),
                              ),
                              mainPanel(                                  
                                  p("Below you have the option to choose between the manual and auto perameter. As before, you can change the region and the purpose of a trip that will change the plots accordingly. Thank you and enjoy!"), 
                                  plotOutput("Hist5"),
                              ),
                              
                          )
                 ),
                 )
                 
                 
                 



server <- function(input, output, session) {
    output$Hist <- renderPlot({
        tourism %>%
            filter( Region %in% input$Region,
                    Purpose %in% input$Purpose) %>%
            autoplot()
    })
    
    output$Hist2 <- renderPlot({
        
        if(input$Choice == "Seasonality"){
            tourism %>%
                filter(Region %in% input$Region, 
                       Purpose %in% input$Purpose) %>%
                gg_subseries()
        } else 
            if (input$Choice == "Autocorrelation"){
                tourism %>% 
                    filter(Region %in% input$Region, 
                           Purpose %in% input$Purpose) %>%
                    ACF(Trips, lag_max = 48) %>%
                    autoplot()
            }else 
                if(input$Choice == "Decomposition"){
                    tourism %>%
                        filter(Region %in% input$Region, 
                               Purpose %in% input$Purpose) %>%
                        model(
                            classical_decomposition(Trips, type = "additive")
                        ) %>%
                        components() %>%
                        autoplot()
                } 
    })
    
    output$text1 <- renderText({
        if( input$Choice == "Decomposition" & input$Region == "Adelaide" & input$Purpose == "Business"){
            paste("In the classical decomposition plot the seasonal pattern is a very reoccurring pattern through the years. It is a cyclical pattern with no variation over time. The trend does show a steep negative trend around 2005 however it slowly increases as the years increase. The random variation has cyclicity as it tends to rise and fall at inconsistent times. ")
        }
        else if( input$Choice == "Seasonality" & input$Region == "Adelaide" & input$Purpose == "Business"){
            paste("There seems to be more seasonality around the third quarter and less seasonality in the first. More people are taking a business trip in Adelaide, Australia during the third quarter and may be because the weather is nicer out. Fewer people are taking business trips in the first quarter which could also be because of the weather and the holiday season.")
        }
        else if( input$Choice == "Autocorrelation" & input$Region == "Adelaide" & input$Purpose == "Business"){
            paste("As the lags increase there seems to be more of a negative correlation which would be because of the trend. There does seem to be some white noise with this data. However, all the positive correlations are between lags 0 to 16 then decrease as the lags continue. There are three statistically significant lags with the highest correlation being negative at 18. Every four quarters are higher than the others this is due to the seasonal pattern in the data, the peaks tend to be four quarters apart. ")
        }
    } )
    output$Hist3 <- renderPlot({
        if(input$Model == "Naive"){
            train <- tourism %>%
                filter( Region %in% input$region1,
                        Purpose %in% input$purpose1)
            Naivefit <- train %>%
                model(`Naïve` = NAIVE(Trips))
            Naive_fc <- Naivefit %>% forecast(h = 12)
            Naive_fc %>% 
                autoplot(train, level = NULL) +
                labs(title="Tourism in Australia",
                     y="Overnight trips (millions)") +
                guides(colour = guide_legend(title = "Forecast"))
            
        } else 
            if (input$Model == "Seasonal Naive"){
                train <- tourism %>%
                    filter( Region %in% input$region1,
                            Purpose %in% input$purpose1)
                SNaivefit <- train %>%
                    model(SNAIVE(Trips ~ lag("year")))
                SNaive_fc <- SNaivefit %>% forecast(h = 12)
                SNaive_fc %>% 
                    autoplot(train, level = NULL)  +
                    labs(title="Tourism in Australia",
                         y="Overnight trips (millions)") +
                    guides(colour = guide_legend(title = "Forecast"))
                
            } else 
                if (input$Model == "Mean"){
                    train <- tourism %>%
                        filter( Region %in% input$region1,
                                Purpose %in% input$purpose1) 
                    meanfit <- train %>%
                        model(MEAN(Trips))
                    mean_fc <- meanfit %>% forecast(h = 12)
                    mean_fc %>%
                        autoplot(train, level = NULL) + 
                        labs(title="Tourism in Australia",
                           y="Overnight trips (millions)") +
                        guides(colour = guide_legend(title = "Forecast"))
                    
                } else 
                    if (input$Model == "Drift"){
                        
                        train <- tourism %>%
                            filter( Region %in% input$region1,
                                    Purpose %in% input$purpose1) 
                        driftfit <- train %>%
                            model(RW(Trips ~ drift()))
                        drift_fc <- driftfit %>% forecast(train)
                        drift_fc %>%
                            autoplot(train, level = NULL) +
                            labs(title="Tourism in Australia",
                                 y="Overnight trips (millions)") +
                            guides(colour = guide_legend(title = "Forecast")) 
                        
                    }
    } )
    output$Hist4 <- renderPlot({
        
        if(input$Holts1 == "Holts"){
            aus_tourism <- tourism %>%
                filter( Region %in% input$region,
                    Purpose %in% input$purpose) %>%
                summarise(Trips = sum(Trips) / 1e3)
            autoplot(aus_tourism, Trips) +
                labs(y = "Overnight trips (millions)", title = "Tourism in Australia")
            
        } else 
            if(input$Holts1 == "Holts/Winters"){   
                aus_tourism <- tourism %>%
                    filter( Region %in% input$region,
                        Purpose %in% input$purpose) %>%
                    summarise(Trips = sum(Trips)/1e3)
                fit <- aus_tourism %>%
                    model(
                        additive = ETS(Trips ~ error("A") + trend("A") +
                                           season("A")),
                        multiplicative = ETS(Trips ~ error("M") + trend("A") +
                                                 season("M"))
                    )
                fc <- fit %>% forecast(h = "3 years")
                fc %>%
                    autoplot(aus_tourism, level = NULL) +
                    labs(title="Tourism in Australia",
                         y="Overnight trips (millions)") +
                    guides(colour = guide_legend(title = "Forecast"))
                
            }
        
        
    })
    
    output$Hist5 <- renderPlot({
        if(input$Arima == "Manual Perameter"){  
            Train <- tourism %>%
                filter( Region %in% input$region2,
                        Purpose %in% input$purpose2)
            Arimafit <- Train %>%
                model(arima210 = ARIMA(Trips~ pdq(2,1,0)),
                      arima013 = ARIMA(Trips ~ pdq(0,1,3)),
                      stepwise = ARIMA(Trips),
                      search = ARIMA(Trips, stepwise=FALSE))
            arimafc <- Arimafit %>% forecast(h=12)
            arimafc %>%
                autoplot(Train)
    }else 
        if(input$Arima == "Auto Perameter"){  
            Train <- tourism %>%
                filter( Region %in% input$region2,
                        Purpose %in% input$purpose2)
            Arima_fit <- Train %>%
                model(ARIMA(Trips))
            arima_fc <- Arima_fit %>%
                forecast(h=12)
            arima_fc %>%
                autoplot(Train)
        }
    })
    
   

        
}
shinyApp(ui, server)



