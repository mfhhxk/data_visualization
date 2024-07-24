library(shiny)
library(ggplot2) # line
library(tidyverse) # data manipulation
library(leaflet) # map
library(geosphere) # map
library(plotly) # line
library(dplyr) # data manipulation
library(zoo) # data manipulation
library(shinycssloaders) # specify size of graph

#### Load Data ####
data <- read.csv('output_with_coordinates.csv')
data <- data %>%
  filter(Sectors_Scheduled != 0 & Sectors_Flown != 0)

# 'Date' column to Date format
data$Date <- as.yearmon(data$Date, "%b-%y")
data$Year <- factor(data$Year)
data$Month <- factor(data$Month)

# remove outlier
outlier <- which(data$Cancellations > 1200)
data <- data[-outlier,]


# filter route that has 3 year, 12 months data
complete_year_routes <- data %>%
  group_by(Route)%>%
  summarize(num_years = n_distinct(Year)) %>%
  filter(num_years == 3) %>%
  pull(Route)

complete_month_routes <- data %>%
  filter(Route %in% complete_year_routes) %>%
  group_by(Route, Year) %>%
  summarise(num_months = n_distinct(Month)) %>%
  filter(num_months == 12) %>%
  pull(Route)


data<- data %>%
  filter(Route %in% complete_month_routes)


#### Aggregate data by route ####
route <- data %>%
  group_by(Route) %>%
  summarise(
    Cancellations = sum(Cancellations),
    Arrivals_Delayed = sum(Arrivals_Delayed),
    Sectors_Scheduled = sum(Sectors_Scheduled),
    Sectors_Flown = sum(Sectors_Flown),
    Departing_Port = first(Departing_Port),
    Arriving_Port = first(Arriving_Port),
    long0 = first(long0),lat0 = first(lat0),
    long1 = first(long1),lat1 = first(lat1)) %>%
  mutate(
    Cancellation_rate = round((Cancellations / Sectors_Scheduled) * 100, 2),
    Delay_rate = round((Arrivals_Delayed / Sectors_Flown) * 100, 2)
  )


#### Route by port #####
unique_ports <- union(unique(route$Departing_Port), 
                      unique(route$Arriving_Port))

##### Dict storing the coordinate of each port ####
port_coordinates <- list()

for (i in 1:nrow(route)) {
  # Extract departing port and its coordinates
  departing_port <- route$Departing_Port[i]
  departing_long <- route$long0[i]
  departing_lat <- route$lat0[i]
  
  arriving_port <- route$Arriving_Port[i]
  arriving_long <- route$long1[i]
  arriving_lat <- route$lat1[i]
  
  # Check if departing port is new
  if (!(departing_port %in% names(port_coordinates))) {
    port_coordinates[[departing_port]] <- c(longitude = departing_long, 
                                            latitude = departing_lat)
  }
  
  if (!(arriving_port %in% names(port_coordinates))) {
    port_coordinates[[arriving_port]] <- c(longitude = arriving_long, 
                                           latitude = arriving_lat)
  }
}

#### functions ####
get_top_cancel_routes <- function(year, data) {
  year <- as.integer(year)
  data %>%
    filter(Year %in% year) %>%
    group_by(Year, Route) %>%
    summarise(total_cancellations = sum(Cancellations),
              total_scheduled = sum(Sectors_Scheduled)) %>%
    mutate(Cancellation_rate = total_cancellations / total_scheduled * 100) %>%
    top_n(5, Cancellation_rate) %>%
    pull(Route)
}

get_top_delay_routes <- function(year, data) {
  year <- as.integer(year)
  data %>%
    filter(Year %in% year) %>%
    group_by(Year, Route) %>%
    summarise(total_delayed = sum(Arrivals_Delayed),
              total_flown = sum(Sectors_Flown)) %>%
    mutate(Delay_rate = (total_delayed / total_flown) * 100) %>%
    top_n(5, Delay_rate) %>%
    pull(Route)
}


#### UI ####
ui <- fluidPage(
  ##### Title #####
  fluidRow(
    headerPanel("Data Visualization Project"),
    tags$p("Author: Michelle Fong", style = "margin-left: 20px;")
  ),
  
  ##### Tabs #####
  tabsetPanel(
    ##### Project Information Tab #####
    tabPanel("Project Information",
             htmlOutput("project_info")),
    
    ##### User Guide #####
    tabPanel("User Guide",
             htmlOutput("guide")),
    
    ##### Cancellation Rate Tab #####
    # https://stackoverflow.com/questions/30846198/shiny-changing-slider-animation-speed
    # https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
    # https://cran.r-project.org/web/packages/shinycssloaders/readme/README.html
    tabPanel("Cancellation Rate",
             fluidRow(
               sidebarPanel(
                 sliderInput("time_frame_cancel",
                             label = 'Year:',
                             min = 2021, max = 2023,
                             value = 2021, step = 1,
                             animate = animationOptions(interval = 2000,loop = TRUE), 
                             sep = "" ),
                 checkboxGroupInput("dataset_cancel", 
                                    label = 'Dimension: (Select up to 2)',
                                    choices = c("Route", "Port"),
                                    selected = c("Route", "Port")),
                 tags$hr(),
                 div(class = "info-box",
                     h4("Information summary:"),
                     uiOutput("info_map_cancel"))
               ),  width = 2,
               mainPanel(
                 div(style = "text-align: center;", 
                     HTML("<b>Cancellation Rate from 2021 to 2023 over Month</b>")),
                 withSpinner(plotlyOutput("cancellation_rate_plot", height = "300px", width = "100%")),
                 
                 div(style = "text-align: center;", 
                     HTML("<b>Interactive Map of Yearly Cancellation Rate</b>")),
                 leafletOutput("cancellation_rate_map", height = "450px", width = "100%"))
    )),
    
    
    ##### Delay Rate Tab #####
    tabPanel("Delay Rate",
             fluidRow(
               sidebarPanel(
                 sliderInput("time_frame_delay",
                             label = 'Year:',
                             min = 2021, max = 2023,
                             value = 2021, step = 1,
                             animate = animationOptions(interval = 2000,loop = TRUE), 
                             ticks = TRUE, sep = "" ),
                 checkboxGroupInput("dataset_delay", 
                                    label = 'Dimension: (Select up to 2)',
                                    choices = c("Route", "Port"),
                                    selected = c("Route", "Port")),
                 tags$hr(),
                 div(class = "info-box",
                     h4("Information summary:"),
                     uiOutput("info_map_delay"))
               ),  width = 2,
               mainPanel(
                 div(style = "text-align: center;", 
                     HTML("<b>Delay Rate from 2021 to 2023 over Month</b>")),
                 withSpinner(plotlyOutput("delay_rate_plot", height = "300px", width = "100%")),
                 
                 div(style = "text-align: center;", 
                     HTML("<b>Interactive Map of Yearly Delay Rate</b>")),
                 leafletOutput("delay_rate_map", height = "450px", width = "100%")
               )
             )
    )
  )
)





route_time <- data %>%
  group_by(Route, 
           Year = as.factor(Year), 
           Month = as.factor(Month)) %>%
  summarize(Cancellations = sum(Cancellations),
            Arrivals_Delayed = sum(Arrivals_Delayed),
            Sectors_Scheduled = sum(Sectors_Scheduled),
            Sectors_Flown = sum(Sectors_Flown)) %>%
  mutate(Cancellation_rate = (Cancellations / Sectors_Scheduled) * 100,
         Delay_rate = (Arrivals_Delayed / Sectors_Flown) * 100)








#### Server ####
server <-function(input, output) {
  default_year <- 2021
  year_cancel <- reactive({
    if (is.null(input$time_frame_cancel)) {
      default_year } 
    else {
        input$time_frame_cancel} })
  
  year_delay <- reactive({
    if (is.null(input$time_frame_delay)) {
      default_year 
    } 
    else {
      input$time_frame_delay} })
  
  top_routes_cancel <- reactive({
    get_top_cancel_routes(year_cancel(), data)
  })
  
  top_routes_delay <- reactive({
    get_top_delay_routes(year_delay(), data)
  })
  
  plot_route_time <- function(data, year, top_routes = NULL) {
    filtered_data <- data %>%
      filter(Year %in% year)
    
    if (!is.null(top_routes)) {
      filtered_data <- filtered_data %>%
        filter(Route %in% top_routes)
    }
    
    result <- filtered_data %>%
      group_by(Route, 
               Year = as.factor(Year), 
               Month = as.factor(Month)) %>%
      summarize(Cancellations = sum(Cancellations),
                Arrivals_Delayed = sum(Arrivals_Delayed),
                Sectors_Scheduled = sum(Sectors_Scheduled),
                Sectors_Flown = sum(Sectors_Flown),
                long0 = first(long0),
                lat0 = first(lat0),
                long1 = first(long1),
                lat1 = first(lat1),
                Departing_Port = first(Departing_Port),
                Arriving_Port = first(Arriving_Port)) %>%
      mutate(Cancellation_rate = (Cancellations / Sectors_Scheduled) * 100,
             Delay_rate = (Arrivals_Delayed / Sectors_Flown) * 100)
    
    return(result)
  }
  
  
  map_route_yr <- function(data, year) {
    filtered_data <- data %>%
      filter(Year %in% year)
    
    result <- filtered_data %>%
      group_by(Route, 
               Year = as.factor(Year)) %>%
      summarize(Cancellations = sum(Cancellations),
                Arrivals_Delayed = sum(Arrivals_Delayed),
                Sectors_Scheduled = sum(Sectors_Scheduled),
                Sectors_Flown = sum(Sectors_Flown),
                long0 = first(long0),
                lat0 = first(lat0),
                long1 = first(long1),
                lat1 = first(lat1),
                Departing_Port = first(Departing_Port),
                Arriving_Port = first(Arriving_Port)) %>%
      mutate(Cancellation_rate = round((Cancellations / Sectors_Scheduled) * 100,2),
             Delay_rate = round((Arrivals_Delayed / Sectors_Flown) * 100),2)
    
    return(result)
  }
  ##### info page #####
  output$project_info <- renderText({
    text <- "
    This application presents an interactive exploration of a study about the factors 
    affecting punctuality in Australian domestic flights. The study focusing on the relationship between 
    flight routes, profitability, and airline companys. 
    This application answers the relationship between punctuality and flight route, 
    answering the sub-question:<br/><br/>
    How do flight routes correlate with the likelihood of cancellations or delays?<br/><br/>
    
    To answer this sub-question, this application was built with several interactive features.
    The data is filtered to include only routes with complete records for each year and month, 
    which ensures a comprehensive coverage for observing seasonal effect. The application 
    includes interactive line graphs and maps that visualize cancellation and delay 
    rates over time and across different routes. <br/><br/>
    
    The goal of this visualization is to offer insights into operational efficiency for 
    Australian domestic airlines and assist travelers in selecting airlines for their next trip.<br/><br/>
    
    <u><b>Data Sources:</b></u><br/><br/>
    <b>1. Domestic Airlines – On-time Performance</b><br/>
    Source: Bureau of Infrastructure and Transport Research Economics<br/>
    Created: 13/02/2015, Updated: 14/08/2023<br/>
    Link: <a href='https://data.gov.au/dataset/ds-dga-29128ebd-dbaa-4ff5-8b86-d9f30de56452/details'>https://data.gov.au/dataset/ds-dga-29128ebd-dbaa-4ff5-8b86-d9f30de56452/details</a><br/>
    Description:<br/>
    Records monthly punctuality of domestic and regional airlines from Jan 2004 to Jun 2023. The dataset contains 101,248 records with 14 variables, covering information such as the number of flights cancelled, delayed, and arrived on time for each route of an airline company.<br/><br/>
    
    <b>2. Domestic Airlines – Top Routes and Totals</b><br/>
    Source: Bureau of Infrastructure and Transport Research Economics<br/>
    Created: 06/06/2016, Updated: 01/12/2023<br/>
    Link: <a href='https://data.gov.au/dataset/ds-dga-c5029f2a-39b3-4aef-8ae1-73e7962f6170/details'>https://data.gov.au/dataset/ds-dga-c5029f2a-39b3-4aef-8ae1-73e7962f6170/details</a><br/>
    Description: <br/>
    Provides monthly data on passengers carried, Revenue Passenger Kilometre (RPKs), Available Seat Kilometres (ASKs), Passenger Load Factor (PLF), etc. The dataset spans from Jan 1984 to Sep 2023, with 26,986 records and 12 variables.<br/><br/>
    
    <b>3. Directory of Airports in Australia</b><br/>
    Source: Falling Rain<br/>
    Link: <a href='https://www.fallingrain.com/world/AS/airports.html'>https://www.fallingrain.com/world/AS/airports.html</a><br/>
    Description: <br/>
    Contains longitude and latitude coordinates of all airports in Australia."
    
    HTML(text)
  })
  
  ##### user guide #####
  output$guide <- renderText({
    text <- "
  <div class='user-guide'>
    <h3>User Guide</h3>
    <p>The interactive features are consistent across both the Cancellation Rate 
    and Delay Rate tabs, making this user guide applicable to both. 
    Here's how to interact with the application:</p>
    <ol>
      <li><strong>Select Tab:</strong> Choose whether you want to explore the 
    Cancellation Rate or Delay Rate tab.</li>
      <li><strong>Choose Year:</strong> Use the slider to select the year you 
    want to focus on, or click the play button for an animated view of changes 
    over the years. The animation will automatically restart once finished.</li>
      <li><strong>Select Dimension:</strong> Choose the dimension you want to 
    see on the map: Route only, Port only, or both.</li>
    </ol>
    <p>Once you've made your selections, the information summary will update to 
    show the top 5 yearly cancellation rates in the selected dimension. Both 
    the line graph and the map will update to display data for the selected year and dimension.</p>
  </div>
  <hr>
    
  <div class='interactive-features'>
    <h3>Interactive Features</h3>
    <p><strong>Line Graph:</strong></p>
    <ul>
      <li><strong>Legend:</strong> Click on any key in the legend to hide or 
    show the departing port.</li>
      <li><strong>Line:</strong> Hover over the dots to display a tooltip 
    with more information.</li>
      <li><strong>Toolbar:</strong> Move the mouse above the graph to reveal 
    a toolbar. Here, you can download the graph, zoom, pan, reset axes, and 
    choose to compare data points on hover.</li>
    </ul>
    <p><strong>Map:</strong></p>
    <ul>
      <li><strong>Zooming:</strong> Zoom in or out using the plus and minus 
    signs on the top left corner.</li>
      <li><strong>Layer Control:</strong> Check or uncheck the Port option 
    to show or hide the corresponding departing ports.</li>
      <li><strong>Flowlines:</strong> Hover over the flowlines to display a 
    tooltip showing the yearly rate of the corresponding route.</li>
      <li><strong>Circle Markers:</strong> Hover over the circle markers to 
    display the yearly rate of the corresponding ports.</li>
    </ul>
  </div>"
    HTML(text)
  })
  
  ##### color palette #####
  departing_ports <- unique(data$Departing_Port)
  num_ports <- length(departing_ports)
  color_palette <- pals::cols25(num_ports)
  port_color_factor <- colorFactor(palette = color_palette, 
                                   levels = unique_ports)
  
  ##### cancellation_rate_plot ##### 
  # Referene:
  # https://forum.posait.co/t/highlight-routes-polylines-based-on-hover-on-destinations-circlemarkers-in-leaflet/112400
  # https://psrc.github.io/intro-shiny-guide/packages_i.html

  

  
  output$cancellation_rate_plot <- renderPlotly({
    plotly_data <- plot_route_time(data, year_cancel(), top_routes_cancel())
    plotly_data <- plotly_data %>%
      rename("Cancellation Rate" = Cancellation_rate) %>%
      rename("Departing Port" = Departing_Port) %>%
      mutate(`Cancellation Rate` = round(`Cancellation Rate`, 2))
    
    fig <- ggplot(plotly_data, 
           aes(x = Month, 
               y = `Cancellation Rate`, 
               color = `Departing Port`, 
               group = Route
           )) +
      geom_line() +
      geom_text(data = plotly_data[!duplicated(plotly_data$Route), ], 
                aes(label = Route), 
                hjust = 1, vjust = 1, 
                size = 3, show.legend = FALSE, 
                nudge_y = 1.9, nudge_x = 0.5) + 
      geom_point() +
      scale_color_manual(values = port_color_factor(unique(plotly_data$`Departing Port`))) +
      labs(
        title = "Cancellation Rate Over Time",
        x = "Month",
        y = "Cancellation Rate (%)",
        color = "Route" 
      ) +
      guides(color = guide_legend(title = "Departing Port") ) +
      ylim(0, 70)+
      theme(axis.text.x = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            plot.title = element_text(size = 10),
            legend.title = element_text(size = 10), 
            legend.text = element_text(size = 8))

    
    fig
    
  })
  
  
  ##### delay_rate_plot ##### 
  output$delay_rate_plot <- renderPlotly({
    plotly_data <- plot_route_time(data, year_delay(), top_routes_delay())

    plotly_data <- plotly_data %>%
      rename("Delay Rate" = Delay_rate) %>%
      rename("Departing Port" = Departing_Port) %>%
      mutate(`Delay Rate` = round(`Delay Rate`, 2))

    ggplot(plotly_data, 
           aes(x = Month, 
               y = `Delay Rate`, 
               color = `Departing Port`, 
               group = Route
           )) +
      geom_line() +
      geom_text(data = plotly_data[!duplicated(plotly_data$Route), ], 
                aes(label = Route, color = `Departing Port`), 
                hjust = 1, vjust = 1, 
                size = 3, show.legend = FALSE, 
                nudge_y = 2.1, nudge_x = 0.5) + 
      geom_point() +
      scale_color_manual(values = port_color_factor(unique(plotly_data$`Departing Port`))) +
      labs(
        title = "Delay Rate Over Time",
        x = "Month",
        y = "Delay Rate (%)",
        color = "Route" 
      ) +
      guides(color = guide_legend(title = "Departing Port") ) +
      ylim(0, 76)+
      theme(axis.text.x = element_text(size = 8),
            axis.title.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            axis.title.y = element_text(size = 8),
            plot.title = element_text(size = 10),
            legend.title = element_text(size = 10), 
            legend.text = element_text(size = 8))
    
  })
  
  
  ##### port_stats_cancel #####
  port_stats_cancel <- reactive({
    selected_year <- year_cancel()
    yearly_route <- data %>% filter(Year == selected_year)
    
    port_stats_cancel <- data.frame(
      Port = unique_ports,
      Long = NA, Lat = NA,
      Cancellation_rate = NA,
      Delay_rate = NA,
      Year = NA
    )
    
    for (i in 1:length(unique_ports)) {
      port <- unique_ports[i]
      # detect if the port can be found in the route using regex
      port_detect <- str_detect(yearly_route$Route, port)
      
      # if found, extract corresponding values and aggregate
      if (any(port_detect)) {
        cancellations <- sum(yearly_route$Cancellations[port_detect])
        arrivals_delayed <- sum(yearly_route$Arrivals_Delayed[port_detect])
        sectors_scheduled <- sum(yearly_route$Sectors_Scheduled[port_detect])
        sectors_flown <- sum(yearly_route$Sectors_Flown[port_detect])
        
        # Recalculate rates
        cancellation_rate <- round((cancellations / sectors_scheduled) * 100, 2)
        delay_rate <- round((arrivals_delayed / sectors_flown) * 100, 2)
        
        # Get the coordinates for the port
        port_long <- port_coordinates[[port]]['longitude']
        port_lat <- port_coordinates[[port]]['latitude']
        
        # Store in the data frame
        port_stats_cancel[i, c("Long", "Lat",
                        "Cancellation_rate", 
                        "Delay_rate", "Year")] <- 
          c(port_long, port_lat,
            cancellation_rate, delay_rate, selected_year)
      }
    }
    
    return(port_stats_cancel)
  })
  
  
  ##### port_stats_delay #####
  port_stats_delay <- reactive({
    selected_year <- year_delay()
    yearly_route <- data %>% filter(Year == selected_year)
    
    port_stats_delay <- data.frame(
      Port = unique_ports,
      Long = NA, Lat = NA,
      Cancellation_rate = NA,
      Delay_rate = NA,
      Year = NA
    )
    
    for (i in 1:length(unique_ports)) {
      port <- unique_ports[i]
      # detect if the port can be found in the route using regex
      port_detect <- str_detect(yearly_route$Route, port)
      
      # if found, extract corresponding values and aggregate
      if (any(port_detect)) {
        cancellations <- sum(yearly_route$Cancellations[port_detect])
        arrivals_delayed <- sum(yearly_route$Arrivals_Delayed[port_detect])
        sectors_scheduled <- sum(yearly_route$Sectors_Scheduled[port_detect])
        sectors_flown <- sum(yearly_route$Sectors_Flown[port_detect])
        
        # Recalculate rates
        cancellation_rate <- round((cancellations / sectors_scheduled) * 100, 2)
        delay_rate <- round((arrivals_delayed / sectors_flown) * 100, 2)
        
        # Get the coordinates for the port
        port_long <- port_coordinates[[port]]['longitude']
        port_lat <- port_coordinates[[port]]['latitude']
        
        # Store in the data frame
        port_stats_delay[i, c("Long", "Lat",
                        "Cancellation_rate", 
                        "Delay_rate", "Year")] <- 
          c(port_long, port_lat,
            cancellation_rate, delay_rate, selected_year)
      }
    }
    
    return(port_stats_delay)
  })
  
  
  
  
  
  
  
  ##### route_cancel #####
  route_cancel <- reactive({
    default_year <- 2021
    
    # Get the selected year from the input
    selected_year <- year_cancel()
    
    # Filter the route data frame by the selected year
    route_filtered <- data %>%
      filter(Year == selected_year) %>%
      group_by(Route) %>%
      summarise(
        Cancellations = sum(Cancellations),
        Arrivals_Delayed = sum(Arrivals_Delayed),
        Sectors_Scheduled = sum(Sectors_Scheduled),
        Sectors_Flown = sum(Sectors_Flown),
        Departing_Port = first(Departing_Port),
        Arriving_Port = first(Arriving_Port),
        long0 = first(long0),
        lat0 = first(lat0),
        long1 = first(long1),
        lat1 = first(lat1)
      ) %>%
      mutate(
        Cancellation_rate = round((Cancellations / Sectors_Scheduled) * 100, 2),
        Delay_rate = round((Arrivals_Delayed / Sectors_Flown) * 100, 2)
      )
    
    return(route_filtered)
  })

  
  

  
  
  ##### route_delay #####
  route_delay <- reactive({
    default_year <- 2021
    
    # Get the selected year from the input
    selected_year <- year_delay()
    
    # Filter the route data frame by the selected year
    route_filtered <- data %>%
      filter(Year == selected_year) %>%
      group_by(Route) %>%
      summarise(
        Cancellations = sum(Cancellations),
        Arrivals_Delayed = sum(Arrivals_Delayed),
        Sectors_Scheduled = sum(Sectors_Scheduled),
        Sectors_Flown = sum(Sectors_Flown),
        Departing_Port = first(Departing_Port),
        Arriving_Port = first(Arriving_Port),
        long0 = first(long0),
        lat0 = first(lat0),
        long1 = first(long1),
        lat1 = first(lat1)
      ) %>%
      mutate(
        Cancellation_rate = round((Cancellations / Sectors_Scheduled) * 100, 2),
        Delay_rate = round((Arrivals_Delayed / Sectors_Flown) * 100, 2)
      )
    
    return(route_filtered)
  })
  
  
  
  
  ##### cancellation_rate_map   ##### 
  # Reference:
  # https://stackoverflow.com/questions/71526164/add-colorfactor-palette-to-mapview-object
  # https://stackoverflow.com/questions/30491419/adding-labels-to-cut-function-in-r
  # https://forum.posit.co/t/why-is-base-rs-cut-output-formatted-the-way-it-is/12149/4
  # https://stackoverflow.com/questions/25086437/how-can-i-get-quantile-label-names-in-cut-function
  # https://rstudio.github.io/leaflet/articles/showhide.html
  # https://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html
  # https://rdrr.io/cran/leaflet.minicharts/man/addFlows.html
  # https://stackoverflow.com/questions/36887384/advanced-popups-in-r-shiny-leaflet
  # https://bhaskarvk.github.io/user2017.geodataviz/notebooks/03-Interactive-Maps.nb.html
  # https://roh.engineering/posts/2021/05/map-symbols-and-size-legends-for-leaflet/ 
  # https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker 
  
  
  output$cancellation_rate_map <- renderLeaflet({
    data_filtered <- map_route_yr(data, year_cancel())
    flows <- gcIntermediate(data_filtered[,c('long0', 'lat0')], 
                            data_filtered[,c('long1', 'lat1')], 
                            sp = TRUE, addStartEnd = TRUE)
    flows$Cancellation_rate <- data_filtered$Cancellation_rate
    flows$Departing_Port <- data_filtered$Departing_Port
    flows$Arriving_Port <- data_filtered$Arriving_Port
    flows$Route <- data_filtered$Route
    
    hover <- paste0('Yearly Cancellation Rate of ',
      flows$Departing_Port, " to ", 
                    flows$Arriving_Port, ': ', 
                    as.character(flows$Cancellation_rate), "%")
    flows$hover <- hover

    
    port_stats_filtered <- port_stats_cancel()
    port_stats_filtered <- port_stats_filtered[!is.na(port_stats_filtered$Cancellation_rate), ]
    
    generate_labels <- function(quantiles) {
      labels <- character(length(quantiles) - 1)
      quantiles_rounded <- round(quantiles, 2)
      for (i in 1:(length(quantiles) - 1)) {
        if (i == 1) {
          labels[i] <- paste0("Q", i, ": <", quantiles_rounded[i + 1])
        } else if (i < (length(quantiles) - 1)) {
          labels[i] <- paste0("Q", i, ": ", quantiles_rounded[i], " - ", quantiles_rounded[i + 1])
        } else {
          labels[i] <- paste0("Q", i, ": >", quantiles_rounded[i])
        }
      }
      return(labels)
    }
    
    # quantile cut and label acc to quantile
    quantiles <- quantile(port_stats_filtered$Cancellation_rate, na.rm = TRUE)
    quantile_labels <- generate_labels(quantiles)
    
    # Update port_stats_filtered$cancel_quantile with new labels
    port_stats_filtered$cancel_quantile <- cut(port_stats_filtered$Cancellation_rate, 
                                               breaks = quantiles,
                                               include.lowest = TRUE,
                                               labels = quantile_labels)
  
    
    quantile_colors <- colorFactor(palette = 'RdYlGn', 
                                   port_stats_filtered$cancel_quantile)
    
    leaflet_output <- leaflet() %>%
      setView(lng = 135, lat = -30, zoom = 3.5) %>%
      addProviderTiles("CartoDB.Positron")
    
    max_rate <- max(data_filtered$Cancellation_rate, na.rm = TRUE)
    min_rate <- min(data_filtered$Cancellation_rate, na.rm = TRUE)
    normalized_rate <- (data_filtered$Cancellation_rate - min_rate) / (max_rate - min_rate)
    
    # if selected Route
    if ("Route" %in% input$dataset_cancel) {
      leaflet_output <- leaflet_output %>%
        # flowline
        addPolylines(data = flows, 
                     weight = ~normalized_rate*10, 
                     label = hover, 
                     group = ~Departing_Port, 
                     color = ~port_color_factor(Departing_Port),
                     opacity = 0.85) %>%
        
        # location of Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = 3.2,  color = "blue", 
                         fillColor = 'blue',
                         fillOpacity = 1, 
                         stroke = FALSE, 
                         opacity = 1,
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Cancellation Rate: ", Cancellation_rate)) %>%
        addLayersControl(overlayGroups = unique(flows$Departing_Port), 
                         baseGroups = "Departing Port",
                         options = layersControlOptions(collapsed = TRUE))
    }
    
    # if Port selected
    if ("Port" %in% input$dataset_cancel) {
      leaflet_output <- leaflet_output %>%
        # color showing the quantile of Rate by Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = ~Cancellation_rate*3 ,
                         weight = 1.2, opacity = 2,
                         fillOpacity = 0.15,
                         color = ~quantile_colors(cancel_quantile), 
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Cancellation Rate: ", Cancellation_rate),
                         group = 'Port Cancellation Rate') %>%
        
        # location of Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = 3.2,  color = "blue", 
                         fillColor = 'blue',
                         fillOpacity = 1, 
                         stroke = FALSE, 
                         opacity = 1,
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Cancellation Rate: ", Cancellation_rate)) %>%
        
        addLegend(position = "bottomleft",
                  pal = quantile_colors, 
                  values = port_stats_filtered$cancel_quantile,
                  title = 'Yearly Port Cancellation Rate by Quantile',
                  opacity = 0.8) }
    leaflet_output
  })
  
  ##### delay_rate_map   ##### 
  output$delay_rate_map <- renderLeaflet({
    data_filtered <- map_route_yr(data, year_delay())
    flows <- gcIntermediate(data_filtered[,c('long0', 'lat0')], 
                            data_filtered[,c('long1', 'lat1')], 
                            sp = TRUE, addStartEnd = TRUE)
    flows$Delay_rate <- data_filtered$Delay_rate
    flows$Departing_Port <- data_filtered$Departing_Port
    flows$Arriving_Port <- data_filtered$Arriving_Port
    flows$Route <- data_filtered$Route
    
    hover <- paste0('Yearly Delay Rate of ',
                    flows$Departing_Port, " to ", 
                    flows$Arriving_Port, ': ', 
                    as.character(flows$Delay_rate), "%")
    flows$hover <- hover
    
    
    port_stats_filtered <- port_stats_delay()
    port_stats_filtered <- port_stats_filtered[!is.na(port_stats_filtered$Delay_rate), ]
    
    generate_labels <- function(quantiles) {
      labels <- character(length(quantiles) - 1)
      quantiles_rounded <- round(quantiles, 2)
      for (i in 1:(length(quantiles) - 1)) {
        if (i == 1) {
          labels[i] <- paste0("Q", i, ": <", quantiles_rounded[i + 1])
        } else if (i < (length(quantiles) - 1)) {
          labels[i] <- paste0("Q", i, ": ", quantiles_rounded[i], " - ", quantiles_rounded[i + 1])
        } else {
          labels[i] <- paste0("Q", i, ": >", quantiles_rounded[i])
        }
      }
      return(labels)
    }
    
    # quantile cut and label acc to quantile
    quantiles <- quantile(port_stats_filtered$Delay_rate, na.rm = TRUE)
    quantile_labels <- generate_labels(quantiles)
    
    # Update port_stats_filtered$delay_quantile with new labels
    port_stats_filtered$delay_quantile <- cut(port_stats_filtered$Delay_rate, 
                                               breaks = quantiles,
                                               include.lowest = TRUE,
                                               labels = quantile_labels)
    
    
    quantile_colors <- colorFactor(palette = 'RdYlGn', 
                                   port_stats_filtered$delay_quantile)
    
    leaflet_output <- leaflet() %>%
      setView(lng = 135, lat = -30, zoom = 3.5) %>%
      addProviderTiles("CartoDB.Positron")
    
    max_rate <- max(data_filtered$Delay_rate, na.rm = TRUE)
    min_rate <- min(data_filtered$Delay_rate, na.rm = TRUE)
    normalized_rate <- (data_filtered$Delay_rate - min_rate) / (max_rate - min_rate)
    
    # if selected Route
    if ("Route" %in% input$dataset_delay) {
      leaflet_output <- leaflet_output %>%
        # flowline
        addPolylines(data = flows, 
                     weight = ~normalized_rate*6, 
                     label = hover, 
                     group = ~Departing_Port, 
                     color = ~port_color_factor(Departing_Port),
                     opacity = 0.7) %>%
        
        # location of Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = 3.2,  color = "blue", 
                         fillColor = 'blue',
                         fillOpacity = 1, 
                         stroke = FALSE, 
                         opacity = 1,
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Delay Rate: ", Delay_rate)) %>%
        addLayersControl(overlayGroups = unique(flows$Departing_Port), 
                         baseGroup = "Departing Port",
                         options = layersControlOptions(collapsed = TRUE))
    }
    
    # if Port selected
    if ("Port" %in% input$dataset_delay) {
      leaflet_output <- leaflet_output %>%
        # color showing the quantile of Rate by Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = ~Delay_rate*1 ,
                         weight = 1.2, opacity = 2,
                         fillOpacity = 0.15,
                         color = ~quantile_colors(delay_quantile), 
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Delay Rate: ", Delay_rate),
                         group = 'Port Delay Rate') %>%
        
        # location of Port
        addCircleMarkers(data = port_stats_filtered,
                         lng = ~Long,lat = ~Lat, 
                         radius = 3.2,  color = "blue", 
                         fillColor = 'blue',
                         fillOpacity = 1, 
                         stroke = FALSE, 
                         opacity = 1,
                         popup = ~paste("Port: ", Port, "<br>",
                                        "Delay Rate: ", Delay_rate)) %>%
        
        addLegend(position = "bottomleft",
                  pal = quantile_colors, 
                  values = port_stats_filtered$delay_quantile,
                  title = 'Yearly Port Delay Rate by Quantile',
                  opacity = 0.8) }
    leaflet_output
  })
    
  
  
  ##### Helper function #####
  # get the highest info for the selected data type
  get_highest_info <- function(route, port_stats, data_type, dataset) {
    if (data_type == "Cancellation Rate") {
      by_route <- route[order(route$Cancellation_rate, decreasing = TRUE), ]
      top_3_route <- head(by_route, 5)
      
      by_port <- port_stats[order(port_stats$Cancellation_rate, decreasing = TRUE), ]
      top_3_port <- head(by_port, 5)
      
      corresponding_info <- NULL
      highest_values <- NULL
      
      if ("Port" %in% dataset & !"Route" %in% dataset) {
        corresponding_info <- top_3_port$Port
        highest_values <- top_3_port$Cancellation_rate
      } else if (!"Port" %in% dataset & "Route" %in% dataset) {
        corresponding_info <- top_3_route$Route
        highest_values <- top_3_route$Cancellation_rate
      } else if ("Port" %in% dataset & "Route" %in% dataset) {
        corresponding_info <- list(top_3_route$Route, top_3_port$Port)
        highest_values <- list(top_3_route$Cancellation_rate, top_3_port$Cancellation_rate)
      }
      
      list(corresponding_info = corresponding_info, highest_values = highest_values)
      
    } else if (data_type == "Delay Rate") {
      by_route <- route[order(route$Delay_rate, decreasing = TRUE), ]
      top_3_route <- head(by_route, 5)
      
      by_port <- port_stats[order(port_stats$Delay_rate, decreasing = TRUE), ]
      top_3_port <- head(by_port, 5)
      
      corresponding_info <- NULL
      highest_values <- NULL
      
      if ("Port" %in% dataset & !"Route" %in% dataset) {
        corresponding_info <- top_3_port$Port
        highest_values <- top_3_port$Delay_rate
      } else if (!"Port" %in% dataset & "Route" %in% dataset) {
        corresponding_info <- top_3_route$Route
        highest_values <- top_3_route$Delay_rate
      } else if ("Port" %in% dataset & "Route" %in% dataset) {
        corresponding_info <- list(top_3_route$Route, top_3_port$Port)
        highest_values <- list(top_3_route$Delay_rate, top_3_port$Delay_rate)
      }
      
      list(corresponding_info = corresponding_info, highest_values = highest_values)
    }
  }
  
  
  ##### info_map_cancel #####
  output$info_map_cancel <- renderUI({
    info_html <- paste("<b>Rate showing:</b><br>",
                       "Cancellation Rate", "<br>",
                       "<b>Selected Dataset:</b><br>",
                       input$dataset_cancel, "<br>")
    port_stats_filtered <- port_stats_cancel()
    port_stats_filtered <- port_stats_filtered[!is.na(port_stats_filtered$Cancellation_rate), ]
    
    route_year <- route_cancel()
    result <- get_highest_info(route_year, port_stats_filtered, "Cancellation Rate", input$dataset_cancel)
    
    if ("Route" %in% input$dataset_cancel & !"Port" %in% input$dataset_cancel) {
      corresponding_info <- result$corresponding_info
      highest_values <- result$highest_values
      
      info_html <- paste(info_html,"<br>",
                         "<b>Yearly Top 5 Route Cancellation Rate:</b><br>",
                         paste(paste0("<b>", 1:5, ". </b>", 
                                      corresponding_info[1:5], ": ", 
                                      highest_values[1:5], "<br>"), 
                               collapse = ""),
                         "<br>")
    }
    
    if ("Port" %in% input$dataset_cancel & !"Route" %in% input$dataset_cancel) {
      corresponding_info <- result$corresponding_info
      highest_values <- result$highest_values
      
      info_html <- paste(info_html,"<br>",
                         "<b>Yearly Top 5 Port Cancellation Rate:</b><br>",
                         paste(paste0("<b>", 1:5, ". </b>", 
                                      corresponding_info[1:5], ": ", 
                                      highest_values[1:5], "<br>"), 
                               collapse = ""),
                         "<br>")
    }
    
    if ("Route" %in% input$dataset_cancel && "Port" %in% input$dataset_cancel) {
      dataset_display <- paste(input$dataset_cancel, collapse = " and ")
      corresponding_info_route <- result$corresponding_info[[1]]
      highest_values_route <- result$highest_values[[1]]
      corresponding_info_port <- result$corresponding_info[[2]]
      highest_values_port <- result$highest_values[[2]]
      
      route_info <- paste(paste0("<b>", 1:5, ". </b>", 
                                 corresponding_info_route[1:5], ": ", 
                                 highest_values_route[1:5], "<br>"), 
                          collapse = "")
      port_info <- paste(paste0("<b>", 1:5, ". </b>", 
                                corresponding_info_port[1:5], ": ", 
                                highest_values_port[1:5], "<br>"), 
                         collapse = "")
      
      info_html <- paste("<b>Rate showing:</b><br>",
                         "Cancellation Rate", "<br>",
                         "<b>Selected Dataset:</b><br>",
                         dataset_display, "<br>",
                         "<br>",
                         "<b>Yearly Top 5 Route Cancellation Rate:</b><br>",
                         route_info, "<br>",
                         "<b>Yearly Top 5 Port Cancellation Rate:</b><br>",
                         port_info)
    }
    
    HTML(info_html)
  })
  
  
  ##### info_map_delay #####
  output$info_map_delay <- renderUI({
    info_html <- paste("<b>Rate showing:</b><br>",
                       "Delay Rate", "<br>",
                       "<b>Selected Dataset:</b><br>",
                       input$dataset_delay, "<br>")
    
    port_stats_filtered <- port_stats_delay()
    port_stats_filtered <- port_stats_filtered[!is.na(port_stats_filtered$Delay_rate), ]
    
    route_year <- route_delay()
    result <- get_highest_info(route_year, port_stats_filtered, "Delay Rate", input$dataset_delay)
    
    if ("Route" %in% input$dataset_delay & !"Port" %in% input$dataset_delay) {
      corresponding_info <- result$corresponding_info
      highest_values <- result$highest_values
      
      info_html <- paste(info_html,"<br>",
                         "<b>Yearly Top 5 Route Delay Rate:</b><br>",
                         paste(paste0("<b>", 1:5, ". </b>", 
                                      corresponding_info[1:5], ": ", 
                                      highest_values[1:5], "<br>"), 
                               collapse = ""),
                         "<br>")
    }
    
    if ("Port" %in% input$dataset_delay & !"Route" %in% input$dataset_delay) {
      corresponding_info <- result$corresponding_info
      highest_values <- result$highest_values
      
      info_html <- paste(info_html,"<br>",
                         "<b>Yearly Top 5 Port Delay Rate:</b><br>",
                         paste(paste0("<b>", 1:5, ". </b>", 
                                      corresponding_info[1:5], ": ", 
                                      highest_values[1:5], "<br>"), 
                               collapse = ""),
                         "<br>")
    }
    
    if ("Route" %in% input$dataset_delay && "Port" %in% input$dataset_delay) {
      dataset_display <- paste(input$dataset_delay, collapse = " and ")
      corresponding_info_route <- result$corresponding_info[[1]]
      highest_values_route <- result$highest_values[[1]]
      corresponding_info_port <- result$corresponding_info[[2]]
      highest_values_port <- result$highest_values[[2]]
      
      route_info <- paste(paste0("<b>", 1:5, ". </b>", 
                                 corresponding_info_route[1:5], ": ", 
                                 highest_values_route[1:5], "<br>"), 
                          collapse = "")
      port_info <- paste(paste0("<b>", 1:5, ". </b>", 
                                corresponding_info_port[1:5], ": ", 
                                highest_values_port[1:5], "<br>"), 
                         collapse = "")
      
      info_html <- paste("<b>Rate showing:</b><br>",
                         "Delay Rate", "<br>",
                         "<b>Selected Dataset:</b><br>",
                         dataset_display, "<br>",
                         "<br>",
                         "<b>Yearly Top 5 Route Delay Rate:</b><br>",
                         route_info, "<br>",
                         "<b>Yearly Top 5 Port Delay Rate:</b><br>",
                         port_info)
    }
    
    HTML(info_html)
  })
  
}


#### run ####
shinyApp(ui = ui, server = server)

