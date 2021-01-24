require(shiny)
require(shinydashboard)
require(tidyverse)
require(magrittr)
require(readxl)
require(ggthemes)
require(hrbrthemes)
theme_set(theme_ipsum())
require(plyr)
require(lubridate)
require(plotly)
####
#define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")
#
query_result <- read_excel("Junior Data Analyst Data for Case Study.xlsx", sheet = "Query result")
#
qr <- query_result %>% select(-c(2,10:13)) %>% 
  mutate(`Standard or Express` = replace_na(query_result$`Standard or Express`, 'Undefined'),
         `Placement Day`  = mapvalues(query_result$`Placement Day`, from = c(1,2,3,4,5,6,7), 
                                      to = c('Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun'))) %>% 
  janitor::clean_names()
#
cancel_reason_highest_order <- qr %>% select(5) %>%  dplyr::count(cancel_reason) %>% filter(n == max(n))
#
veh_highest_km <- qr %>% group_by(vehicle_type) %>% 
  summarise_at(vars(distance_km), funs(sum(., na.rm = TRUE))) %>% 
  slice_max(distance_km)
####################################################################################################
b1 <- valueBox(color = 'red', 
               value = round(mean(qr$distance_km)), 
               subtitle = 'Average Kilometers Covered')

b2 <- valueBox(color = 'green', 
               value = 89001, 
               subtitle = 'Bike had the highest distance(Km)')

b3 <- valueBox(color = 'maroon', 
               value = 1775, 
               subtitle = 'No driver allocated to request')
#############################################################
header <- dashboardHeader(title = 'Sendy Junior-Data-Analyst Case-Study', titleWidth = 400)
#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = 'Dashboard', tabName = 'dashboard', icon = icon('dashboard'), badgeColor = 'green'),
    menuItem(text = 'Conditions',  tabName = 'conditions', badgeColor = 'orange', icon = icon("th"))
  )
)
#
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'dashboard',
      fluidRow(valueBoxOutput('box1'),
               valueBoxOutput('box2'),
               valueBoxOutput('box3')),
      fluidRow(box(width = 6, plotlyOutput('plot1', height = '500px'), height = "500px", status = 'primary'),
               box(width = 6, verbatimTextOutput('text1'), title = 'Recommendations', status = 'info', solidHeader = TRUE))
    ),
    tabItem(tabName = 'conditions',
            fluidRow(box(title = 'Conditional Filters', status = 'success', solidHeader = TRUE, width = 12,
                         column(width = 2, style='padding:1px;', numericInput("distid", label = "Distance (Km)", value = 10, step = 1, min = 0, max = 20)),
                         column(width = 2, style='padding:1px;', selectInput(inputId = 'vehid', label = 'Vehicle', choices = unique(qr$vehicle_type), selected = "Business", selectize = TRUE)),
                         column(width = 2, style='padding:1px;', selectInput(inputId = 'typeid', label = 'Order Type', choices = unique(qr$standard_or_express), selected = "Business", selectize = TRUE)),
                         column(width = 2, style='padding:1px;', selectInput(inputId = 'platformid', label = 'Platform', choices = unique(qr$platform), selected = "Business", selectize = TRUE)),
                         column(width = 2, style='padding:1px;', selectInput(inputId = 'cancelid', label = 'Cancelled By', choices = unique(qr$cancelled_by), selected = "Business", selectize = TRUE)),
                         column(width = 2, style='padding:1px;', selectInput(inputId = 'scheduleid', label = 'Schedule', choices = unique(qr$scheduled), selected = "Business", selectize = TRUE)))),
            fluidRow(box(width = 8, solidHeader = TRUE, plotlyOutput('plot2', height = '500px')),
                     box(width = 4, solidHeader = TRUE, status = 'info', title = 'Day of the Week',
                         radioButtons(inputId = 'dayid', label = 'Day of the week', choices = unique(qr$placement_day), selected = 'Mon')),
                         shinydashboardPlus::dashboardFooter(right_text = 'Daniel Obare'))
            )
  )
)
#
ui <- dashboardPage(header = header, sidebar = sidebar, body = body)
#
server <- function(input, output) {
  output$box1 <- renderValueBox({b1})
  #
  output$box2 <- renderValueBox({b2})
  #
  output$box3 <- renderValueBox({b3})
  ###################################
  conditional_filter <- reactive({
    qr %>% select(-c(8,10,11)) %>% 
      filter(
             distance_km == input$distid,
             placement_day == input$dayid,
            vehicle_type == input$vehid,
            standard_or_express == input$typeid,
            platform == input$platformid,
            cancelled_by == input$cancelid,
            scheduled == input$scheduleid)
  })
  ###################################
  output$plot1 <- renderPlotly({
    
    qr %>% select(5) %>% ggplot(aes(cancel_reason))+
      geom_bar(stat = 'count', fill = my_colors[5])+
      labs(x = 'interviewer', y = 'Value count', title = 'Frequency of Cancellation Reasons.')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 35),
            axis.text.y = element_text(face = 'bold', size = 12)) -> p1
    ggplotly(p1) 
    
  })
  ####
  output$plot2 <- renderPlotly({
    
    conditional_filter() %>% ggplot(aes(cancel_reason, fill = personal_or_business))+
      geom_bar(stat = 'count', position = 'dodge')+
      labs(x = 'interviewer', y = 'Value count', title = 'Frequency of Cancellation Reasons per Agenda.')+
      theme(axis.title = element_text(size = 12),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14),
            axis.text.x = element_text(face = 'bold', size = 12, angle = 35),
            axis.text.y = element_text(face = 'bold', size = 12))+
      scale_fill_manual(values = c(my_colors[3],my_colors[4]))-> p2
    ggplotly(p2)
    
  })
  ####
  output$text1 <- renderText({
    paste(sep = '\n',
      "1. Use of Voronoi diagrams to strategically position vehicles such that there 
      is at least two vehicles to facilitate an order to evade cancellations 
      due to delays or the driver is too far away", 
      "2. Outsource more drivers to curb issue of no driver allocated",
      "3. Allow time for clients to make more orders",
      "5. Improve efficiency and availability on orders on demand",
      "6. Detailed location descriptions on android apps")
  })
}
#
shinyApp(ui, server)