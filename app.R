library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggiraph)
source("www/load_data.R")
source("www/home_functions.R")
source("www/sector_functions.R")
source("www/city_functions.R")



# Define UI for application that draws a histogram
ui <- dashboardPage(skin="green",
        
  dashboardHeader(),
    dashboardSidebar(
      h2("MENU"),
      sidebarMenu(
        menuItem("Home",tabName = "home", icon=icon("home")),
        menuItem("Sector",tabName = "sectors", icon=icon("industry")),
        menuItem("City",tabName = "cities", icon=icon("globe-americas"))
      )
    ),
    dashboardBody(
      includeCSS("www/mainstyle.css"),
      tabItems(
        tabItem("home",
            tags$div(class="secondaryHeading"),
            #ValueBox Outputs
            fluidRow(
              valueBoxOutput("Total_Applicaitons_Input",width = 3),
              valueBoxOutput("Total_Approved_Input",width = 3),
              valueBoxOutput("Total_Rejected_Input",width = 3),
              valueBoxOutput("Total_Evaluation_Ongoing_Input",width = 3)
            ),
            
            fluidRow(class="fluidrow2",
              box(plotOutput("Plot_App_Acc"), width = 12)
            ),
            
            fluidRow(class="fluidrow3",
                     box(plotOutput("Plot_Grant"), width = 12)
            )
         ),#tabItem_Home
         
         tabItem("sectors",
                  
            fluidRow(
              div(class="firstrow",
              box(class="gttable", width = 5, height="1220px",
                  gt_output(outputId ="sector_table")),
              box(width = 7,  height="1220px",
                  box(width = 12, #height = 100,
                      box(class="select_sector",width=6,
                          selectInput("select_sector", label = h3("Sector"), 
                                      choices = Choices_List, 
                                      selected = 1)),
                      box(class="select_date",width=6)
                      ),
                  box(class="Sector_Info",title="Sector Info", width = 12, #height = 100,
                      textOutput("sector_info")),
                  box(class="Sector_Plot1", width = 12, 
                      plotOutput("Plot1_Sector")),
                  box(class="Sector_Plot2", width = 12,
                      plotOutput("Plot2_Sector")))
              )
            ),
            
            fluidRow(
              box(width=12,
              div(class="box1",    
              box(class="top_companies_table", width = 6,
                  gt_output(outputId ="top_company_in_sector")), #height = 100,),
              box(class="top_companies_pie", width = 6, 
                  plotOutput("Pieplot_Sector")) #height = 100,
              )
              )
            )
          ),#tabItem_Sector
            
            tabItem("cities",
                    
                    fluidRow(
                      box(class="ggiraph",width=5, height="620px",
                          girafeOutput("ggiraph_plot")),
                      box(width = 7, height="620px",
                          div(class="cityInput",
                              box(width=6,
                                  includeScript(path="www/set_search_val.js"),
                                  textInput("company_city", label=NULL, 
                                            placeholder="Please Enter A City Name Or Choose From Map")
                               )
                              ),
                      box(width=12,
                        gt_output("ggiraph_df")),
                      box(width=12, #title= "zeliha", 
                        textOutput("text1")),
                      box(width=12, #title = "serdar",
                        gt_output("gg")))
                    )
              )#tabItem_City
    )#tabItems
  )#dashboardBody
)#dashboardPage

server <- function(input, output) {
  
  #Increase Graph Quality
  library(Cairo)
  options(shiny.usecairo=T)

  
  #Home Pane Outputs
  output$Total_Applicaitons_Input <- renderValueBox({valueBox(value=Total_Applications,
                                                              subtitle="Total Applications")})
  
  output$Total_Approved_Input <- renderValueBox({valueBox(value=Total_Approved,
                                                          subtitle="Total Approved")})
  
  output$Total_Rejected_Input <- renderValueBox({valueBox(value=Total_Rejected,
                                                          subtitle="Total Rejected")})
  
  output$Total_Evaluation_Ongoing_Input <- renderValueBox({valueBox(value=Total_Evaluation_Ongoing,
                                                                    subtitle="Total Evaluating")})
  
  output$Plot_App_Acc <- renderPlot({Plot_Yearly_Application_Approved}) 
  output$Plot_Grant <- renderPlot({Plot_Yearly_Grant})
  
  # Sector Pane Outputs
  output$sector_table <- render_gt(expr = Sector_Table_gt)
  output$sector_info <- renderText(sector_info_function(input$select_sector))
  output$Plot1_Sector <- renderPlot(sector_plot1_function(input$select_sector))
  output$Plot2_Sector <- renderPlot(sector_plot2_function(input$select_sector))
  output$top_company_in_sector <- render_gt(expr = top5_company_sector_function(input$select_sector))
  output$Pieplot_Sector <- renderPlot(sector_piechart(input$select_sector))
  
  
  #City Pane Outputs
  output$ggiraph_plot <- renderGirafe(Ggiraph_Map_Plot)
  output$ggiraph_df <- render_gt(expr = ggiraph_table(input$company_city))
  
  output_condition <- reactive({first_inyear(input$company_city) %>% nrow()})
  
  output$text1 <- renderText({
    if(output_condition()==0){
      paste0("There is no company applying for projects for the first time in 2021")
    }
  })
  
  output$gg <- render_gt({
    if(output_condition()>=1){
      expr = first_inyear_gt(input$company_city)
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
