library("shiny")
library("highcharter")
library(dplyr)

ui <- shinyUI(
    fluidPage(
        column(width = 8, highchartOutput("hcontainer", height = "500px")),
        column(width = 4, textOutput("text")),
        column(width = 4, dataTableOutput('temptable')))
)


server <- function(input, output) {      
    
    a <- data.frame(b = LETTERS[1:5], c = 11:15)
    aa <- data.frame(b = LETTERS[1:5])
    
    
    output$hcontainer <- renderHighchart({      
        
        #canvasClickFunction <- JS("function(event) {Shiny.setInputValue('canvasClicked', [this.name, event.point.name, Math.random()]);}")
        #legendClickFunction <- JS("function(event) {Shiny.setInputValue('legendClicked', this.name);}")
        click_js <- JS(paste0("function(event) {let now = new Date(); Shiny.onInputChange('PieClicked',event.point.name + ' Timestamp ' + now + now.getMilliseconds());}"))
        
        hc <- a %>%
            hchart(
                "pie", hcaes(x = b, y = c) 
            ) %>%
            hc_plotOptions(series = list(
                innerSize = '60%',
                allowPointSelect= TRUE,
                slicedOffset = 20,
                states = list(
                    select =  list(
                        color= NULL,
                        borderWidth = 5,
                        borderColor = '#ccc'
                    )),
                events = list(#click = canvasClickFunction, 
                              #legendItemClick = legendClickFunction
                    click=click_js
                    ))
                    
                ) #%>% hc_add_event_point(event = "unselect")
        hc
        
        
    })      
    
    makeReactiveBinding("outputText")
    
    rv <- reactiveValues(lstval=0,curval=0)
    
    rv$CurrentSelection <- NULL
    rv$PreviousSelection <- NULL
    
    # observeEvent(input$canvasClicked[2], {
    #     rv$lstval <- rv$curval; 
    #     rv$curval <- input$canvasClicked[2]}
    # )
    # 
    # curre <- reactive({req(input$canvasClicked[2]); input$canvasClicked[2]; rv$curval})
    # lstre <- reactive({req(input$canvasClicked[2]); input$canvasClicked[2]; rv$lstval})
    
    # observeEvent(input$canvasClicked, {
    #     outputText <<- paste0("You clicked on series ", input$canvasClicked[1], " and the bar you clicked was from category ", input$canvasClicked[2], 
    #                           input$plot_hc_unselect, ".")
    # })
    
    # observeEvent(input$legendClicked, {
    #     outputText <<- paste0("You clicked into the legend and selected series ", input$legendClicked, ".")
    #     print('triggered')
    # })
    
    # output$text <- renderText({
    #     outputText
    # })
    
    output$text <- renderText({
        rv$CurrentSelection
    })
    
    
    
    # output$temptable <- renderDataTable({
    # 
    #     if (length(input$canvasClicked[2])>0) {
    #         if (curre()!=lstre())
    #             aa %>% filter(b==input$canvasClicked[2])
    #         else {
    #             aa
    #         }
    #     }
    #     else {aa}
    # })
    
    output$temptable <- renderDataTable({
        if(is.null(rv$CurrentSelection)) {
            aa
        }
        else {
            aa %>% filter(b==rv$CurrentSelection)
        }
        

    })
    
    observeEvent( input$PieClicked ,{
        rv$PreviousSelection <- rv$CurrentSelection
        
        if( is.null(input$PieClicked)) { 
            rv$CurrentSelection <- NULL 
        }
        else if ( is.null(rv$PreviousSelection) ) {
            rv$CurrentSelection <- strsplit(input$PieClicked, " Timestamp ")[[1]][1] 
        }
        else if( strsplit(input$PieClicked, " Timestamp ")[[1]][1] == rv$PreviousSelection ) { 
            rv$CurrentSelection <- NULL 
        }
        else { 
            rv$CurrentSelection <- strsplit(input$PieClicked, " Timestamp ")[[1]][1] 
        }
    })
}

shinyApp(ui, server)
