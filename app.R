if(!require("dplyr")){
  install.packages("dplyr")
}
if(!require("ggplot2")){
  install.packages("ggplot2")
}
if(!require("ggExtra")){
  install.packages("ggExtra")
}
if(!require("plotly")){
  install.packages("plotly")
}
if(!require("shiny")){
  install.packages("shiny")
}
if(!require("knitr")){
  install.packages("knitr")
}
if(!require("kableExtra")){
  install.packages("kableExtra")
}
if(!require("shinydashboard")){
  install.packages("shinydashboard")
}
if(!require("shinydashboardPlus")){
  install.packages("shinydashboardPlus")
}

suppressWarnings({
  phenotype_apriori = read.csv("phenotype_apriori.csv") %>%
    mutate_if(is.factor,as.character)
  met = read.csv("metabolite_master.csv")
  pheno = read.csv("2015_aug_dec_2016_dec.csv")
  pheno_met = dplyr::inner_join(pheno,met,by = "id") %>%
    mutate(bmi = weight/(height^2))
  pheno = dplyr::select(pheno_met,id,bmi,phenotype_apriori$phenotype)
  controlled_pheno = dplyr::select(pheno,id,sex,age,bmi)
  pheno = dplyr::select(pheno,-id,-sex,-age,-bmi) %>%
    dplyr::mutate_if(is.factor,as.numeric) %>%
    cbind.data.frame(controlled_pheno)
  met = dplyr::select(pheno,id) %>%
    dplyr::left_join(met,by = "id") %>%
    dplyr::select(-id)
  pheno_met = cbind.data.frame(met,pheno)
  met_names = paste0("M",seq(1,5957))
  pheno_names = phenotype_apriori$phenotype[-(1:2)]
})

ui = dashboardPagePlus(header = dashboardHeaderPlus(title = "CDP Metabolomics",titleWidth = 400),
                       sidebar = dashboardSidebar(width = 400,
                                                  sidebarMenu(id = "input_data",
                                                              menuItem("Input Data",startExpanded = TRUE,
                                                                       selectInput(inputId = "pheno",label = NULL,
                                                                                   choices = as.list(pheno_names)),
                                                                       textInput(inputId = "met",label = NULL,
                                                                                 placeholder = "M1070",value = "M1070"),
                                                                       br(),
                                                                       actionButton(inputId = "start_plotting",
                                                                                    label = "Plot"),
                                                                       br()
                                                                       
                                                              ),
                                                              menuItem("Developer Info",startExpanded = FALSE,
                                                                       
                                                                       p("This App is developped by Wanjun Gu."),
                                                                       # br(),
                                                                       p("Visit his GitHub Repo for more info:"),
                                                                       # br(),
                                                                       a("https://github.com/Broccolito",
                                                                         href = "https://github.com/Broccolito")
                                                                       
                                                              )
                                                  )
                                                  
                       ),
                       body = dashboardBody(
                         box(
                           plotlyOutput(outputId = "plot_output",height = "600px")
                         ),
                         # br(),
                         box(
                           verbatimTextOutput(outputId = "regression_stats")
                         )
                       )
)


server = function(input, output, session){
  
  output$regression_stats = renderText({
    "Regression statistics will be displayed here: "
  })
  
  observeEvent(input$start_plotting,{
    
    x_name = as.character(input$met)
    y_name = as.character(input$pheno)
    
    plot_data = data.frame(x = pheno_met[[x_name]],
                           y = pheno_met[[y_name]],
                           Sex = pheno_met[["sex"]],
                           age = pheno_met[["age"]],
                           bmi = pheno_met[["bmi"]])
    
    plt = ggplot(data = plot_data,aes(x = x,y = y, fill = Sex)) +
      geom_point(aes(col = Sex),size = 2) +
      geom_smooth(aes(col = Sex), method = "lm",formula = "y~x") +
      xlab(x_name) +
      ylab(y_name) +
      # theme_bw() +
      theme(text = element_text(size = 15),legend.position = "bottom")
    
    plt = ggplotly(plt)
    
    output$plot_output = renderPlotly({
      plt
    })
    
    output$regression_stats = renderText({
      tx = ""
      try({
        fit = lm(data = plot_data, formula = "y~x+Sex+age+bmi")
        tx = paste(capture.output(summary(fit)),collapse = "\n")
      },silent = TRUE)
      
      if(tx==""){
        fit = lm(data = plot_data, formula = "y~x+age+bmi")
        tx = paste(capture.output(summary(fit)),collapse = "\n")
      }
      tx
      
    })
    
  })
  
  onSessionEnded(fun = function(){stopApp()})
}

shinyApp(ui, server)


