library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ez)
library(DT) 

ui <- fluidPage(
  titlePanel("Empathy Experiment Results"),
  mainPanel(
    plotOutput("totalPlot"),
    verbatimTextOutput("anova_Total"),
    plotOutput("conditionPlot"), # Add this line
    verbatimTextOutput("anova_EC"),  # Add this line
    verbatimTextOutput("anova_F"),  # Add this line
    verbatimTextOutput("anova_PD"),  # Add this line
    verbatimTextOutput("anova_PT"),
    DT::dataTableOutput("data") 
  )
)

server <- function(input, output) {
  data <- reactive({
    df <- read_sheet("https://docs.google.com/spreadsheets/d/1zm3Sv6F_4fEciHjSu1N8vMAeDL9k6olo6irfgXHrJB0")
    names(df) <- c("Time", "Code", "Condition", paste0("Item", 1:16))
    df$EC <- rowSums(df[,c("Item1", "Item5", "Item9", "Item10")])
    df$F <- rowSums(df[,c("Item2", "Item7", "Item11", "Item14")])
    df$PD <- rowSums(df[,c("Item3", "Item8", "Item12", "Item15")])
    df$PT <- rowSums(df[,c("Item4", "Item6", "Item13", "Item16")])
    df$Total <- rowSums(df[,c("EC", "F", "PD", "PT")])
    df$Condition <- factor(df$Condition, levels = c("Before any test", "After video", "After VR experience"))  # Add this line
    df
  })
  
  output$conditionPlot <- renderPlot({
    df <- data()
    df_melt <- reshape2::melt(df, id.vars = "Condition", measure.vars = c("EC", "F", "PD", "PT"))
    ggplot(df_melt, aes(x = variable, y = value, fill = Condition)) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge") +
      labs(title = "Sub-score Results", x = "Variable", y = "Mean Value", fill = "Condition") +
      theme_minimal()
  })
  
  output$totalPlot <- renderPlot({
    df <- data()
    ggplot(df, aes(x = Condition, y = Total)) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge") +
      labs(title = "Total Scores Results", x = "Condition", y = "Mean Total Value") +
      theme_minimal()
  })
  
  output$data <- DT::renderDataTable({  # Modify this block
    df <- data()
    DT::datatable(df, options = list(pageLength = 25))
  })
  
  # Add these blocks
  output$anova_Total <- renderPrint({
    df <- data()
    aov_results <- ezANOVA(
      data = df,
      dv = .(Total),
      wid = .(Code),
      within = .(Condition),
      detailed = TRUE,
      type = 3
    )
    if (aov_results$ANOVA[2, "p"] < 0.05) {
      print("The effect of Condition on Total is statistically significant.")
    } else {
      print("The effect of Condition on Total is not statistically significant.")
    }
  })
  
  output$anova_EC <- renderPrint({
    df <- data()
    aov_results <- ezANOVA(
      data = df,
      dv = .(EC),
      wid = .(Code),
      within = .(Condition),
      detailed = TRUE,
      type = 3
    )
    if (aov_results$ANOVA[2, "p"] < 0.05) {
      print("The effect of Condition on EC is statistically significant.")
    } else {
      print("The effect of Condition on EC is not statistically significant.")
    }
  })
  
  output$anova_F <- renderPrint({
    df <- data()
    aov_results <- ezANOVA(
      data = df,
      dv = .(F),
      wid = .(Code),
      within = .(Condition),
      detailed = TRUE,
      type = 3
    )
    if (aov_results$ANOVA[2, "p"] < 0.05) {
      print("The effect of Condition on F is statistically significant.")
    } else {
      print("The effect of Condition on F is not statistically significant.")
    }
  })
  
  output$anova_PD <- renderPrint({
    df <- data()
    aov_results <- ezANOVA(
      data = df,
      dv = .(PD),
      wid = .(Code),
      within = .(Condition),
      detailed = TRUE,
      type = 3
    )
    if (aov_results$ANOVA[2, "p"] < 0.05) {
      print("The effect of Condition on PD is statistically significant.")
    } else {
      print("The effect of Condition on PD is not statistically significant.")
    }
  })
  
  output$anova_PT <- renderPrint({
    df <- data()
    aov_results <- ezANOVA(
      data = df,
      dv = .(PT),
      wid = .(Code),
      within = .(Condition),
      detailed = TRUE,
      type = 3
    )
    if (aov_results$ANOVA[2, "p"] < 0.05) {
      print("The effect of Condition on PT is statistically significant.")
    } else {
      print("The effect of Condition on PT is not statistically significant.")
    }
  })
  
}

shinyApp(ui = ui, server = server)