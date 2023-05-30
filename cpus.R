library(tidyverse)
library(shiny)

# Smartphone Processors Ranking & Scores

cpus <- read_csv("ML_ALL_benchmarks.csv")

ui <- fluidPage(
    tableOutput("cpuTable"),
    tableOutput("gpuTable"),
    tableOutput("tpuTable"),
    tableOutput("clockDist"),
    tableOutput("coresDist"),
    tableOutput("perfomance")
)

server <- function(input, output, session){

    output$cpuTable <- renderTable(

        # Top CPU Performance
        cpus %>%
            arrange(desc(cpuScore)) %>%
            head(10)
    )

    output$gpuTable <- renderTable(

        # Top GPU Performance
        cpus %>%
            arrange(desc(gpuScore)) %>%
            head(10)
    )

    output$tpuTable <- renderTable(

        # Top TPU Performance
        cpus %>%
            arrange(desc(npuScore)) %>%
            head(10)
    )

    output$clockDist <- renderPlot(

        # Clock Speed Distribution
        ggplot(cpus, mapping = aes(x = clock)) +
            geom_histogram(binwidth = 500) +
            xlab("Clock speed") +
            ggtitle("Clock speed distribution")
    )

    output$perfomance <- renderTable(

        # Top Performance by Producent
        cpus %>%
            group_by(company) %>%
            summarize(averageCPUPerformance = mean(cpuScore),
                      averageGPUPerformance = mean(gpuScore),
                      averageNPUPerformance = mean(npuScore))
    )

    output$coresDist <- renderPlot(

        # Cores Distribution
        ggplot(cpus, mapping = aes(x = cores)) +
            geom_histogram(binwidth = 1) +
            xlab("Number of Cores") +
            ggtitle("Core count distribution")
    )
}

shinyApp(ui, server)
