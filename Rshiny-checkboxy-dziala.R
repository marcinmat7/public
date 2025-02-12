library(shiny)
library(DT)

# Dane przykładowe
data <- data.frame(
    ID = 1:10,
    Name = paste("Item", 1:10),
    Value = sample(10:200, 10)
)

# UI aplikacji
ui <- fluidPage(
    titlePanel("Tabela z checkboxami"),
    sidebarLayout(
        sidebarPanel(
            h3("Opcje"),
            actionButton("process", "Wykorzystaj zaznaczone wiersze"),
            hr(),
            h4("Zaznaczone wiersze:"),
            verbatimTextOutput("selected_rows"),
            hr(),
            h4("Podsumowanie:"),
            verbatimTextOutput("row_count")
        ),
        mainPanel(
            h3("Tabela"),
            DTOutput("table")
        )
    )
)

# Serwer aplikacji
server <- function(input, output, session) {
    # Lista początkowo zaznaczonych ID
    preselected_ids <- data$ID[data$Value > 100]
    
    # Renderowanie tabeli z checkboxami
    output$table <- renderDT({
        datatable(
            data,
            selection = "none",
            escape = FALSE,
            rownames = FALSE,
            callback = JS(
                paste0(
                    "table.on('click', 'input[type=\"checkbox\"]', function() {",
                    "  var data = [];",
                    "  table.$('input[type=\"checkbox\"]:checked').each(function(){",
                    "    data.push($(this).val());",
                    "  });",
                    "  Shiny.setInputValue('row_selected', data);",
                    "});",
                    "$(document).ready(function(){",
                    paste(
                        sprintf(
                            "$('input[type=\"checkbox\"][value=\"%s\"]').prop('checked', true);",
                            preselected_ids
                        ),
                        collapse = ""
                    ),
                    "});"
                )
            ),
            options = list(
                columnDefs = list(
                    list(
                        targets = 0,
                        render = JS(
                            "function(data, type, row, meta) {",
                            "  return '<input type=\"checkbox\" value=\"' + row[0] + '\">';",
                            "}"
                        )
                    )
                )
            )
        )
    }, server = FALSE)
    
    # Wyświetlanie zaznaczonych wierszy
    output$selected_rows <- renderPrint({
        if (is.null(input$row_selected)) {
            "Brak zaznaczonych wierszy."
        } else {
            input$row_selected
        }
    })
    
    # Wyświetlanie liczby zaznaczonych wierszy po kliknięciu przycisku
    output$row_count <- renderPrint({
        input$process
        isolate({
            if (is.null(input$row_selected)) {
                "Liczba zaznaczonych wierszy: 0"
            } else {
                paste("Liczba zaznaczonych wierszy:", length(input$row_selected))
            }
        })
    })
}

# Uruchomienie aplikacji
shinyApp(ui, server)
