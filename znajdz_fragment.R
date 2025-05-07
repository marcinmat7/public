library(shiny)
library(DT)
library(stringr)

# Hardcoded folder z plikami
code_folder <- "C:/Users/garri/Desktop/Nauka/R stuff/"

ui <- fluidPage(
    titlePanel("Wyszukiwanie fraz w skryptach"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("extension", "Wybierz rozszerzenie:", choices = c(".R", ".py")),
            textInput("phrase", "Wpisz frazę do wyszukania:", value = ""),
            actionButton("search", "Szukaj")
        ),
        
        mainPanel(
            DTOutput("results_table"),
            verbatimTextOutput("code_snippet")
        )
    )
)

server <- function(input, output, session) {
    results <- reactiveVal(data.frame(File = character(), Line = integer(), Text = character(), stringsAsFactors = FALSE))
    
    observeEvent(input$search, {
        req(input$phrase)
        
        # Pobieranie plików z rozszerzeniem
        files <- list.files(code_folder, pattern = paste0("\\", input$extension, "$"), full.names = TRUE)
        # files <- list.files("C:/Users/garri/Desktop/Nauka/PCA", full.names = TRUE)
        found <- data.frame(File = character(), Line = integer(), Text = character(), stringsAsFactors = FALSE)
        
        for (f in files) {
            lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
            match_lines <- which(str_detect(lines, fixed(input$phrase, ignore_case = TRUE)))
            if (length(match_lines) > 0) {
                found <- rbind(found, data.frame(
                    File = rep(basename(f), length(match_lines)),
                    Line = match_lines,
                    Text = lines[match_lines],
                    stringsAsFactors = FALSE
                ))
            }
        }
        results(found)
    })
    
    output$results_table <- renderDT({
        datatable(results(), selection = "single", options = list(pageLength = 10))
    })
    
    output$code_snippet <- renderPrint({
        selected <- input$results_table_rows_selected
        if (length(selected)) {
            res <- results()
            row <- res[selected, ]
            full_path <- file.path(code_folder, row$File)
            lines <- readLines(full_path, warn = FALSE, encoding = "UTF-8")
            
            # Wyciągamy 3 linie przed i 3 po
            start <- max(1, row$Line - 3)
            end <- min(length(lines), row$Line + 3)
            cat(paste0(seq(start, end), ": ", lines[start:end]), sep = "\n")
        }
    })
}

shinyApp(ui, server)
