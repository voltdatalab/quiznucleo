#' The application User-Interface
#'
#' @noRd
app_ui <- function() {

  shiny::fluidPage(
    shiny::navbarPage(
      theme = bslib::bs_theme(bootswatch = "slate"),
      title = "Quiz | Núcleo",
      bslib::nav(
        "Perguntas",
        shiny::tagList(
          bslib::accordion(
            id = "perguntas",
            bslib::accordion_panel(
              "Pergunta 1",
              shiny::textInput("q01", "Texto da pergunta"),
              shiny::textInput("q01_opt1", "Resposta 1"),
              shiny::textInput("q01_opt2", "Resposta 2"),
              shiny::textInput("q01_opt3", "Resposta 3"),
              shiny::selectInput("q01_correta", "Resposta correta", 1:3),
              shiny::textInput("q01_output_correto", "Output correto"),
              shiny::textInput("q01_output_errado", "Output errado")
            )
          ),
          shiny::actionButton("add_panel", "Adicionar pergunta"),
          shiny::actionButton("gerar_js", "Gerar Quiz"),
          shiny::textOutput("json")
        )
      )
    )
  )
}