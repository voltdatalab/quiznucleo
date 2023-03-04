#' The application User-Interface
#'
#' @noRd
app_ui <- function() {

  custom_theme <- function(...) {
    style <- normalizePath("inst/app/style.scss")
    theme <- bslib::bs_theme(...)
    theme <- bslib::bs_add_rules(theme, sass::sass_file(style))
    theme
  }

  shiny::fluidPage(
    shiny::navbarPage(
      theme = custom_theme(),
      title = "Quiz | Núcleo",
      bslib::nav(
        "Perguntas",
        shiny::tagList(
          bslib::accordion(
            id = "perguntas",
            bslib::accordion_panel(
              "Pergunta 1",
              shiny::textInput("q01", "Texto da pergunta"),
              shiny::actionButton("add", "Adicionar resposta"),
              shiny::selectInput("q01_correta", "Resposta correta", 1),
              shiny::textInput("q01_output_correto", "Output correto"),
              shiny::textInput("q01_output_errado", "Output errado")
            )
          ),
          shiny::br(),
          shiny::actionButton("add_panel", "Adicionar pergunta"),
          shiny::actionButton("gerar_js", "Gerar Quiz"),
          rclipboard::rclipboardSetup(),
          shiny::hr(),
          shiny::textOutput("json"),
          shiny::uiOutput("clip")
        )
      )
    )
  )
}
