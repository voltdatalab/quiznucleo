#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {

  count_perguntas <- 1

  shiny::observeEvent(input$add_panel, {

    count_perguntas <<- count_perguntas + 1

    bslib::accordion_panel_insert(
      "perguntas",
      bslib::accordion_panel(
        paste("Pergunta", count_perguntas),
        shiny::textInput(
          paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0)),
          "Texto da pergunta"
        ),
        shiny::textInput(
          paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0), "_opt1"),
          "Resposta 1"
        ),
        shiny::textInput(
          paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0), "_opt2"),
          "Resposta 2"
        ),
        shiny::textInput(
          paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0), "_opt3"),
          "Resposta 3"
        ),
        shiny::selectInput(paste0(
          "q", stringr::str_pad(count_perguntas, 2, pad = 0), "_correta"
        ), "Resposta correta", 1:3),
        shiny::textInput(paste0(
          "q", stringr::str_pad(count_perguntas, 2, pad = 0), "_output_correto"
        ), "Output correto"),
        shiny::textInput(paste0(
          "q", stringr::str_pad(count_perguntas, 2, pad = 0), "_output_errado"
        ), "Output errado")
      )
    )
  })


  shiny::observeEvent(input$gerar_js, {

    json <- input |>
      names() |>
      purrr::keep(stringr::str_detect, pattern = "q") |>
      stringr::str_extract("(?<=q)[0-9]+") |>
      unique() |>
      sort() |>
      purrr::map_chr(gerar_json, input = input) |>
      paste(collapse = ", ")

    output$json <- shiny::renderText({
      json
    })

    shinyWidgets::sendSweetAlert(
      title = "Copie e cole:", text = json
    )
  })


}
