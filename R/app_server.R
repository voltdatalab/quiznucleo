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
        shiny::textInput(
          paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0), "_opt4"),
          "Resposta 4"
        ),
        shiny::selectInput(paste0(
          "q", stringr::str_pad(count_perguntas, 2, pad = 0), "_correta"
        ), "Resposta correta", 1:4),
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
      paste(collapse = ",\n")

    json <- paste0(
      "<div id=\"quiz\">",
      "  <div id=\"quiz-start-screen\">",
      "    <p><a href=\"#\" id=\"quiz-start-btn\" ",
      "class=\"quiz-button\">Começar</a></p>",
      "  </div>",
      "</div>",
      "<script>",
      "    $('#quiz').quiz({",
      "  counterFormat: 'Pergunta %current de %total',",
      "  questions: [",
      json,
      "  ]",
      "});",
      "function copyToClipboard(element) {",
      " var $temp = $(\"<textarea>\");",
      " var $mess = $(\"#quiz-results\");",
      " var $tit = $(\".post-title\");",
      " $($mess).append($tit);",
      " $(\"body\").append($temp).add($tit);",
      " $temp.val($(element).html().replace('<br>', '\r\n').",
      "replace('<h1 class=\"post-title\">', ' | ').",
      "replace('</h1>', '\r\n')).select();",
      " document.execCommand(\"copy\");",
      " $mess.after(\"Resultado copiado! \");",
      " $temp.remove();",
      "}",
      "</script>"
    )

    output$clip <- shiny::renderUI({
      output$clip <- shiny::renderUI({
        rclipboard::rclipButton(
          inputId = "clipbtn",
          label = "Copiar",
          clipText = json,
          icon = shiny::icon("clipboard")
        )
      })
    })

    output$json <- shiny::renderText({
      json
    })

  })


}
