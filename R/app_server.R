#' The application server-side
#'
#' @param input, output, session Internal parameters for {shiny}.
#'
#' @noRd
app_server <- function(input, output, session) {

  count_perguntas <- 1

  output$q01_opt_correto <- shiny::renderUI({
    n_respostas <- as.numeric(input$add)
    shiny::selectInput("q01_correta", "Resposta correta", 1:n_respostas)
  })

  shiny::observeEvent(input$add_panel, {

    count_perguntas <<- count_perguntas + 1
    n_q <- paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0))

    bslib::accordion_panel_insert(
      "perguntas",
      bslib::accordion_panel(
        paste("Pergunta", count_perguntas),
        shiny::textInput(n_q, "Texto da pergunta"),
        shiny::actionButton(paste0(n_q, "_add"), "Adicionar resposta"),
        shiny::selectInput(paste0(n_q, "_correta"), "Resposta correta", 1),
        shiny::uiOutput(paste0(n_q, "resp")),
        shiny::textInput(paste0(n_q, "_output_correto"), "Output correto"),
        shiny::textInput(paste0(n_q, "_output_errado"), "Output errado"),
      )
    )

    shiny::observeEvent(input[[paste0(n_q, "_add")]], {
      shiny::insertUI(
        selector = paste0("#", n_q, "_add"),
        where = "beforeBegin",
        ui = shiny::tagList(
          shiny::textInput(
            paste0(n_q,"_opt", input[[paste0(n_q, "_add")]]),
            paste("Resposta", input[[paste0(n_q, "_add")]])
          )
        )
      )
    })
    shiny::observeEvent(input[[paste0(n_q, "_add")]], {
      shiny::updateSelectInput(
        inputId = paste0(n_q, "_correta"),
        choices = 1:as.numeric(input[[paste0(n_q, "_add")]])
      )
    })

  })



  shiny::observeEvent(input$add, {
    n_q <- paste0("q", stringr::str_pad(count_perguntas, 2, pad = 0))
    shiny::insertUI(
      selector = "#add",
      where = "beforeBegin",
      ui = shiny::textInput(
        paste0(n_q, "_opt", input$add), paste("Resposta", input$add)
      )
    )
  })

  shiny::observeEvent(input$add, {
    shiny::updateSelectInput(
      inputId = "q01_correta",
      choices = 1:as.numeric(input$add)
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

    json_quiz <- paste0(
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
      " $temp.val($(element).html().",
      "replace('<h1 class=\"post-title\">', ' | ').",
      "replace('</h1>', '')).select();",
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
