gerar_json <- function(i, input) {
  gerar_json_(
    input[[paste0("q", i)]],
    c(
      input[[paste0("q", i, "_opt1")]], input[[paste0("q", i, "_opt2")]],
      input[[paste0("q", i, "_opt3")]]
    ),
    as.numeric(input[[paste0("q", i, "_correta")]]) - 1,
    input[[paste0("q", i, "_output_correto")]],
    input[[paste0("q", i, "_output_errado")]]
  )
}

gerar_json_ <- function(questao, opcoes, correto, txt_correto, txt_errado) {
  teste <- list(
    q = questao,
    options = as.character(opcoes),
    correctIndex = jsonlite::unbox(correto),
    correctResponse = txt_correto,
    incorrectResponse = txt_errado
  ) |>
  jsonlite::toJSON()
}

