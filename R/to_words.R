#' Spell numbers into words (wrapper)
#'
#' @description
#' `to_words()` dispatches to language-specific functions: `to_words_en()` or
#' `to_words_fr()`.
#'
#' @param x Numeric vector.
#' @param lang Language code: `"en"` (English) or `"fr"` (French).
#' @param ... Passed to the language-specific function.
#' @return Character vector.
#' @examples
#' to_words(21, lang = "fr")
#' to_words(21, lang = "en")
#' @export
to_words <- function(x, lang = c("en", "fr"), ...) {
  lang <- match.arg(lang)
  switch(
    lang,
    en = to_words_en(x, ...),
    fr = to_words_fr(x, ...)
  )
}
