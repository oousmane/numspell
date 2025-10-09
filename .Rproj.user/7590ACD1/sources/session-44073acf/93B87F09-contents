#' English number words
#'
#' @description
#' Convert numbers to English words with hyphenation (e.g., "twenty-one"),
#' short scale (thousand, million, billion), negatives, and decimal handling.
#'
#' @param x Numeric vector.
#' @param style Either `"us"` (default, no mandatory "and") or `"uk"`
#'   (inserts "and" for numbers like 101 -> "one hundred and one").
#' @param decimal One of `"digits"` or `"cents"` (currency-like; two decimals).
#' @param unit Optional major unit for integers in `"digits"` mode (e.g., "day"/"days").
#' @param unit_plural Optional explicit plural for `unit`.
#' @param subunit Subunit label for `"cents"` mode (default `"cent"`).
#' @return Character vector.
#' @examples
#' to_words_en(21)                         # "twenty-one"
#' to_words_en(101, style="uk")            # "one hundred and one"
#' to_words_en(8, unit="days")             # "eight days"
#' to_words_en(1523.5, decimal="cents", unit="dollar", subunit="cent")
#' @export
to_words_en <- function(x,
                        style = c("us", "uk"),
                        decimal = c("digits", "cents"),
                        unit = NULL,
                        unit_plural = NULL,
                        subunit = "cent") {

  style <- match.arg(style)
  decimal <- match.arg(decimal)

  units_0_19 <- c(
    "zero","one","two","three","four","five","six","seven","eight","nine",
    "ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen",
    "seventeen","eighteen","nineteen"
  )
  tens_words <- c("", "", "twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety")

  under_100 <- function(n) {
    stopifnot(n >= 0, n < 100)
    if (n < 20) return(units_0_19[n + 1])
    t <- n %/% 10; r <- n %% 10
    base <- tens_words[t + 1]
    if (r == 0) base else paste0(base, "-", units_0_19[r + 1])
  }

  under_1000 <- function(n) {
    stopifnot(n >= 0, n < 1000)
    if (n < 100) return(under_100(n))
    h <- n %/% 100; r <- n %% 100
    hpart <- paste(units_0_19[h + 1], "hundred")
    if (r == 0) return(hpart)
    if (style == "uk") paste(hpart, "and", under_100(r)) else paste(hpart, under_100(r))
  }

  chunk_with_scale <- function(n, scale_word) {
    paste(under_1000(n), scale_word)
  }

  spell_integer_one <- function(n) {
    stopifnot(abs(n) < 1e12)
    if (n == 0) return("zero")
    if (n < 0) return(paste("minus", spell_integer_one(-n)))

    out <- character()
    billions <- n %/% 1e9; n <- n %% 1e9
    millions <- n %/% 1e6; n <- n %% 1e6
    thousands<- n %/% 1e3; n <- n %% 1e3
    rest    <- n

    if (billions) out <- c(out, chunk_with_scale(billions, "billion"))
    if (millions)  out <- c(out, chunk_with_scale(millions,  "million"))
    if (thousands) out <- c(out, chunk_with_scale(thousands, "thousand"))
    if (rest)      out <- c(out, under_1000(rest))

    paste(out, collapse = " ")
  }

  spell_decimal_digits <- function(s) {
    digits <- strsplit(s, "")[[1]]
    paste(sapply(digits, function(d) units_0_19[as.integer(d) + 1]), collapse = " ")
  }

  choose_unit <- function(n, unit, unit_plural) {
    if (is.null(unit)) return(NULL)
    if (!is.null(unit_plural)) return(if (abs(n) == 1) unit else unit_plural)
    # naive pluralization: add 's' if not 1
    if (abs(n) == 1) unit else paste0(unit, "s")
  }

  en_one <- function(v) {
    if (is.na(v)) return(NA_character_)

    if (decimal == "digits") {
      s <- as.character(v)
      if (!grepl("\\.", s, fixed = TRUE)) {
        words <- spell_integer_one(as.numeric(s))
        if (!is.null(unit)) {
          u <- choose_unit(as.numeric(s), unit, unit_plural)
          words <- paste(words, u)
        }
        return(words)
      }
      parts <- strsplit(s, "\\.", fixed = TRUE)[[1]]
      intp <- suppressWarnings(as.numeric(parts[1]))
      decp <- sub("0+$", "", parts[2])
      if (identical(decp, "") || is.na(decp)) {
        words <- spell_integer_one(intp)
        if (!is.null(unit)) {
          u <- choose_unit(intp, unit, unit_plural)
          words <- paste(words, u)
        }
        return(words)
      } else {
        return(paste(spell_integer_one(intp), "point", spell_decimal_digits(decp)))
      }
    } else {
      # currency-like
      sign_str <- if (v < 0) "minus " else ""
      v_abs <- abs(v)
      intp  <- floor(v_abs)
      cents <- round((v_abs - intp) * 100)
      if (cents == 100) { intp <- intp + 1; cents <- 0 }

      major <- if (is.null(unit)) "dollar" else unit
      minor <- subunit
      major_pl <- if (!is.null(unit_plural)) unit_plural else paste0(major, "s")
      minor_pl <- paste0(minor, "s")

      major_word <- if (intp == 1) major else major_pl
      minor_word <- if (cents == 1) minor else minor_pl

      if (cents == 0) {
        paste0(sign_str, spell_integer_one(intp), " ", major_word)
      } else if (intp == 0) {
        paste0(sign_str, spell_integer_one(cents), " ", minor_word)
      } else {
        paste0(sign_str, spell_integer_one(intp), " ", major_word,
               " and ", spell_integer_one(cents), " ", minor_word)
      }
    }
  }

  vapply(x, en_one, FUN.VALUE = character(1))
}
