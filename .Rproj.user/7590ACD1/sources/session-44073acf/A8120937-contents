#' French number words
#'
#' @description
#' Convert numbers to French words with correct hyphenation and pluralization
#' (e.g., `cent(s)`, `quatre-vingt(s)`, `soixante et onze`). Supports
#' negatives, up to |n| < 1e12, and decimal handling.
#'
#' @param x Numeric vector.
#' @param decimal One of `"digits"` (read each decimal digit) or `"centimes"`
#'   (currency-like formatting using two decimals).
#' @param unit Optional major unit to append for integers in `"digits"` mode
#'   (e.g., `"jour"` or `"jours"`). If supplied, pluralization is handled
#'   heuristically or via `unit_plural`.
#' @param unit_plural Optional explicit plural for `unit` (e.g., `"chevaux"`).
#' @param subunit Subunit label for `"centimes"` mode (default `"centime"`).
#' @return Character vector.
#' @examples
#' to_words_fr(8, unit = "jours")     # "huit jours"
#' to_words_fr(81)                    # "quatre-vingt-un"
#' to_words_fr(80)                    # "quatre-vingts"
#' to_words_fr(200)                   # "deux cents"
#' to_words_fr(201)                   # "deux cent un"
#' to_words_fr(12.305, decimal="digits")
#' to_words_fr(1523.5, decimal="centimes", unit="euro", subunit="centime")
#' @export
to_words_fr <- function(x,
                        decimal = c("digits", "centimes"),
                        unit = NULL,
                        unit_plural = NULL,
                        subunit = "centime") {

  decimal <- match.arg(decimal)

  unit_words <- c(
    "zéro","un","deux","trois","quatre","cinq","six","sept","huit","neuf",
    "dix","onze","douze","treize","quatorze","quinze","seize"
  )

  under_100 <- function(n) {
    stopifnot(n >= 0, n < 100)
    if (n <= 16) return(unit_words[n + 1])
    tens_words <- c("vingt","trente","quarante","cinquante","soixante")
    if (n < 20) {
      return(paste0("dix-", unit_words[n - 10 + 1]))
    } else if (n < 70) {
      d <- n %/% 10; r <- n %% 10
      base <- tens_words[d - 1L]
      if (r == 0) return(base)
      if (r == 1) return(paste(base, "et un"))
      return(paste0(base, "-", under_100(r)))
    } else if (n < 80) {
      r <- n - 60
      if (r == 11) return("soixante et onze")       # 71
      return(paste("soixante", under_100(r)))
    } else {
      r <- n - 80
      base <- "quatre-vingts"
      if (r == 0) return(base)                      # 80 (with 's')
      base <- "quatre-vingt"                        # drop 's' if followed
      if (r == 1) return(paste0(base, "-un"))       # no 'et' at 81
      return(paste(base, under_100(r)))
    }
  }

  under_1000 <- function(n) {
    stopifnot(n >= 0, n < 1000)
    if (n < 100) return(under_100(n))
    cts <- n %/% 100; r <- n %% 100
    if (cts == 1) {
      if (r == 0) return("cent")
      return(paste("cent", under_100(r)))
    } else {
      if (r == 0) return(paste(unit_words[cts + 1], "cents"))
      return(paste(unit_words[cts + 1], "cent", under_100(r)))
    }
  }

  chunk_with_scale <- function(n, scale_word, pluralize = TRUE) {
    w <- under_1000(n)
    if (scale_word == "mille") {
      return(if (n == 1) "mille" else paste(w, "mille"))
    }
    if (pluralize && n > 1) paste(w, paste0(scale_word, "s")) else paste(w, scale_word)
  }

  spell_integer_one <- function(n) {
    stopifnot(abs(n) < 1e12)
    if (n == 0) return("zéro")
    if (n < 0) return(paste("moins", spell_integer_one(-n)))

    out <- character()
    milliards <- n %/% 1e9; n <- n %% 1e9
    millions  <- n %/% 1e6; n <- n %% 1e6
    milliers  <- n %/% 1e3; n <- n %% 1e3
    reste     <- n

    if (milliards) out <- c(out, chunk_with_scale(milliards, "milliard"))
    if (millions)  out <- c(out, chunk_with_scale(millions,  "million"))
    if (milliers)  out <- c(out, chunk_with_scale(milliers,  "mille"))
    if (reste)     out <- c(out, under_1000(reste))
    paste(out, collapse = " ")
  }

  spell_decimal_digits <- function(s) {
    digits <- strsplit(s, "")[[1]]
    paste(sapply(digits, function(d) unit_words[as.integer(d) + 1]), collapse = " ")
  }

  choose_unit <- function(n, unit, unit_plural) {
    if (is.null(unit)) return(NULL)
    if (!is.null(unit_plural)) return(if (abs(n) == 1) unit else unit_plural)
    ends_s <- grepl("s$", unit, perl = TRUE)
    if (abs(n) == 1) {
      if (ends_s) sub("s$", "", unit) else unit
    } else {
      if (ends_s) unit else paste0(unit, "s")
    }
  }

  fr_one <- function(v) {
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
        return(paste(spell_integer_one(intp), "virgule", spell_decimal_digits(decp)))
      }
    } else {
      # currency-like
      sign_str <- if (v < 0) "moins " else ""
      v_abs <- abs(v)
      intp  <- floor(v_abs)
      cents <- round((v_abs - intp) * 100)
      if (cents == 100) { intp <- intp + 1; cents <- 0 }

      major <- if (is.null(unit)) "euro" else unit
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
               " et ", spell_integer_one(cents), " ", minor_word)
      }
    }
  }

  vapply(x, fr_one, FUN.VALUE = character(1))
}
