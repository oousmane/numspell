
# numspell

<!-- badges: start -->
<!-- badges: end -->

The goal of numspell is to ...

## Installation

You can install the development version of numspell from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("oousmane/numspell")
```

## Example

``` r
library(numspell)

# Wrapper
to_words(21, lang = "fr")  # "vingt et un"
to_words(21, lang = "en")  # "twenty-one"

# French
enlettres(8, unit = "jours")                          # "huit jours"
to_words_fr(1523.5, decimal = "centimes", unit = "euro", subunit = "centime")

# English
spell_en(101, style = "uk")                           # "one hundred and one"
to_words_en(1523.5, decimal = "cents", unit = "dollar", subunit = "cent")

```
