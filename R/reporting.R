#' Convert a raw p value to conventional notation
#'
#' This function converts a raw _p_ value with any number of decimal places to
#' a string that is used conventionally to report _p_ values in the form of
#' _p_ = x or _p_ < x.
#' @param x A _p_ value.
#' @param max_digits The maximum number of decimal places to show.
#' @param signif The number of significant digits to include.
#' @param nonbreaking Should spaces be non-breaking?
#' @return A string in the form of _p_ = x or _p_ < x
#' @examples
#' p_show(0.0125623123, max_digits = 2)
#'
#' test = t.test(c(1,2,3), c(5,4,3))
#' p_show(test$p.value)
p_show <- function(x, max_digits = 3, signif = 2, nonbreaking = TRUE) {
  if (nonbreaking){
    space = "\ "
  } else {
    space = " "
  }
  factor = 10^max_digits

  # above threshold return full value
  if (x >= 10^(-max_digits)) {
    signif = signif(x, digits = signif)

    return(paste0("_p_", space, "=", space, round(signif, digits = max_digits)))
    # below threshold return p < x if applicable
  } else {
    ceiling = ceiling(x * factor)

    if (ceiling == x * factor) {
      return(paste0("_p_", space, "=", space, x))
    } else {
      return(paste0("_p_", space, "<", space, ceiling/factor))
    }
  }
}

#' Function to report high values as powers of 10
#'
#' This function converts a large raw number to a string of a multiplicative
#' expression with 10^x^.
#' @param x A large number.
#' @return A string in the form of a $\cdot$ 10^b^
#' @examples
#' power_val(exp(20))
#'
power_val <- function(x){
  require(stringr)

  sci = format(x, scientific = TRUE)

  sign_base = str_extract(sci, "^[\\-+]")
  if (is.na(sign_base)){
    sign_base = ""
  }
  base = str_extract(sci, "[1-9]\\.[1-9]")

  sign_power = str_extract(sci, "(?<=e)[\\-+]")
  if (sign_power == "+"){
    sign_power = ""
  }
  power = str_extract(sci, "[1-9]*$")

  return(paste0(sign_base, base, "$\\cdot$\\ 10^", sign_power, power, "^"))
}

#' Function to convert a data frame to a list named by a column.
#'
#' Convenient method to access cells within a data frame by an identifying
#' column value.
#' @param df A data frame.
#' @param by Name of identifying column
#' @return The value of the specified cell.
#' @examples
#' df = data.frame(ID = c("Item1", "Item2"), First_Value = c(2, 3), Second_Value = c("A", "B"))
#' results = to_named_list(df, by = "ID")
#'
#' results$Item1$First_Value
to_named_list <- function(df, by){
  require(tidyr)
  require(tibble)

  df %>%
    tidyr::nest(.by = all_of(by)) %>%
    tibble::deframe()
}


#' Function to return specific cells of an ANOVA table by term
#'
#' Conveniently report single values (like _p_ value or t statistic) of an ANOVA
#' table by term. **UNFINISHED!!!**
#' @param results An ANOVA table or lm object or data frame.
#' @param term The term of which to report the value.
#' @param which_value The type of value to report (_p_ value, t statistic, ...). Either "p" or the name of the statistic ("F", "t", ...). Currently only F supported
#' @param digits The number of digits to which to round the value.
#' @param print_name Should the name of the statistic ("F", "t", ...) be printed? **NOT IMPLEMENTED YET**
#' @return A string specifying degrees of freedom and test statistic.
report <- function(results, term, which_value, digits = 3, print_name = TRUE){
  require(broom)
  require(stringr)

  if (any(c("lm", "anova") %in% class(results))){
    table = broom::tidy(results)
  } else {
    table = results
  }

  term_row = which(table$term == term)
  resid_row = which(stringr::str_detect(table$term, "Resid"))

  if (which_value == "p"){
    p = table[[term_row, "p.value"]]
    return(p_show(p))
  } else if (which_value == "F"){
    value = table[[term_row, "statistic"]]
    round = round(value, digits = digits)

    if (print_name){
      print_value = which_value
    } else {
      print_value = ""
    }

    return(paste0(which_value,
                  "~",
                  table[[term_row, "df"]],
                  ",",
                  table[[resid_row, "df"]],
                  "~ = ",
                  round))
  } else {
    return(NA)
  }

}
