#' @name dust_inline.lm
#' @title Inline Summary for \code{lm} coefficients
#' 
#' @description Generates an inline summary of a linear model coefficient.
#' 
#' @param object An object that may be tidied.
#' @param term A character vector naming the term (or combination of terms)
#'   in the model for which the summary is desired.  When \code{term} has 
#'   length 1, a main effect is returned.  When the length is greater than 
#'   1, the interaction term between those effects is returned.
#' @param level A vector or list, optionally named, that gives the character 
#'   values of the levels for which coefficients are desired when 
#'   \code{term} is a factor.
#' @param conf_int \code{logical(1)}. Should the confidence interval be 
#'   included in the summary?
#' @param se \code{logical(1)}. Should the standard error of the estimate be
#'   included in the summary?
#' @param test_stat \code{logical(1)}. Should the test statistic for the 
#'   estimate be included in the summary?
#' @param p_value \code{logical(1)}. Should the p-value for the estimate 
#'   be included in the summary?
#' @param label \code{character(1)}. The character label for the term. For 
#'   example, \code{"Estimate"}, \code{"OR"}, or \code{"HR"}. Defaults to
#'   \code{"$\\\\beta$"}, for something generic.
#' @param fun A function to apply to the estimate and confidence intervals.
#'   Useful if the response in \code{object} has been transformed. Then 
#'   \code{fun} should be the inverse function of the response.
#' @param fun_arg A list of additional arguments for \code{fun}
#' @param alpha Significance level (or 1 - confidence) for the confidence 
#'   interval
#' @param ... Additional arguments to pass to other methods. Not used.
#' @param style A \code{character(1)} specifying the output style.  When not
#'   \code{"none"}, this overrules any arguments defined in the call.
#' 
#' @section: Functional Requirements
#' In addition to the requirements listed here, the requirements listed under
#' \code{\link{dust_inline}} must be satisfied.
#' 
#' \enumerate{
#'   \item Accepts a character vector naming the \code{term} to be summarised. 
#'     A length one vector returns the main effect. 
#'     A length two vector returns the interaction between two terms, etc.
#'   \item Return an error if no term exists that satisfies the linear 
#'     combination.
#'   \item Accepts a vector or list of characters, optionally named, 
#'     specifying the level for any factors named in term. 
#'     If unnamed, the levels are assumed to follow the same order of 
#'     factors in term.
#'   \item Returns an error if any levels in level cannot be found 
#'     in its corresponding term.
#'   \item Accept a \code{logical(1)} indicating if the confidence interval 
#'     is to be included in the summary
#'   \item Accept a \code{logical(1)} indicating if the SE is to be included in 
#'     the summary
#'   \item Accept a \code{logical(1)} indicating if the test statistic is to be 
#'     included in the summary
#'   \item Accept a \code{logical(1)} indicating if the p-value is to be 
#'     included in the summary
#'   \item Accept a \code{character(1)} designating the text label for the 
#'     coefficient (beta, OR, etc)
#'   \item Accept a function by to apply to the coefficient and CI
#'   \item Accept additional arguments to the transformation function
#' }
#'   
#' @seealso \code{link{dust_inline}}
#' @export

dust_inline.lm <- function(object, term, level = NULL, 
                           se = FALSE, test_stat = FALSE, 
                           conf_int = TRUE, p_value = TRUE,
                           label = "$\\beta$", 
                           fun = identity, fun_arg = list(),
                           alpha = 0.05, 
                           ..., style = "none")
{
# Argument validations ----------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  tidy_object <- attempt_to_tidy(object = object,
                                 coll = coll,
                                 conf_int = conf_int,
                                 alpha = alpha)
  
  checkmate::assert_character(x = term,
                              add = coll)
  
  if (!is.null(level))
  {
    if (is.list(level))
    {
      level <- unlist(level)
    }
    checkmate::assert_character(x = level,
                                add = coll)
  }
  
  checkmate::assert_logical(x = conf_int,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = se,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = test_stat,
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = p_value,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = label,
                              len = 1,
                              add = coll)
  
  checkmate::assert_class(x = fun,
                          classes = "function",
                          add = coll)
  
  checkmate::assert_class(x = fun_arg,
                          classes = "list",
                          add = coll)
  
  checkmate::assert_numeric(x = alpha,
                            len = 1,
                            add = coll)
  
  checkmate::assert_character(x = style,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)

# Functional Code ---------------------------------------------------
  
  level <- force_named_level(object, level, coll)
  checkmate::reportAssertions(coll)
  
  term[term %in% names(level)] <- level[term[term %in% names(level)]]
  term <- paste0(term, collapse = ":")
  
  if (!term %in% tidy_object[["term"]])
  {
    coll$push(sprintf("`%s` is not a valid term in the model",
                      term))
    checkmate::reportAssertions(coll)
  }
  
  term_data <- tidy_object[tidy_object[["term"]] == term, , drop = FALSE]
  term_data[c("estimate", "conf.low", "conf.high")] <- 
    lapply(X = term_data[c("estimate", "conf.low", "conf.high")],
           FUN = function(x) do.call(fun, c(x, fun_arg)))
  
  term_data[-match(c("term", "p.value"), names(term_data))] <- 
    lapply(X = term_data[-match(c("term", "p.value"), names(term_data))],
           FUN = round,
           digits = 2)
  term_data[["p.value"]] <- pvalString(term_data[["p.value"]])
  
  term_data
  
  if (style == "none")
  {
    if (!grepl("([<]|[>])", term_data[["p.value"]]))
    {
      term_data[["p.value"]] <- sprintf("= %s", term_data[["p.value"]])
    }
    
    sprintf("(%s = %s%s%s%s%s)",
            label,
            term_data[["estimate"]],
            if (se) paste0("; SE = ", term_data[["std.error"]]) else "",
            if (test_stat) paste0("; T = ", term_data[["statistic"]]) else "",
            if (conf_int) paste0("; ", 
                                 scales::percent(1 - alpha), 
                                 " CI: ", 
                                 term_data[["conf.low"]], 
                                 " - ", 
                                 term_data[["conf.high"]]) else "",
            if (p_value) paste0("; P ", term_data[["p.value"]]) else "")
  }
}

# Unexported Utilities ----------------------------------------------

#' @name dust_inline_unexported
#' @title Unexported Utilities for \code{dust_inline}
#' 
#' @description Utilites for the \code{dust_inline} method. These are 
#'   unexported functions that do nothing but simplify the code, and 
#'   make reasoning the various parts a little simpler.
#'   
#' @param object An object to be tidied
#' @param coll An \code{AssertCollection} object.
#' @param conf_int The value of \code{conf_int} from \code{dust_inline.lm}
#' @param alpha The value of \code{alpha} from \code{dust_inline.lm}
#'   
#' @details \code{attempt_to_tidy} returns a tidied object, if it can, or
#'   the error generated upon failure.

attempt_to_tidy <- function(object, coll, conf_int, alpha)
{
  tidied_object <- 
    tryCatch(broom::tidy(object, 
                         conf.int = conf_int, 
                         conf.level = 1 - alpha),
             error = function(cond) cond)
  
  if ("error" %in% class(tidied_object))
  {
    coll$push("No `tidy` method available for `object`")
  }
  
  tidied_object
}

#' @rdname dust_inline_unexported
#' @param level A character vector of levels to match to terms.

force_named_level <- function(object, level, coll)
{
  if (!is.null(level))
  {
    tll <- 
      tidy_levels_labels(object, 
                         descriptors = c("term", "term_plain", "level")) %>%
      dplyr::mutate(is_main = !grepl(pattern = "[:]",
                                     x = term),
                    is_factor = level != "" & is_main)
    
    # Named level elements must be in the terms of the model
    if (any(!names(level)[names(level) != ""] %in% tll[["term_plain"]][tll[["is_main"]]]))
    {
      coll$push("Named element in `level` does not match terms in model")
      return(level)
    }
    else
    {
      named_factor <- tll[["term_plain"]][match(x = names(level), 
                                                table = tll[["term_plain"]])]
      
      remaining_factor <- tll[["term_plain"]][tll[["is_factor"]]]
      remaining_factor <- unique(remaining_factor)
      remaining_factor <- remaining_factor[!remaining_factor %in% named_factor]
      
      w_unnamed <- which(names(level) == "")
      
      names(level)[w_unnamed] <- remaining_factor

      paste0(names(level), level) %>%
        stats::setNames(names(level))
    }
  }
  else
  {
    level
  }
}


