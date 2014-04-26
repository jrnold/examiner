#' examiner.
#'
#' @name examiner
#' @docType package
#' @import yaml
#' @import stringr
#' @import whisker
#' @import plyr
NULL

beginenv <- function(x) str_c("\\begin{", x, "}")
endenv <- function(x) str_c("\\end{", x, "}")
ltxnewenv <- function(x, begin = "", end = "") {
    str_c("\\newenvironment{", x, "}{", begin, "}{", end, "}")
}

#' Examiner Options
#'
#' This environment stores the options used in the \pkg{examiner} package.
#' @export
examiner_opts <- new.env(hash = TRUE)
examiner_opts$tpl_problem <-
    str_c("\\begin{problem}",
          "\\begin{problemtext}",
          "{{{text}}}",
          "\\end{problemtext}",
          "\\begin{answers}",
          "{{{answers}}}",
          "\\end{answers}",
          "{{#show_solutions}}",
          "\\begin{solution}",
          "{{{solution}}}",
          "\\end{solution}",
          "{{/show_solutions}}",
          "\\end{problem}", sep = "\n")

examiner_opts$tpl_answers <-
    str_c("{{#answers}}",
          "\\item {{#show_solutions}}{{#correct}}**{{/correct}}{{/show_solutions}} {{{text}}}",
          "{{/answers}}",
          sep = "\n")


examiner_opts$tpl_problemset <-
    str_c(
        "\\begin{problemset}",
        "\\begin{problemsetpretext}",
        "{{{problemsetpretext}}}",
        "\\end{problemsetpretext}",
        "\\begin{problems}",
        "{{#problems}}",
        "{{{.}}}",
        "{{/problems}}",
        "\\end{problems}",
        "\\begin{problemsetpretext}",
        "{{{problemsetpretext}}}",
        "\\end{problemsetpretext}",
        "\\end{problemset}",
        sep = "\n")

examiner_opts$latex_header <-
    c("\\usepackage{amsthm,amsmath,enumitem}",
      "\\theoremstyle{definition}\\newtheorem{problem}{Problem}",
      ltxnewenv("problemtext", "\\par", ""),
      ltxnewenv("problemset", "\\par", ""),
      ltxnewenv("problems", "\\par", ""),
      ltxnewenv("problemsetpretext", "\\par", ""),
      ltxnewenv("problemsetposttext", "\\par", ""),
      "\\newlist{answers}{enumerate}{1}",
      "\\setlist[answers]{label=(\\alph*)}")

#' Create a LaTeX header for \code{examiner} output
#'
#' This renders the contents of \code{opts_examiner$get('header')}.
#'
#' @return A character vector with the header
#' @export
examiner_latex_header <- function() {
    str_c(examiner_opts[["latex_header"]], collapse = "\n")
}


#' Create an answerlist object
#' @param x A data frame with colums: text, correct, and fixed.
#' @return An object of class \code{answerlist}.
#' @export
answerlist <- function(x) {
    x <- as.data.frame(x)
    class(x) <- c("answerlist", class(x))
    x
}

#' @export
format.answerlist <- function(answers, show_solutions = FALSE, ...) {
    answers2 <- unname(rowSplit(answers))
    whisker.render(examiner_opts[["tpl_answers"]],
                   data = c(list(answers = answers2,
                       show_solutions = show_solutions),
                       list(...)))
}

#' Create a problem object
#'
#' @param text \code{character} vector with the prompt for the problem.
#' @param answers A \code{list} of \code{problem} or \code{problemset} objects.
#' @param correct Indices of answers which are correct.
#' @param first Number of observations at the beginning of \code{answers} which will not be shuffled.
#' @param last Number of observations at the end of \code{answers} which will not be shuffled.
#' @param randomizable Can the answers be shuffled?
#' @return An object of class \code{problem}.
#' @keywords internal 
problem <- function(text = "",
                    answers = character(),
                    solution = "",
                    correct = 1L, first = 0L, last = 0L,
                    randomizable = TRUE) {
    n <- length(answers)
    first <- min(c(first, n - last + 1))
    last <- min(c(last, n))

    firsti <- seq_len(first)
    lasti <- pmin(rev(length(answers) - seq_len(last) + 1),
                  length(answers))
    answers <-
        answerlist(data.frame(text = answers,
                              correct = seq_along(answers) == correct,
                              fixed = seq_along(answers) %in% c(firsti, lasti),
                              stringsAsFactors = FALSE))
    .Data <-
        list(text = text,
             answers = answers,
             correct = correct,
             first = first,
             last = last,
             solution = as.character(solution),
             randomizable = as.logical(randomizable)[1])
    class(.Data) <- c("problem", "list")
    .Data
}

shuffle_answers <- function(x) {
    indices <- seq_len(nrow(x))[!x[["fixed"]]]
    iorder <- sample(indices, length(indices))
    x[indices, ] <- x[iorder, , drop = FALSE]
    attr(x, "order") <- iorder
    x
}

#' @export
format.problem <- function(x, randomize = FALSE, show_solutions = FALSE, ...) {
    x <- as.list(x)
    if (x[["randomizable"]] && as.logical(randomize)) {
        x[["answers"]] <- shuffle_answers(x[["answers"]])
    }
    x[["answers"]] <- format(x[["answers"]], show_solutions = show_solutions, ...)
    x[["show_solutions"]] <- show_solutions
    x <- c(x, list(...))
    whisker.render(examiner_opts[["tpl_problem"]], data = x)
}

#' Create a problemset object
#'
#' @param problems list of problems
#' @param pretext \code{character} vector with text to go before the questions.
#' @param posttext \code{character} vector with text to go after the questions.
#' @return A \code{problemset} object
#' @export
problemset <- function(problems, pretext = "", posttext = "") {
    if (! inherits(problems, "list") ||
        !all(sapply(problems, inherits, what = "problem"))) {
        stop("problems must be a list of problem objects")
    }
    .Data <- list(problems = problems, pretext = pretext,
                  posttext = posttext)
    class(.Data) <- c("problemset", "list")
    .Data
}

#' @export
format.problemset <- function(x, randomize = FALSE, show_solutions = FALSE, ...) {
    x <- as.list(x)
    problems <- laply(x[["problems"]], format,
                      show_solutions = show_solutions,
                      randomize = randomize, ...)
    x[["problems"]] <- format(problems, show_solutions = show_solutions, ...)
    x <- c(x, list(...))
    whisker.render(examiner_opts[["tpl_problemset"]], data = x)
}

#' Create problemset from a yaml file
#'
#' @param input Name of the input file.
#' @return A \code{problemset} object.
#' @export
problemset_from_yaml <- function(input) {
    .Data <- yaml.load_file(input)
    problems <- llply(.Data[["problems"]], function(x) do.call(problem, x))
    .Data[["problems"]] <- problems
    do.call(problemset, .Data)
}
