#' Turn an R list into an HTML list
#'
#' @param list An R list
#' @param class a class for the list
#'
#' @return an HTML list
#' @noRd
#'
#' @examples
#' list_to_li(c("a", "b"))
list_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    htmltools::tagList(
      lapply(
        list,
        shiny::tags$li
      )
    )
  } else {
    res <- lapply(
      list,
      shiny::tags$li
    )
    res <- lapply(
      res,
      function(x) {
        htmltools::tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    htmltools::tagList(res)
  }
}

list_to_p <- function(list, class = NULL) {
  if (is.null(class)) {
    htmltools::tagList(
      lapply(
        list,
        shiny::tags$p
      )
    )
  } else {
    res <- lapply(
      list,
      shiny::tags$p
    )
    res <- lapply(
      res,
      function(x) {
        htmltools::tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    htmltools::tagList(res)
  }
}

named_to_li <- function(list, class = NULL) {
  if (is.null(class)) {
    res <- mapply(
      function(x, y) {
        shiny::tags$li(
          shiny::HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    shiny::tagList(res)
  } else {
    res <- mapply(
      function(x, y) {
        shiny::tags$li(
          shiny::HTML(
            sprintf("<b>%s:</b> %s", y, x)
          )
        )
      },
      list,
      names(list),
      SIMPLIFY = FALSE
    )
    res <- lapply(
      res,
      function(x) {
        shiny::tagAppendAttributes(
          x,
          class = class
        )
      }
    )
    shiny::tagList(res)
  }
}

#' Remove a tag attribute
#'
#' @param tag the tag
#' @param ... the attributes to remove
#'
#' @return a new tag
#' @noRd
#'
#' @examples
#' a <- shiny::tags$p(src = "plop", "pouet")
#' tagRemoveAttributes(a, "src")
tagRemoveAttributes <- function(tag, ...) { # nolint
  attrs <- as.character(list(...))
  for (i in seq_along(attrs)) {
    tag$attribs[[attrs[i]]] <- NULL
  }
  tag
}

#' Hide or display a tag
#'
#' @param tag the tag
#'
#' @return a tag
#' @noRd
#'
#' @examples
#' ## Hide
#' a <- shiny::tags$p(src = "plop", "pouet")
#' undisplay(a)
#' b <- shiny::actionButton("go_filter", "go")
#' undisplay(b)
undisplay <- function(tag) {
  # if not already hidden
  if (
    !is.null(tag$attribs$style) &&
      !grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- paste(
      "display: none;",
      tag$attribs$style
    )
  } else {
    tag$attribs$style <- "display: none;"
  }
  tag
}

display <- function(tag) {
  if (
    !is.null(tag$attribs$style) &&
      grepl("display:\\s+none", tag$attribs$style)
  ) {
    tag$attribs$style <- gsub(
      "(\\s)*display:(\\s)*none(\\s)*(;)*(\\s)*",
      "",
      tag$attribs$style
    )
  }
  tag
}

#' Hide an elements by calling jquery hide on it
#'
#' @param id the id of the element to hide
#'
#' @noRd
#'
jq_hide <- function(id) {
  shiny::tags$script(sprintf("$('#%s').hide()", id))
}

#' Add a red star at the end of the text
#'
#' Adds a red star at the end of the text
#' (for example for indicating mandatory fields).
#'
#' @param text the HTLM text to put before the red star
#'
#' @return an html element
#' @noRd
#'
#' @examples
#' with_red_star("Enter your name here")
with_red_star <- function(text) {
  shiny::tags$span(
    shiny::HTML(
      paste0(
        text,
        shiny::tags$span(
          style = "color:red", "*"
        )
      )
    )
  )
}



#' Repeat tags$br
#'
#' @param times the number of br to return
#'
#' @return the number of br specified in times
#' @noRd
#'
#' @examples
#' rep_br(5)
rep_br <- function(times = 1) {
  shiny::HTML(rep("<br/>", times = times))
}

#' Create an url
#'
#' @param url the URL
#' @param text the text to display
#'
#' @return an a tag
#' @noRd
#'
#' @examples
#' enurl("https://www.thinkr.fr", "ThinkR")
enurl <- function(url, text) {
  shiny::tags$a(href = url, text)
}

#' Columns wrappers
#'
#' These are convenient wrappers around
#' `column(12, ...)`, `column(6, ...)`, `column(4, ...)`...
#'
#' @noRd
#'
col_12 <- function(...) {
  shiny::column(12, ...)
}

col_10 <- function(...) {
  shiny::column(10, ...)
}

col_8 <- function(...) {
  shiny::column(8, ...)
}

col_6 <- function(...) {
  shiny::column(6, ...)
}

col_4 <- function(...) {
  shiny::column(4, ...)
}

col_3 <- function(...) {
  shiny::column(3, ...)
}


col_2 <- function(...) {
  shiny::column(2, ...)
}

col_1 <- function(...) {
  shiny::column(1, ...)
}

# UNCOMMENT AND USE
#
#
# To use this part of the UI
#
#' #' Include Content From a File
#' #'
#' #' Load rendered RMarkdown from a file and turn into HTML.
#' #'
#' #' @rdname includeRMarkdown
#' #' @export
#' #'
#' #' @importFrom rmarkdown render
#' #' @importFrom markdown markdownToHTML
#' #' @importFrom htmltools HTML
#' includeRMarkdown <- function(path) {
#'
#'   md <- tempfile(fileext = '.md')
#'
#'   on.exit(unlink(md),add = TRUE)
#'
#'   rmarkdown::render(
#'     path,
#'     output_format = 'md_document',
#'     output_dir = tempdir(),
#'     output_file = md,quiet = TRUE
#'     )
#'
#'   html <- markdown::markdownToHTML(md, fragment.only = TRUE)
#'
#'   Encoding(html) <- "UTF-8"
#'
#'   return(HTML(html))
#' }
