#' Outputs
#'
#' \code{.render_overview} and \code{.render_download} create the output of the
#'   UI, which consists of the overview of the TreeSE object and the download
#'   object, respectively.
#'
#' @param output The Shiny output object from the server function.
#'
#' @return Adds a rendered item to \code{output}.
#'   A \code{NULL} value is invisibly returned.
#'
#' @name render_output
#' @keywords internal

#' @rdname render_output
.render_overview <- function(output, rObjects) {
  
    # nocov start
    output$object <- renderPrint({
        rObjects$tse
    })
    # nocov end

    invisible(NULL)
}

#' @rdname render_output
.render_download <- function(output, rObjects) {
  
    # nocov start
    output$download <- downloadHandler(
        filename = function() paste0("se-", Sys.Date(), ".rds"),
        content = function(file) saveRDS(rObjects$tse, file)
    )
    # nocov end
  
    invisible(NULL)
}