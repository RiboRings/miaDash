#' Utilities
#' 
#' Helper functions and constants to support the app functionality.
#'
#' @name utils
#' @keywords internal

#' @rdname utils
.import_datasets <- function(selection) {
  
    mia_datasets <- data(package = "mia")
    mia_datasets <- mia_datasets$results[selection, "Item"]
    data(list = mia_datasets, package = "mia")
    
    return(mia_datasets)
}

#' @rdname utils
.update_tse <- function(tse, fun, fun_args) {

    tse <- tryCatch({withCallingHandlers({
        
        do.call(fun, fun_args)
      
        # nocov start
        }, message = function(m) {
        
            showNotification(conditionMessage(m))
            invokeRestart("muffleMessage")
        
        })}, error = function(e) {
      
            .print_message(e, title = "Unexpected error:")
            return(tse)
          
        })
        # nocov end
  
    return(tse)
}

#' @rdname utils
.print_message <- function(..., title = "Invalid input:") {

    # nocov start
    showModal(modalDialog(
        title = title, ...,
        easyClose = TRUE, footer = NULL
    ))
    # nocov end
}

#' @rdname utils
.set_optarg <- function(item, loader = NULL, alternative = NULL, ...){
  
    if( !(is.null(item) || is.null(loader)) ){
        out <- loader(item, ...)
    } else {
        out <- alternative
    }
  
    return(out)
}

#' @rdname utils
#' @importFrom SummarizedExperiment colData
.check_formula <- function(form, se){
  
    form <- gsub("data ~\\s*", "", form)
    vars <- unlist(strsplit(form, "\\s*[\\+|\\*]\\s*"))
  
    cond <- all(vars %in% names(colData(se)))
    return(cond)
}

#' @rdname utils
#' @importFrom S4Vectors isEmpty
#' @importFrom methods is
.check_panel <- function(se, panel_list, panel_class, panel_fun, wtext) {
  
    no_keep <- unlist(lapply(panel_list, function(x) is(x, panel_class)))
  
    if( any(no_keep) && (is.null(panel_fun(se)) || isEmpty(panel_fun(se))) ){
        panel_list <- panel_list[!no_keep]
        warning("no valid ", as.character(substitute(panel_fun)),
            " fields for ", panel_class, call. = FALSE)
    }
  
    return(panel_list)
}

#' @rdname utils
default_panels <- c("RowDataTable", "ColumnDataTable", "RowTreePlot",
    "AbundancePlot", "AbundanceDensityPlot", "ReducedDimensionPlot",
    "ComplexHeatmapPlot")

#' @rdname utils
other_panels <- c("LoadingPlot", "ColumnTreePlot", "RDAPlot", "ColumnDataPlot",
    "RowDataPlot")