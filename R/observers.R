#' Observers
#' 
#' \code{.create_observers} and \code{.create_launch_observers} define the
#' server to import and build TreeSE objects and track the state of the Build
#' and Launch buttons.
#'
#' @param input The Shiny input object from the server function.
#' @param rObjects A reactive list of values generated in the landing page.
#'
#' @return Observers are created in the server function in which this is called.
#' A \code{NULL} value is invisibly returned.
#'
#' @name create_observers
#' @keywords internal

#' @rdname create_observers
#' @importFrom utils read.csv
#' @importFrom ape read.tree
#' @importFrom S4Vectors DataFrame
#' @importFrom biomformat read_biom
#' @importFrom mia convertFromBIOM importMetaPhlAn
#' @importFrom TreeSummarizedExperiment TreeSummarizedExperiment
.create_import_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$import, {
      
        if( input$format == "dataset" ){
      
            rObjects$tse <- isolate(get(input$data))
      
        }else if( input$format == "rds" ){
      
            isolate({
                req(input$file)
                rObjects$tse <- readRDS(input$file$datapath)
            })
      
        }else if( input$format == "raw" ){
      
            isolate({
                req(input$assay)
        
                assay_list <- lapply(input$assay$datapath,
                    function(x) as.matrix(read.csv(x, row.names = 1)))
                
                names(assay_list) <- gsub(".csv", "", input$assay$name)
                
                coldata <- .set_optarg(input$coldata$datapath,
                    alternative = DataFrame(row.names = colnames(assay_list[[1]])),
                    loader = read.csv, row.names = 1)

                rowdata <- .set_optarg(input$rowdata$datapath,
                    loader = read.csv, row.names = 1)
               
                row.tree <- .set_optarg(input$row.tree$datapath,
                    loader = read.tree)
                
                col.tree <- .set_optarg(input$col.tree$datapath,
                    loader = read.tree)
                
                fun_args <- list(assays = assay_list, colData = coldata,
                    rowData = rowdata, rowTree = row.tree, colTree = col.tree)
        
                rObjects$tse <- .update_tse(
                     rObjects$tse, TreeSummarizedExperiment, fun_args
                )
            
            })
      
        }else if( input$format == "foreign" ){
          
            isolate({
                req(input$main.file)
      
                if( input$ftype == "biom" ){

                    biom_object <- read_biom(input$main.file$datapath)
                    
                    fun_args <- list(x = biom_object,
                        removeTaxaPrefixes = input$rm.tax.pref,
                        rankFromPrefix = input$rank.from.pref)

                    rObjects$tse <- .update_tse(
                        rObjects$tse, convertFromBIOM, fun_args
                    )
              
                } else if( input$ftype == "MetaPhlAn" ){
                  
                    coldata <- .set_optarg(input$col.data$datapath,
                        alternative = input$col.data$datapath)
                    
                    treefile <- .set_optarg(input$tree.file$datapath)
                  
                    fun_args <- list(file = input$main.file$datapath,
                        col.data = coldata, tree.file = treefile)
              
                    rObjects$tse <- .update_tse(
                         rObjects$tse, importMetaPhlAn, fun_args
                    ) 
                 
                }
        
            })
            
        }
      
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom SummarizedExperiment assay
#' @importFrom mia subsetByPrevalent subsetByRare agglomerateByRank
#'   transformAssay
.create_manipulate_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$apply, {
      
        if( input$manipulate == "subset" ){
          
            isolate({
                req(input$subassay)
              
                if( input$subkeep == "prevalent" ){
                    subset_fun <- subsetByPrevalent
                } else if( input$subkeep == "rare" ){
                    subset_fun <- subsetByRare
                }
            
                fun_args <- list(x = rObjects$tse, assay.type = input$subassay,
                    prevalence = input$prevalence, detection = input$detection)
                
                rObjects$tse <- .update_tse(rObjects$tse, subset_fun, fun_args)
              
            })
          
        }
      
        else if( input$manipulate == "agglomerate" ){
          
            isolate({
                
                fun_args <- list(x = rObjects$tse, rank = input$taxrank)
                rObjects$tse <- .update_tse(
                     rObjects$tse, agglomerateByRank, fun_args
                )
              
            })
          
        } else if( input$manipulate == "transform" ){

            if( input$trans.method == "clr" && !input$pseudocount &&
                any(assay(rObjects$tse, input$assay.type) <= 0)){
              
                .print_message(
                    "'clr' cannot be used with non-positive data:",
                    "please turn on pseudocount."
                )
              
                return()
            }
          
            isolate({
                req(input$assay.type)
              
                if( input$assay.name != "" ){
                    name <- input$assay.name
                } else {
                    name <- input$trans.method
                }
              
                fun_args <- list(x = rObjects$tse, name = name,
                    method = input$trans.method, assay.type = input$assay.type,
                    MARGIN = input$margin, pseudocount = input$pseudocount)
                
                rObjects$tse <- .update_tse(
                     rObjects$tse, transformAssay, fun_args
                )
                
            })
          
        }
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom stats as.formula
#' @importFrom mia addAlpha runNMDS runRDA getDissimilarity
#' @importFrom TreeSummarizedExperiment rowTree
#' @importFrom scater runMDS runPCA
#' @importFrom vegan vegdist
.create_estimate_observers <- function(input, rObjects) {
  
    # nocov start
    observeEvent(input$compute, {
        
        if( input$estimate == "alpha" ){
          
            if( is.null(input$alpha.index) ){
                .print_message("Please select one or more metrics.")
                return()
            }
        
            isolate({
                req(input$estimate.assay)
              
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                } else {
                    name <- input$alpha.index
                }
          
                fun_args <- list(x = rObjects$tse, name = name,
                    assay.type = input$estimate.assay, index = input$alpha.index)
                
                rObjects$tse <- .update_tse(rObjects$tse, addAlpha, fun_args)
          
            })
        
        } else if( input$estimate == "beta" ){
          
            if( input$ncomponents > nrow(rObjects$tse) - 1 ){
              
                .print_message(
                    "Please use a number of components smaller than the number",
                    "of features in the assay."
                )
              
                return()
            }
          
            isolate({
                req(input$estimate.assay)
              
                if( input$estimate.name != "" ){
                    name <- input$estimate.name
                } else {
                    name <- input$bmethod
                }
              
                beta_args <- list(x = rObjects$tse, assay.type = input$assay.type,
                    ncomponents = input$ncomponents, name = name)
              
                if( input$beta.index == "unifrac" ){
                  
                    if( is.null(rowTree(rObjects$tse)) ){
                        .print_message("Unifrac cannot be computed without a rowTree.")
                        return()
                    }
                  
                    beta_args <- c(beta_args, FUN = getDissimilarity,
                        tree = list(rowTree(rObjects$tse)),
                        ntop = nrow(rObjects$tse), method = input$beta.index)
                    
                } else if( input$bmethod %in% c("MDS", "NMDS") ){
                  
                    beta_args <- c(beta_args, FUN = vegdist,
                        method = input$beta.index)
                    
                } else if( input$bmethod == "RDA" ){
                  
                    if( input$rda.formula == "" ){
                        .print_message("Please enter a formula.")
                        return()
                    }
                  
                    if( !.check_formula(input$rda.formula, rObjects$tse) ){
                        .print_message("Please make sure all elements in the",
                           "formula match variables of the column data.")
                        return()
                    }
                  
                    beta_args <- c(beta_args,
                        formula = as.formula(input$rda.formula))
                  
                }
                
                beta_fun <- eval(parse(text = paste0("run", input$bmethod)))
                
                rObjects$tse <- .update_tse(rObjects$tse, beta_fun, beta_args)
            })
        
        }
        
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}

#' @rdname create_observers
#' @importFrom SummarizedExperiment assayNames
#' @importFrom mia taxonomyRanks
#' @importFrom rintrojs introjs
.update_observers <- function(input, session, rObjects){
  
    # nocov start
    observe({
      
      if( isS4(rObjects$tse) ){
        
          updateSelectInput(session, inputId = "subassay",
              choices = assayNames(rObjects$tse))
        
          updateSelectInput(session, inputId = "taxrank",
              choices = taxonomyRanks(rObjects$tse))
          
          updateSelectInput(session, inputId = "assay.type",
              choices = assayNames(rObjects$tse))
          
          updateSelectInput(session, inputId = "estimate.assay",
              choices = assayNames(rObjects$tse))
          
          updateSelectInput(session, inputId = "estimate.assay",
              choices = assayNames(rObjects$tse))
          
          updateNumericInput(session, inputId = "ncomponents",
              max = nrow(rObjects$tse) - 1)
        
      }
    
    })
    
    observeEvent(input$iSEE_INTERNAL_tour_steps, {
      
        introjs(session, options = list(steps = .landing_page_tour))
      
    }, ignoreInit = TRUE)
    # nocov end
    
    invisible(NULL)
}

#' @rdname create_observers
.create_launch_observers <- function(FUN, input, session, rObjects) {
  
    # nocov start
    observeEvent(input$launch, {
    
        .launch_isee(FUN, input$panels, session, rObjects)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    # nocov end
  
    invisible(NULL)
}