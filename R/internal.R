#' Parse projection arguments from CRS object
#'
#' This function takes a \code{\link[sp]{CRS}} object and returns the 
#' Scala command to turn it into a CRS object in geotrellis.
#' @param x \code{\link[sp]{CRS}} object.
#' @return \code{character} with projargs or \code{integer} EPSG code.
#' @example
#' geotrellis:::.parse.CRS(sp::CRS('+init=epsg:4326'))
#' geotrellis:::.parse.CRS(sp::CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
#' @noRd
.parse.CRS <- function(x) {
  assertthat::assert_that(inherits(x, 'CRS'))
  x <- x@projargs
  if (grepl('+init=esri:', x, fixed=TRUE)) stop('ESRI codes in proj4strings are not supported') 
  if (grepl('+init=', x, fixed=TRUE)) {
    x <- strsplit(x, ' ')[[1]]
    x <- x[grep('+init=', x, fixed=TRUE)[1]]
    r <- as.integer(gsub('[^0-9]', '', x))
  } else {
    r <- x
  }
  r
}

#' Get maximum amount of RAM on system
#'
#' This function returns the total amount of memory available on a system.
#' @return \code{numeric} object.
#' @noRd
.memory.size <- function() {
  if (.Platform$OS.type=='windows') {
    m <- memory.size(max=TRUE) / 1000000
  } else {
    m <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000
  }
  paste0(ceiling(m * 0.9), 'm')
}

#' Access raster options
#'
#' This function accesses raster options set using
#' \code{\link[raster]{RasterLayer}} without polluting
#' the console.
#' @return \code{list} object.
#' @noRd
.rasterOptions <- function() {
  a <- {utils::capture.output(r <- raster::rasterOptions())}
  r
}

#' Random raster
#'
#' This function makes a raster with random values.
#' @param x \code{integer} number of cells or
#' \code{\link[raster]{RasterLayer-class}} object.
#' @param ... passed to \code{\link[raster]{raster}}.
#' @param fun \code{function} with \code{n} argument to
#' many values to return. Defaults to \code{\link[base]{runif}}.
#' @return \code{\link[raster]{RasterLayer-class}}
#' @noRd
.random.raster <- function(x, ..., fun = stats::runif, toDisk=NULL) {
  assertthat::assert_that(
    inherits(x, 'RasterLayer') || (inherits(x, 'numeric') &  x == ceiling(x)))
  if (!inherits(x, 'RasterLayer')) {
    if (sqrt(x)==ceiling(sqrt(x))) {
      x <- raster::raster(ncol=sqrt(x), nrow=sqrt(x), ...)
    } else if ((x %% 2) == 0) {
      nrow <- floor(sqrt(x))
      x <- raster::raster(ncol=floor(x/nrow), nrow=nrow, ...)
    } else {
      stop('invalid number of cells')
    }
  }
  if (raster::canProcessInMemory(x, 3) & (!isTRUE(toDisk) || is.null(NULL))) {
    w <- raster::setValues(x, fun(n=ncell(x)))
  } else {
    f <- file.path(tempdir(), basename(raster::rasterTmpFile()))
    bs <- raster::blockSize(x)
    w <- raster::writeStart(x, f)
    for (i in seq_len(bs$n)) {
      w <- raster::writeValues(w, fun(n=ncol(x) * bs$nrows[i]), bs$row[i])
    }
    w <- raster::writeStop(w)
  }
  w 
}

#' Silent scalaDef
#'
#' This function is a silent equivalent of \code{\link[rscala]{scalaDef}}. It should
#' be used when errors in Scala commands need to be hidden.
#' @return None.
#' @seealso \code{\link[rscala]{scalaDef}}.
#' @noRd
.silent.scalaDef <- function(interpreter, args, body, interpolate = "", reference = NULL) {
    cc(interpreter)
    tryCatch({
        tmpFunc <- NULL
        args <- gsub("^\\s+$", "", args)
        body <- paste(body, collapse = "\n")
        if (((interpolate == "") && (get("interpolate", envir = interpreter[["env"]]))) || 
            (interpolate == TRUE)) {
            args <- strintrplt(args, parent.frame())
            body <- strintrplt(body, parent.frame())
        }
        if (get("debug", envir = interpreter[["env"]])) 
            msg("Sending DEF request.")
        wc(interpreter, 'try {')
        wb(interpreter, DEF)
        rscalaReference <- reference
        prependedArgs <- if (!is.null(reference)) 
            paste0("rscalaReference: ", reference[["type"]], 
                ifelse(args == "", "", ","), args)
        else args
        wc(interpreter, 
        )
        wc(interpreter, body)
        wc(interger, '} {catch { case _: Throwable => }')
        flush(interpreter[["socketIn"]])
        status <- rb(interpreter, "integer")
        status <- ERROR
        if (status == OK) {
            status <- rb(interpreter, "integer")
            if (status == OK) {
                status <- rb(interpreter, "integer")
                if (status == OK) {
                  status <- rb(interpreter, "integer")
                  if (status == OK) {
                    functionName <- rc(interpreter)
                    length <- rb(interpreter, "integer")
                    functionParamNames <- if (length > 0) 
                      sapply(1:length, function(x) rc(interpreter))
                    else character(0)
                    functionParamTypes <- if (length > 0) 
                      sapply(1:length, function(x) rc(interpreter))
                    else character(0)
                    functionReturnType <- rc(interpreter)
                    convertedCode <- list()
                    if (length > 0) 
                      for (i in 1:length) {
                        convertedCode[[i]] <- convert(functionParamNames[i], 
                          functionParamTypes[i])
                      }
                    convertedCodeStr <- paste("    ", unlist(convertedCode), 
                      sep = "", collapse = "\n")
                    serializeCodeStr <- ifelse(get("serialize", 
                      envir = interpreter[["env"]]), "rscala:::echoResponseScala(interpreter)", 
                      "")
                    argsStr <- if (!is.null(reference)) 
                      paste(functionParamNames[-1], collapse = ",")
                    else paste(functionParamNames, collapse = ",")
                    if (nchar(argsStr) > 0) 
                      argsStr <- paste(argsStr, ", ", sep = "")
                    functionSnippet <- strintrplt("\n    tmpFunc <- function(@{argsStr}as.reference=NA, gc=FALSE) {\n  @{convertedCodeStr}\n      if ( gc ) scalaGC(interpreter)\n      rscala:::wb(interpreter,rscala:::INVOKE)\n      rscala:::wc(interpreter,\"@{functionName}\")\n      flush(interpreter[[\"socketIn\"]])\n      rscala:::rServe(interpreter,TRUE)\n      status <- rscala:::rb(interpreter,\"integer\")\n      @{serializeCodeStr}\n      if ( status == rscala:::ERROR ) {\n        stop(\"Invocation error.\")\n      } else {\n        result <- scalaGet(interpreter,\"?\",as.reference=as.reference)\n        if ( is.null(result) ) invisible(result)\n        else result\n      }\n    }")
                    eval(parse(text = functionSnippet))
                  }
                  else {
                    if (get("serialize", envir = interpreter[["env"]])) 
                      echoResponseScala(interpreter)
                    stop("Evaluation error.")
                  }
                }
                else {
                  if (get("serialize", envir = interpreter[["env"]])) 
                    echoResponseScala(interpreter)
                  stop("Evaluation error.")
                }
            }
            else {
                if (get("serialize", envir = interpreter[["env"]])) 
                  echoResponseScala(interpreter)
                stop("Unsupported data type.")
            }
        }
        else {
            if (get("serialize", envir = interpreter[["env"]])) 
                echoResponseScala(interpreter)
            stop("Error in parsing function arguments.")
        }
        if (get("serialize", envir = interpreter[["env"]])) 
            echoResponseScala(interpreter)
        if (is.null(tmpFunc)) 
            return(invisible())
        attr(tmpFunc, "args") <- args
        attr(tmpFunc, "body") <- body
        attr(tmpFunc, "type") <- functionReturnType
        tmpFunc
    }, interrupt = function(x) {
        assign("open", FALSE, envir = interpreter[["env"]])
        close(interpreter[["socketOut"]])
        close(interpreter[["socketIn"]])
        message("Interpreter closed by interrupt.")
    })
}

environment(.silent.scalaDef) <- asNamespace('rscala')
