
#' Creates object of class surveybraidppt.
#' 
#' A surveybraidppt object consists of a surveyor and braidppt object.
#' 
#' @param surveyor surveyor object
#' @param braid braid object  
#' @return A list object of class \code{surveyorbraid}
#' @export
as.surveybraidppt <- function(surveyor, braid){
  if (!is.surveyor(surveyor)) stop("surveybraidppt: surveyor must be a surveyor object")
  if (!is.braidppt(braid))       stop("surveybraidppt: braid must be a braid object")
  structure(
      list(
          surveyor = surveyor,
          braid      = braid
      ), 
      class = "surveybraidppt"
  )
}

#' Tests that object is of class surveyorbraid.
#' 
#' @param x Object to be tested
#' @export
is.surveybraidppt <- function(x){
  if(!inherits(x, "surveybraidppt")) return(FALSE)
  if(is.surveyor(x$surveyor) & is.braidppt(x$braid)) TRUE else FALSE
}

#------------------------------------------------------------------------------

#' Codes and plots a survey question.
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param x surveybraidppt object
#' @param qid Question id
#' @param statsFunction  A surveyor stats function
#' @param plotFunction A surveyor plot function
#' @param codeFunction A surveyor stats function
#' @param onlyBreaks Numeric vector that limits crossbreak processing
#' @param outputType "latex", "ppt" or "device": Specifies destination of ouput
#' @param plotSize Size in inches of plot output, e.g. c(4,3)
#' @param plotMultiplierLimits Numeric vector of length two, indicating lower and upper limit of vertical plot resizing
#' @param addPlotTitle If TRUE, adds question text to plot title
#' @param ... Other parameters passed to \code{\link[surveyor]{surveyPlot}}
#' @method surveyPlot surveybraidppt
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyPlot.surveybraidppt <- function(
    x,
    qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    onlyBreaks=seq_along(x$surveyor$crossbreak),
    outputType = x$braid$outputType,
    plotSize = x$braid$defaultPlotSize,
    #plotMultiplierLimits = if(outputType=="ppt") c(0.8, 1.2) else c(0.8, 2.5),
    plotMultiplierLimits = c(0.8, 1.2),
    addPlotTitle = TRUE,
    ...
  ){
  
  surveyor <- x$surveyor
  braid    <- x$braid
  
  #browser()
  if(!require(braidppt)) stop("Unable to load package braidppt")
  
  if(!exists(qid, surveyor$sdata) & is.null(which.q(surveyor$sdata, qid))){
    message(paste(qid,": Question not found.  Processing aborted"))
    return(NULL)
  }
  message(qid)
  
  lh <- surveyPlot(surveyor, qid, statsFunction, plotFunction, codeFunction, 
      onlyBreaks=onlyBreaks, addPlotTitle=addPlotTitle, ...)
  
  
  lapply(seq_along(lh), function(i){
      catString <- surveybraidpptPrintQuestion(
          onlyBreaks[i],
          surveyor,
          braid,
          qid,
          lh[[i]],
          plotSize,
          outputType=outputType,
          plotMultiplierLimits=plotMultiplierLimits
      )
      if(!identical(catString, "")) braidWrite(braid, catString)
    }
  )
    	
  return(invisible(NULL))
}

#' Prints surveyor question. 
#' 
#' @inheritParams surveyPlot.surveybraidppt
#' @keywords internal
surveybraidpptPrintQuestion <- function(i, surveyor, braid, qid, h, plotSize, outputType, 
    plotMultiplierLimits=c(1, 1)){
  
  stopifnot(require(braidppt))
  if(inherits(h$plot, "text")) return(h$plot)
  
  # Print plot
  filename <- paste(qid, "_", names(surveyor$crossbreak)[i], ".", braid$graphicFormat, sep="")
  message(paste(" --", filename))
  
  # Adjust vertical size of plot depending on number of questions
  # Make the assumption that 7 questions can fit on a plot
  # Limit vertical size to [1, 3]*size of default
  plotMin <- plotMultiplierLimits[1]
  plotMax <- plotMultiplierLimits[2]
  height_multiplier <- ifelse(
      is.numeric(h$nquestion), 
      min(plotMax, max(plotMin, h$nquestion / 7)),
      plotMin
  )

  plot_title <- qTextCommon(surveyor$sdata, qid)
  surveyor$plot_title <- plot_title
  
  braidpptNewSlide(braid, title=qid, text=plot_title)
  braidpptPlot(braid, h$plot, filename=filename,
      width=plotSize[1], height=(plotSize[2] * height_multiplier), Qid=qid)
  return(NULL)
}



