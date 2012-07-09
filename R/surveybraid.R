
#' Creates object of class surveybraid.
#' 
#' A surveybraid object consists of a surveyor and braid object.
#' 
#' @param surveyor surveyor object
#' @param braid braid object  
#' @return A list object of class \code{surveyorbraid}
#' @export
as.surveybraid <- function(surveyor, braid){
  if (!is.surveyor(surveyor)) stop("surveybraid: surveyor must be a surveyor object")
  if (!is.braid(braid))       stop("surveybraid: braid must be a braid object")
  structure(
      list(
          surveyor = surveyor,
          braid      = braid
      ), 
      class = "surveybraid"
  )
}

#' Tests that object is of class surveyorbraid.
#' 
#' @param x Object to be tested
#' @export
is.surveybraid <- function(x){
  if(!inherits(x, "surveybraid")) return(FALSE)
  if(is.surveyor(x$surveyor) & is.braid(x$braid)) TRUE else FALSE
}

#------------------------------------------------------------------------------

#' Codes and plots a survey question.
#' 
#' This is the top level function that determines how a question is processed,
#' coded, printed and plotted.
#' 
#' @param x surveybraid object
#' @param qid Question id
#' @param statsFunction  A surveyor stats function
#' @param plotFunction A surveyor plot function
#' @param codeFunction A surveyor stats function
#' @param outputType "latex", "ppt" or "device": Specifies destination of ouput
#' @param plotSize Size in inches of plot output, e.g. c(4,3)
#' @param onlyBreaks Numeric vector that limits crossbreak processing
#' @param plotMultiplierLimits Numeric vector of length two, indicating lower and upper limit of vertical plot resizing
#' @param addPlotTitle If TRUE, adds question text to plot title
#' @param ... Other parameters passed to \code{\link[surveyor]{surveyPlot}}
#' @method surveyPlot surveybraid
#' @export
#' @seealso \code{\link{as.surveyor}}
surveyPlot.surveybraid <- function(
    x,
    qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    onlyBreaks=seq_along(x$surveyor$crossbreak),
    outputType = x$braid$outputType,
    plotSize = x$braid$defaultPlotSize,
    #plotMultiplierLimits = if(outputType=="ppt") c(0.8, 1.2) else c(0.8, 2.5),
    plotMultiplierLimits = c(0.8, 2.5),
    addPlotTitle = FALSE,
    ...
  ){
  
  surveyor <- x$surveyor
  braid    <- x$braid
  
  #browser()
  if(outputType=="ppt") if(!require(braidppt)) stop("Unable to load package braidppt")
  
  if(!exists(qid, surveyor$sdata) & is.null(which.q(surveyor$sdata, qid))){
    message(paste(qid,": Question not found.  Processing aborted"))
    return(NULL)
  }
  message(qid)
  plot_title <- qTextCommon(surveyor$sdata, qid)
  surveyor$plot_title <- plot_title
  if(outputType=="latex"){
    braidHeading(
        braid, 
        paste(qid, plot_title), 
        headinglevel= "section",
        pagebreak=FALSE)
  }
  
  #browser()
  
#  if (!is.list(surveyor$crossbreak)) {
#    surveyor$crossbreak <- list(surveyor$crossbreak)
#    onlyBreaks <- 1
#  }
  
#  lh <- lapply(onlyBreaks, function(i){
#        surveyor$cbreak <- unlist(surveyor$crossbreak[i])
#        surveyor$cbreakname <- names(surveyor$crossbreak[i])
#        surveyPlot(surveyor, qid, onlyBreaks=i, ...)
#        
#      })
  
  lh <- surveyPlot(surveyor, qid, statsFunction, plotFunction, codeFunction, 
      onlyBreaks=onlyBreaks, addPlotTitle=addPlotTitle, ...)
  
#  browser()
  
  lapply(seq_along(lh), function(i){
      catString <- surveybraidPrintQuestion(
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
#' @inheritParams surveyPlot.surveybraid
#' @keywords internal
surveybraidPrintQuestion <- function(i, surveyor, braid, qid, h, plotSize, outputType, 
    plotMultiplierLimits=c(1, 1)){
  
  
#  if(is.null(f)) return("\nNo data\n\n")
  if(class(h$plot)=="text") return(h$plot)
  
  # Print plot
  ###filename <- braidFilename(braid)
  #filename <- paste(qid, "_", surveyor$cbreakname, ".", braid$graphicFormat, sep="")
  filename <- paste(qid, "_", names(surveyor$crossbreak)[i], ".", braid$graphicFormat, sep="")
  message(paste(" --", filename))
  
  # Adjust vertical size of plot depending on number of questions
  # Make the assumption that 7 questions can fit on a plot
  # Limit vertical size to [1, 3]*size of default
  #browser()
  plotMin <- plotMultiplierLimits[1]
  plotMax <- plotMultiplierLimits[2]
  height_multiplier <- ifelse(
      is.numeric(h$nquestion), 
      min(plotMax, max(plotMin, h$nquestion / 7)),
      plotMin
  )
  #message(paste("In surveyorPrintQuestion, height_multiplier = ", height_multiplier))
  #browser()
  switch(outputType,
      latex = {
        braidPlot(braid, h$plot, filename=filename,
            width=plotSize[1], height=(plotSize[2] * height_multiplier), Qid=qid)
      },
      ppt = {
        stopifnot(require(braidppt))
        braidppt::braidpptPlot(braid, h$plot, filename=filename,
            width=plotSize[1], height=(plotSize[2] * height_multiplier), Qid=qid)
      }
  )
  
  catString <- if(surveyor$defaults$printTable) tableGuess(h) else ""
  
  return(catString)
}



