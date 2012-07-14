\name{surveyPlot.surveybraid}
\alias{surveyPlot.surveybraid}
\title{Codes and plots a survey question.}
\usage{
  \method{surveyPlot}{surveybraid} (x, qid,
    statsFunction = "statsGuess",
    plotFunction = "plotGuess",
    codeFunction = "codeQuickArray",
    onlyBreaks = seq_along(x$surveyor$crossbreak),
    outputType = x$braid$outputType,
    plotSize = x$braid$defaultPlotSize,
    plotMultiplierLimits = c(0.8, 2.5),
    addPlotTitle = FALSE, ...)
}
\arguments{
  \item{x}{surveybraid object}

  \item{qid}{Question id}

  \item{statsFunction}{A surveyor stats function}

  \item{plotFunction}{A surveyor plot function}

  \item{codeFunction}{A surveyor stats function}

  \item{onlyBreaks}{Numeric vector that limits crossbreak
  processing}

  \item{outputType}{"latex", "ppt" or "device": Specifies
  destination of ouput}

  \item{plotSize}{Size in inches of plot output, e.g.
  c(4,3)}

  \item{plotMultiplierLimits}{Numeric vector of length two,
  indicating lower and upper limit of vertical plot
  resizing}

  \item{addPlotTitle}{If TRUE, adds question text to plot
  title}

  \item{...}{Other parameters passed to
  \code{\link[surveyor]{surveyPlot}}}
}
\description{
  This is the top level function that determines how a
  question is processed, coded, printed and plotted.
}
\seealso{
  \code{\link{as.surveyor}}
}

