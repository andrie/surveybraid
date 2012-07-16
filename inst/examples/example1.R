# Prepares folders for braidsurveyor output

latexPath <- file.path(tempdir(), "latex")
graphPath <- file.path(latexPath, "graphics")
dir.create(latexPath, recursive=TRUE, showWarnings=FALSE)
dir.create(graphPath, recursive=TRUE, showWarnings=FALSE)

# Defines data frame with survey data

qData <- data.frame(
    Q1=c("Yes", "No", "Yes", "Yes"),
    Q4_1 = c(1, 2, 1, 2), 
    Q4_3=c(3, 4, 4, 3), 
    Q4_2=c(5, 5, 6, 6), 
    crossbreak=c("A", "A", "B", "B"), 
    crossbreak2=c("D", "E", "D", "E"),
    weight=c(0.9, 1.1, 0.8, 1.2)
)
varlabels(qData) <- c(
    "Question 1", 
    "Question 4: red", "Question 4: blue", "Question 4: green", 
    "crossbreak",
    "crossbreak2",
    "weight")

# Creates surveydata object

qData <- as.surveydata(qData, renameVarlabels=TRUE)

# Creates surveyor and surveybraid object

s <- as.surveyor(qData, crossbreak=qData$crossbreak, weight=qData$weight, 
    defaults=surveyorDefaults(printTable=FALSE))
b <- as.braid(path = latexPath, fileInner="surveyor_test.tex", outputType = "latex")
sb <- as.surveybraid(s, b)

# Write heading and two plots; then save braid

braidHeading(b, "Test")
surveyPlot(sb, "Q1", statsBin, plotBar)
surveyPlot(sb, "Q4", statsBin, plotBar)
braidSave(b)

# Check that braid output exists

file.exists(file.path(latexPath, "surveyor_test.tex"))
file.exists(file.path(graphPath, "Q1_.pdf"))
file.exists(file.path(graphPath, "Q4_.pdf"))
      

