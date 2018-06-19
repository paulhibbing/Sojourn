  countsFile <- 'ActiGraph Files/ExampleFile_CountsHip.csv'
    ## ^^^ The location and filename of the counts data file to process
  countsData <- read.ActiGraph(countsFile)
    ## ^^^ This runs a function that will read the file and process it
    ## ^^^ using Sojourns. It will prompt you to give it information that it needs,
    ## including demographics, which you can make up for purposes of the demo.
  View(countsData) ## This will pull up a tab for you to see the processed data

## We can do the same thing with a raw acceleration file
  rawFile <- 'ActiGraph Files/ExampleFile_RawHip.csv'
  rawData <- read.ActiGraph(rawFile)
  View(rawData)
