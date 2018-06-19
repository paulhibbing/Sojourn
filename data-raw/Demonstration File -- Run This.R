## Welcome to the demonstration. If you opened this file in RStudio, you
## should have no trouble from here. All you need to do is highlight
## the lines of code you want to run, and click "Run." You can run
## just a few lines to get an idea of what's happening a bit at a time,
## or you can highlight everything to run the whole file.
## Either way, extensive documentation (like this) will be present throughout,
## to help you understand what's happening in the code.

## For starters, let's get set up.
## In order for the code to run, it needs to be set to look for files
## in the right place. This happens automatically if you didn't have
## RStudio open before you opened this file, but either way, the
## best way to check is to run the following lines:

  if(basename(getwd())!='YouthSojournDemo'){
    message('Working directory is not set to the same folder as the Sojourns demo.')
    message('Please locate the YouthSojournDemo folder in the dialog that pops up.')
    message('If you don\'t see the dialog, you may have to minimize RStudio.')
    setwd(choose.dir(default = getwd(), caption = 'Find the folder called YouthSojournDemo'))
    
    counter = 1
    repeat{
        if(counter==1&basename(getwd())=='YouthSojournDemo'){
          message('Now we\'re set.')
          rm(counter)
          break
        } else{
          message('That didn\'t work. Try finding the folder again.')
          setwd(choose.dir(default = getwd(), caption = 'Find the folder called YouthSojournDemo'))
        }
      }
    } ## This will prompt you to find the right folder if it wasn't automatically selected

## Now for a few more lines of set up to load the Sojourn materials
  source('Sojourn R Materials/Final Sojourn Functions.R')
    ## ^^^ This loads Sojourn functions, one of which is sojourns.startup()
    ## which we'll now run. This function sets up your machine to use
    ## Sojourns, including installing some packages if you don't already
    ## have them. That's a one-time thing that could take some time.
    ## Otherwise, it's just a matter of loading everything.
    ## You'll see some messages print out in the console, and it will tell you
    ## when it's done and everything is ready to go.
  sojourns.startup()
  
## Now all we have to do is identify the file(s) we're going to run,
## and then process them with Sojourns. Not much to it.
  
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
  
## If you want to save as csv, it's easy enough.
## These will be saved in the "Example Output" folder.
  data.table::fwrite(countsData, file = 'Example Output/CountsExample.csv',
                     row.names = F)
  data.table::fwrite(rawData, file = 'Example Output/RawExample.csv',
                      row.names = F)