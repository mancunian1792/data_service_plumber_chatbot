library(plumber)
library(jsonlite)
library(dplyr)
library(nhis5110)
library(aws.s3)
library(ggplot2)

# The dir path should be changed to server path once changed.
dir_path <- '/home/mancunian92/Documents/courses/Data Management and Processing/Dataset/final_project_demo_images/'
aws_path <- 'https://ds-5110-storage.s3.amazonaws.com/'




#* @apiTitle Webhook Call

#* Webhook Call from dialogflow
#* @post /webhook
#* @serializer unboxedJSON

function(req) {
  # Converting the request object to JSON.
  dialog_req <- fromJSON(req$postBody)
  intent_name <- dialog_req$queryResult$intent$displayName

  #Getting response for individual intents.
  response <- mapFunctions(intent_name, dialog_req)
  #Formatting response for dialogflow.
  list(fulfillmentText = "response from webhook", payload = list(response = response))
  #as.data.frame(fullfillmentText = "response from webhook", payload = list(response = "Hi"))
}


#* @apiTitle Get the colnames for all the dataframes that we want to expose.
#* Get the names of the columns of all the dataframes that is present the custom package.
#* @get /names/columns
#* @serializer unboxedJSON

function(req) {
  #Get all the datasets found in the nhis package.
  nhis_datasets <- data(package = "nhis5110")$results[, "Item"]
  nhis_datasets <- setdiff(nhis_datasets, c("nhisPerson", "family", "sampleAdult"))
  # We are returning here only 200 because of dialogflow's limitation.
  all_cols <- unique(unlist(lapply(nhis_datasets, giveNames)))
  # Format the response to be sent.
  list(displayName = "columns", kind = "KIND_MAP", entities = list(value = "columns", synonyms = all_cols))
}


#* @apiTitle Get the names of all the dataframes that we have our package.
#* Get all the names of the dataframes that is present in the custom package.
#* @get /names/dataframes
#* @serializer unboxedJSON

function(req) {
  nhis_datasets <- data(package = "nhis5110")$results[, "Item"]
  nhis_datasets <- setdiff(nhis_datasets, c("nhisPerson", "family", "sampleAdult"))
  list(displayName = "dataframes", kind = "KIND_MAP", entities = list(value = "dataframes", synonyms = nhis_datasets))
}

#* @apiTitle Get the names of all the dataframes that we have our package.
#* Get all the names of the dataframes that is present in the custom package.
#* @get /check
#* @serializer unboxedJSON

function(req) {
p <- ggplot()+aes(nhis5110::family$PHONEUSE)+geom_bar()
aws_file_path<- uploadPlotToS3(p)
list(path = aws_file_path)
}

# Support Functions for the api's

giveNames <- function(df) {
  df <- paste("nhis5110::", df, sep="")
  df_nhis<- eval(parse(text = df))
  names(df_nhis)
}

getMyBucket <- function() {
  ds_5110_bucket<-get_bucket(
    bucket = 'ds-5110-storage',
    key = Sys.getenv(c("key")),
    secret = Sys.getenv(c("secret"))
  )
  return(ds_5110_bucket)
}
mapFunctions <- function(intentName, payload){
  switch(intentName,
         "get-relationship" = getRelation(payload),
         "get-help" = getHelp(),
         "get-data-options" = getDataOptions(),
         "get-dataframe-name" = loadDataFrameOptions(),
         "get-relationship-options" = getRelationshipOptions(payload))
}

getRelation <- function(payload) {
  print("Am i here ?!")
  dataFrameName <- payload$queryResult$outputContexts$parameters$dataframes[1]
  columnName1 <- payload$queryResult$outputContexts$parameters$columns1[1]
  columnName2 <- payload$queryResult$outputContexts$parameters$columns2[1]

  col1class <- eval(parse(text=paste("class(nhis5110::", dataFrameName, "$", columnName1, ")", sep = "")))
  col2class <- eval(parse(text=paste("class(nhis5110::", dataFrameName, "$", columnName2, ")", sep = "")))
  print(paste(columnName1, columnName2, col1class, col2class, sep = "-----"))
  plotText <- ""
  if ((col1class == "integer" || col1class == "numeric") && (col2class == "integer" || col2class == "numeric")) {
    plotText <- paste("ggplot2::ggplot(mapping = aes(", dataFrameName, "$", columnName1, ",", dataFrameName, "$", columnName2, "))+geom_point()", sep="")
  } else if ((col1class == "integer" || col1class == "numeric") && col2class == "factor") {
    plotText <- paste("ggplot2::ggplot(mapping = aes(", dataFrameName, "$", columnName2, ",", dataFrameName, "$", columnName1, "))+geom_boxplot()", sep="")
  } else if (col1class == "factor" && (col2class == "integer" || col2class == "numeric")) {
    plotText <- paste("ggplot2::ggplot(mapping = aes(", dataFrameName, "$", columnName1, ",", dataFrameName, "$", columnName2, "))+geom_boxplot()", sep="")
  } else if (col1class == "factor" && col2class == "factor") {
    plotText <- paste("ggplot2::ggplot(mapping = aes(", dataFrameName, "$", columnName1, ",", "fill=", dataFrameName, "$", columnName2, "))+geom_bar()", sep="")
  }

  if(plotText != "") {
    plot <- eval(parse(text = plotText))
    aws_path <- uploadPlotToS3(plot)
    return(list(list(MSG_TYPE = "INFO", message = paste("Here's your plot.", aws_path))))
  } else {
    return(list(list(MSG_TYPE = "INFO", message = "Sorry, We are not able to plot that currently.")))
  }
}

getRelationshipOptions <- function(payload) {
  dataFrameName <- payload$queryResult$outputContexts$parameters$dataframes[1]
  colNames <- get_df_metadata(name = dataFrameName)
  return(list(list(MSG_TYPE = "OPTIONS", message = list(repeatOptions = 2, chatText = "Choose the columns that you want relationship for.", name = "columns", text = "Select a column..", options = colNames))))
}

getDataOptions <- function() {
metadata <- nhis5110::get_metadata_dataframes()
return (list(list(MSG_TYPE = "OPTIONS", message = list(repeatOptions = 1, chatText = "Choose a dataframe from one of the options", name = "dataframe", text = "Select a dataframe..", options = metadata))))

}

loadDataFrameOptions <- function() {
  return (list(list(MSG_TYPE = "INFO", message = "Your choice of data is taken into account."),
               list(MSG_TYPE = "INFO", message = "You can run sample statements like, What columns can i relate for ?. If you already know the column names for which you need a relationship, enter them like, Tell relationship between HHX and RECTYPE, for example")))
}

getHelp <- function(payload) {
  return(list(list(MSG_TYPE = "INFO", message = "Hi,User.NHIS Chatbot at your service. You can ask questions related to the National Health Survey Data"),
              list(MSG_TYPE= "INFO", message = "You can ask questions like , What data do you have? for starters.  Other things you can do, is ask about relationship between 2 variables")))
}

uploadPlotToS3 <- function(pp) {
  image_name <- paste("chatbot-images", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), sep = "-")
  image_with_extension <- paste(image_name, ".png", sep = "")
  file_name <- paste(dir_path, image_with_extension,sep="")
  ggsave(filename = file_name, plot = pp)
  b <- getMyBucket()
  aws_file <- paste("images/", image_with_extension, sep = "")
  put_object(file = file_name, object = aws_file, bucket = b)
  return(paste(aws_path, aws_file, sep = ""))
}
# Command to execute the file and expose it as a service.

#plumber::plumb(file = "/home/mancunian92/Documents/courses/Data Management and Processing/data-service-final-project/data-service.R")$run(port = 8081)

