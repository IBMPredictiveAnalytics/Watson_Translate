# Watson Translate
#
# IBM SPSS Modeler Extension
#
# Install function for packages    
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(httr)
packages(XML)

# This function is used to generate automatically the dataModel for SPSS Modeler
getMetaData <- function (data) {
  if( is.null(dim(data)))
    stop("Invalid data received: not a data.frame")
  if (dim(data)[1]<=0) {
    print("Warning : modelerData has no line, all fieldStorage fields set to strings")
    getStorage <- function(x){return("string")}
  } else {
    getStorage <- function(x) {
      x <- unlist(x)
      res <- NULL
      #if x is a factor, typeof will return an integer so we treat the case on the side
      if(is.factor(x)) {
        res <- "string"
      } else {
        res <- switch(typeof(x),
                      integer="integer",
                      double = "real",
                      "string")
      }
      return (res)
    }
  }
  col = vector("list", dim(data)[2])
  for (i in 1:dim(data)[2]) {
    col[[i]] <- c(fieldName=names(data[i]),
                  fieldLabel="",
                  fieldStorage=getStorage(data[i]),
                  fieldMeasure="",
                  fieldFormat="",
                  fieldRole="")
  }
  mdm<-do.call(cbind,col)
  mdm<-data.frame(mdm)
  return(mdm)
}

version <- R.Version()$major #Get R version for Post call
print(version)

data <- data.frame() #Data frame to be populated with results
u <- "%%user%%"   #Username and Password provided in CDB
p <- "%%password%%"
source_language =%%source%%  #From CDB Combo Boxes
  target_language =%%target%%
  base <- "https://gateway.watsonplatform.net/language-translation/api/v2/translate?source="

if (source_language != target_language){
  for(i in 1:nrow(modelerData)) {
    text <-modelerData$%%text%%[i] #Load text       
    if (!(is.element("en", c(source_language, target_language)))){
      language<-"Unable to convert non-english to non-english at this time" }
    else if(source_language != target_language) {
      full_url <- paste(base,source_language,"&target=", target_language, sep = '')
      pars <- list(txt=iconv(text,to="UTF-8"))
      if (version == 2){
        r<-POST(full_url,authenticate(u, p), body = pars)
      } else{
        r<-POST(full_url,authenticate(u, p),add_headers(Accept = "text/plain"), body = pars)
      }
      stop_for_status(r)
      language<-content(r)            
    }                         
    data[i,1] <- language
  }
  
  modelerData <-cbind(modelerData,data)
  colnames(modelerData)[ncol(modelerData)] <- "Translated Text" #Name new column
  modelerDataModel <- getMetaData(modelerData)
} else {
  print("Choose different languages for source and target values!")
}
