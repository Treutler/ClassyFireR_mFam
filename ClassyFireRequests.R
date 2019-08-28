
inchisToTruncatedInchis <- function(inchis){
  inchisSplitted <- strsplit(x = inchis, split = "/")
  structurePrefixes <- unlist(lapply(X = inchisSplitted, FUN = function(x){
    toTruncate <- grepl(pattern = "^[btmsifr].+", x = x)
    if(any(toTruncate)){
      x <- x[1:(min(which(toTruncate)) - 1)]
    }
    return(paste(x, collapse = "/"))
  }))
  return(structurePrefixes)
}
classyFireQuery <- function(setOfInChIs, parentFolder, annoFileAll, annoFileAllPresent, numberOfInChIsPerQuery = 10){
  library("stringr")
  library("stringi")
  
  classificationVersion <- "2.1"
  
  subFolderName <- basename(annoFileAll)
  subFolder <- paste(parentFolder, "/", subFolderName, sep = "")
  if(!file.exists(subFolder))
    if(!dir.create(subFolder))
      stop(paste("Creation of folder", subFolder, "failed"))
  
  #######################################################################################
  ## data
  setOfInChIsPresent <- read.table(file = annoFileAllPresent, sep = "\t", header = TRUE, stringsAsFactors = FALSE, comment.char = "")$InChI
  setOfMainClassesPresent <- read.table(file = annoFileAllPresent, sep = "\t", header = TRUE, stringsAsFactors = FALSE, comment.char = "")$CHEMONT_name
  if(FALSE){
    setOfInChIsPresentButUnannotated <- setOfInChIsPresent[setOfMainClassesPresent==""]
    setOfInChIs <- setOfInChIsPresentButUnannotated
  }
  
  selectedInPresent <- setOfInChIsPresent %in% setOfInChIs
  sum(selectedInPresent)
  setOfInChIsPresent <- setOfInChIsPresent[selectedInPresent]
  setOfMainClassesPresent <- setOfMainClassesPresent[selectedInPresent]
  setOfInChIs <- setOfInChIs[!(setOfInChIs %in% setOfInChIsPresent)]
  length(setOfInChIs)
  
  formatted <- stri_startswith_fixed(pattern = "InChI=", str = setOfInChIs)
  print(paste(sum(formatted), length(formatted)))
  setOfInChIs[!formatted]
  setOfInChIs <- setOfInChIs[formatted]
  setOfInChIs <- unique(inchisToTruncatedInchis(setOfInChIs))
  
  ## chunked ClassyFire queries
  #numberOfInChIsPerQuery <- 10
  chunks <- split(setOfInChIs, cut(seq_along(setOfInChIs), ceiling(length(setOfInChIs) / numberOfInChIsPerQuery), labels = FALSE))
  rm(setOfInChIs)
  
  #######################################################################################
  ## process inchis chunk-wise
  invalidRequestIndeces <- NULL
  failedChunks <- NULL
  
  startChunk <- -1
  missingChunks <- NULL
  for(chunkIdx in seq_along(chunks)){
    annoFile        <- paste(subFolder, "/Chunk_", chunkIdx, "_Anno_result.tsv", sep = "")
    if(!file.exists(annoFile)){
      if(startChunk == -1)
        startChunk <- chunkIdx
      missingChunks <- c(missingChunks, chunkIdx)
      #break
    }
  }
  print(paste("Start here:", startChunk))
  
  queryIDs <- vector(mode = "character", length = length(chunks))
  reprocess <- FALSE
  startTime <- Sys.time()
  #for(chunkIdx in seq_along(chunks)){
  #for(chunkIdx in c(61, 841, 2087, 2103, 2105, 2132, 2170, 2184, 2185, 2186, 2195, 2237, 2238)){
  #for(chunkIdx in 2533:length(chunks)){
  #for(chunkIdx in startChunk:length(chunks)){
  #reprocess <- TRUE
  #missingChunks <- failedChunks
  #missingChunks <- failedIndeces
  #for(chunkIdx in failedIndeces){
  for(chunkIdx in missingChunks){
    chunkFile       <- paste(subFolder, "/Chunk_", chunkIdx, "_InChI.txt", sep = "")
    queryResultFile <- paste(subFolder, "/Chunk_", chunkIdx, "_query_result.json", sep = "")
    annoFile        <- paste(subFolder, "/Chunk_", chunkIdx, "_Anno_result.tsv", sep = "")
    
    print(paste("+++", chunkIdx, "/", length(chunks)))
    
    if(file.exists(chunkFile))
      currentChunk <- readLines(con = chunkFile)
    else
      currentChunk <- chunks[[chunkIdx]]
    currentChunkImpl <- paste(currentChunk, collapse = "\\n")
    
    tryCatch({
      #if(FALSE){
      if(!file.exists(chunkFile) | reprocess)
        writeLines(text = currentChunk, con = chunkFile)
      
      queryId <- NULL
      if(!file.exists(queryResultFile) | reprocess){
        if(queryIDs[[chunkIdx]] == ""){
          cmdClassyFire <- paste(
            "curl",
            " -is http://classyfire.wishartlab.com/queries.json",
            " -X POST",
            " -d '{\"label\":\"curl_test\",\"query_input\":\"", currentChunkImpl, "\",\"query_type\":\"STRUCTURE\"}'",
            " -H \"Content-Type: application/json\"",
            sep = ""
          )
          ## exec ClassyFire rest API
          suppressWarnings(
            output <- system(command = cmdClassyFire, intern = TRUE)
          )
          ## parse query ID
          
          if(any(grepl(pattern = "Invalid Request", x = output)) | any(grepl(pattern = "Internal Server Error", x = output))){
            error <- "### Unknown ###"
            if(any(grepl(pattern = "Invalid Request", x = output)))
              error <- "Invalid Request"
            if(any(grepl(pattern = "Internal Server Error", x = output)))
              error <- "Internal Server Error"
            
            print(paste("### Request for chunk", chunkIdx, "not successful:", error))
            invalidRequestIndeces <- c(invalidRequestIndeces, chunkIdx)
            next
          }
          
          #output <- output[length(output)]
          output2 <- paste(output[21:length(output)], collapse = "")
          queryId <- strsplit(x = strsplit(x = output2, split = ",")[[1]][[1]], split = ":")[[1]][[2]]
          queryIDs[[chunkIdx]] <- queryId
        } else {
          queryId <- queryIDs[[chunkIdx]]
        }
        
        ## request results
        cmdClassyFire <- paste(
          "curl",
          " -H \"Accept: application/json\"",
          " -H \"Content-Type: application/csv\"",
          " -X GET ", paste("http://classyfire.wishartlab.com/queries/", queryId, ".json", sep = ""),
          sep = ""
        )
        
        ## fetch ClassyFire results
        goOn <- TRUE
        numberOfClassifiedThings <- 0
        numberOfUnsuccessfulIterations <- 0
        maximumNumberOfUnsuccessfulIterations <- 3
        first <- TRUE
        while(goOn){
          ## wait for the results
          if(first){
            Sys.sleep(time = 10)
            first <- FALSE
          } else {
            Sys.sleep(time = 3)
          }
          
          ## {\"id\":478522,\"label\":\"curl_test\",\"classification_status\":\"In Queue\",\"invalid_entities\":[],\"entities\":[]}
          suppressWarnings(
            output <- system(command = cmdClassyFire, intern = TRUE)
          )
          output <- paste(output, collapse = "")
          
          identifiers <- str_extract_all(string = output, pattern = paste("Q", queryId, "\\-[0-9]+", sep = ""))[[1]]
          numberOfClassifiedThingsHere <- length(identifiers)
          
          if(numberOfClassifiedThingsHere == numberOfClassifiedThings){
            numberOfUnsuccessfulIterations <- numberOfUnsuccessfulIterations + 1
          } else {
            numberOfUnsuccessfulIterations <- 0
          }
          numberOfClassifiedThings <- numberOfClassifiedThingsHere
          
          if(!(
            grepl(pattern = "\"classification_status\":\"In Queue\"", x = output) | 
            grepl(pattern = "\"classification_status\":\"In progress\"", x = output) | 
            grepl(pattern = "\"classification_status\":\"Processing\"", x = output)
          ))
            goOn <- FALSE
          if(numberOfUnsuccessfulIterations == maximumNumberOfUnsuccessfulIterations)
            goOn <- FALSE
        }
        
        writeLines(text = output, con = queryResultFile)
      } else {
        ## query was successful but parsing of query-result was not successful
        output <- readLines(con = queryResultFile)
        queryId <- str_extract(string = str_extract(string = output, pattern = "\\{\"id\":[0-9]+,\"label\":"), pattern = "[0-9]+")
      }
      
      ## process results
      #Q477822-1
      identifiers <- str_extract_all(string = output, pattern = paste("Q", queryId, "\\-[0-9]+", sep = ""))[[1]]
      
      chebiIdentified2           <- vector(mode = "character", length = length(currentChunk))
      chebiPredictedImpl2        <- vector(mode = "character", length = length(currentChunk))
      chemont_names2             <- vector(mode = "character", length = length(currentChunk))
      chemont_ids2               <- vector(mode = "character", length = length(currentChunk))
      alternative_parents_names2 <- vector(mode = "character", length = length(currentChunk))
      alternative_parents_ids2   <- vector(mode = "character", length = length(currentChunk))
      substituents2              <- vector(mode = "character", length = length(currentChunk))
      smiles2                    <- vector(mode = "character", length = length(currentChunk))
      inchiKeys2                 <- vector(mode = "character", length = length(currentChunk))
      
      if(length(identifiers) > 0){
        identifierIndeces <- as.integer(unlist(lapply(X = strsplit(x = identifiers, split = "-"), FUN = function(x){x[[2]]})))
        
        entries <- strsplit(x = output, split = "\"identifier\":")[[1]]
        entries <- entries[2:length(entries)]
        
        # - identifier
        # + smiles
        # + inchikey
        # + kingdom
        # + superclass
        # + class
        # + subclass
        # + intermediate_nodes
        # + direct_parent
        # + alternative_parents
        # - molecular_framework
        # + substituents
        # - description
        # + external_descriptors
        # + predicted_chebi_terms
        # - predicted_lipidmaps_terms
        # + classification_version
        
        ## IDs
        smiles <- str_extract(string = entries, pattern = "\"smiles\":.*\"inchikey\":")
        smiles <- unlist(lapply(X = strsplit(x = smiles, split = "\""), FUN = function(x){x[[4]]}))
        
        inchikey <- str_extract(string = entries, pattern = "\"inchikey\":.*\"kingdom\":")
        inchikey <- unlist(lapply(X = strsplit(x = inchikey, split = "\""), FUN = function(x){x[[4]]}))
        inchikey <- gsub(x = inchikey, pattern = "InChIKey=", replacement = "")
        
        ## chemont hierarchy
        kingdom <- str_extract(string = entries, pattern = "\"kingdom\":.*\"superclass\":")
        kingdom_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = kingdom, pattern = "\"name\":.*\"description\":"))
        kingdom_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = kingdom, pattern = "\"chemont_id\":.*\"url\":"))
        
        superclass <- str_extract(string = entries, pattern = "\"superclass\":.*\"class\":")
        superclass_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = superclass, pattern = "\"name\":.*\"description\":"))
        superclass_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = superclass, pattern = "\"chemont_id\":.*\"url\":"))
        
        class <- str_extract(string = entries, pattern = "\"class\":.*\"subclass\":")
        class_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = class, pattern = "\"name\":.*\"description\":"))
        class_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = class, pattern = "\"chemont_id\":.*\"url\":"))
        
        subclass <- str_extract(string = entries, pattern = "\"subclass\":.*\"intermediate_nodes\":")
        subclass_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = subclass, pattern = "\"name\":.*\"description\":"))
        subclass_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = subclass, pattern = "\"chemont_id\":.*\"url\":"))
        
        intermediate <- str_extract(string = entries, pattern = "\"intermediate_nodes\":.*\"direct_parent\":")
        intermediate_names <- lapply(X = str_split(string = intermediate, pattern = "\\},\\{"), FUN = function(x){
          gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = x, pattern = "\"name\":.*\"description\":"))
        })
        intermediate_names <- unlist(lapply(X = intermediate_names, FUN = function(x){
          if(!all(is.na(x)) & length(x) > 1){
            return(paste(x, collapse = "; "))
          } else {
            return(x)
          }
        }))
        intermediate_ids <- lapply(X = str_split(string = intermediate, pattern = "\\},\\{"), FUN = function(x){
          gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = x, pattern = "\"chemont_id\":.*\"url\":"))
        })
        intermediate_ids <- unlist(lapply(X = intermediate_ids, FUN = function(x){paste(x, collapse = "; ")}))
        
        ## wrong
        #intermediate <- str_extract(string = entries, pattern = "\"intermediate_nodes\":.*\"direct_parent\":")
        #intermediate_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = intermediate, pattern = "\"name\":.*\"description\":"))
        #intermediate_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = intermediate, pattern = "\"chemont_id\":.*\"url\":"))
        
        directParent <- str_extract(string = entries, pattern = "\"direct_parent\":.*\"alternative_parents\":")
        directParent_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = directParent, pattern = "\"name\":.*\"description\":"))
        directParent_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = directParent, pattern = "\"chemont_id\":.*\"url\":"))
        
        chemont_names <- vector(mode = "character", length = length(identifierIndeces))
        chemont_ids   <- vector(mode = "character", length = length(identifierIndeces))
        for(entryIdx in seq_along(identifierIndeces)){
          chemont_names3 <- c(kingdom_name     [[entryIdx]], superclass_name     [[entryIdx]], class_name     [[entryIdx]], subclass_name     [[entryIdx]], intermediate_names     [[entryIdx]], directParent_name     [[entryIdx]])
          chemont_ids3   <- c(kingdom_chemontid[[entryIdx]], superclass_chemontid[[entryIdx]], class_chemontid[[entryIdx]], subclass_chemontid[[entryIdx]], intermediate_ids       [[entryIdx]], directParent_chemontid[[entryIdx]])
          
          removeIndeces <- which(is.na(chemont_names3))
          if(length(removeIndeces) > 0){
            chemont_names3 <- chemont_names3[-removeIndeces]
            chemont_ids3   <- chemont_ids3  [-removeIndeces]
          }
          
          if(chemont_names3[[length(chemont_names3)]] == chemont_names3[[length(chemont_names3) - 1]]){
            chemont_names3 <- chemont_names3[1:(length(chemont_names3) - 1)]
            chemont_ids3   <- chemont_ids3  [1:(length(chemont_ids3  ) - 1)]
          }
          
          chemont_names[[entryIdx]] <- paste(chemont_names3, collapse = "; ")
          chemont_ids  [[entryIdx]] <- paste(chemont_ids3, collapse = "; ")
        }
        #chemont_names <- paste(kingdom_name, superclass_name, class_name, subclass_name, intermediate_name, directParent_name, sep = "; ")
        #chemont_ids   <- paste(kingdom_chemontid, superclass_chemontid, class_chemontid, subclass_chemontid, intermediate_chemontid, directParent_chemontid, sep = "; ")
        
        alternative_parents <- str_extract(string = entries, pattern = "\"alternative_parents\":.*\"molecular_framework\":")
        alternative_parents_names <- lapply(X = str_split(string = alternative_parents, pattern = "\\},\\{"), FUN = function(x){
          gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = x, pattern = "\"name\":.*\"description\":"))
        })
        alternative_parents_names <- unlist(lapply(X = alternative_parents_names, FUN = function(x){paste(x, collapse = "; ")}))
        alternative_parents_ids <- lapply(X = str_split(string = alternative_parents, pattern = "\\},\\{"), FUN = function(x){
          gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = x, pattern = "\"chemont_id\":.*\"url\":"))
        })
        alternative_parents_ids <- unlist(lapply(X = alternative_parents_ids, FUN = function(x){paste(x, collapse = "; ")}))
        
        substituents <- gsub(pattern = "(\"substituents\":\\[)|(\\],\"description\":)", replacement = "", x = str_extract(string = entries, pattern = "\"substituents\":.*\"description\":"))
        substituents <- gsub(pattern = "\",\"", replacement = "; ", x = substituents)
        substituents <- gsub(pattern = "\"", replacement = "", x = substituents)
        
        #intermediate_nodes_name <- gsub(pattern = "(\"name\":\")|(\",\"description\":)", replacement = "", x = str_extract(string = subclass, pattern = "\"name\":.*\"description\":"))
        #intermediate_nodes_chemontid <- gsub(pattern = "(\"chemont_id\":\")|(\",\"url\":)", replacement = "", x = str_extract(string = subclass, pattern = "\"chemont_id\":.*\"url\":"))
        
        ## chebi identified
        external_descriptors <- unlist(lapply(X = strsplit(x = entries, split = "\"external_descriptors\":"), FUN = function(x){if(length(x)==1){return("")} else {x[[2]]}}))
        chebiIdentified <- unlist(lapply(X = strsplit(x = as.character(str_extract_all(string = external_descriptors, pattern = "source_id\":\"CHEBI:[0-9]+")), split = "\":\""), FUN = function(x){if(length(x) > 1){return(x[[2]])}else{return("")}}))
        
        ## chebi predicted
        external_descriptors <- unlist(lapply(X = strsplit(x = entries, split = "\"predicted_chebi_terms\":"), FUN = function(x){if(length(x)==1){return("")} else {x[[2]]}}))
        chebiPredicted <- str_extract_all(string = external_descriptors, pattern = "CHEBI:[0-9]+")
        chebiPredictedImpl <- unlist(lapply(X = chebiPredicted, FUN = function(x){paste(x, collapse = "; ")}))
        
        classification_version <- unlist(lapply(X = strsplit(x = entries, split = "\"classification_version\":"), FUN = function(x){if(length(x)==1){return("")} else {x[[2]]}}))
        classification_version <- unlist(lapply(X = strsplit(x = classification_version, split = "\""), FUN = function(x){x[[2]]}))
        
        ## check version
        if(any(classification_version != classificationVersion))
          stop(paste("Classification version from ClassyFire is", classification_version, "; expected is", classificationVersion))
        
        rm(entries)
        
        ## box
        smiles2   [identifierIndeces] <- smiles
        inchiKeys2[identifierIndeces] <- inchikey
        
        chemont_names2[identifierIndeces] <- chemont_names
        chemont_ids2[identifierIndeces] <- chemont_ids
        
        alternative_parents_names2[identifierIndeces] <- alternative_parents_names
        alternative_parents_ids2[identifierIndeces]   <- alternative_parents_ids
        substituents2[identifierIndeces]              <- substituents
        
        chebiIdentified2[identifierIndeces] <- chebiIdentified
        chebiPredictedImpl2[identifierIndeces] <- chebiPredictedImpl
      }
      
      ## assemble
      tmpDf <- data.frame(
        "InChI"            = currentChunk, 
        "ChEBI_identified" = chebiIdentified2, 
        "ChEBI_predicted"  = chebiPredictedImpl2, 
        "CHEMONT_name"     = chemont_names2, 
        "CHEMONT_IDs"      = chemont_ids2, 
        "AlternativeParents_name" = alternative_parents_names2,
        "AlternativeParents_IDs"  = alternative_parents_ids2,
        "Substituents"     = substituents2,
        "SMILES"           = smiles2,
        "InChIKey"         = inchiKeys2,
        stringsAsFactors = FALSE
      )
      write.table(x = tmpDf, file = annoFile,  row.names = FALSE, sep = "\t", quote = FALSE)
    }, error = function(e) {
      #error <- e
      print(e)
      failedChunks <<- c(failedChunks, chunkIdx)
    })
  }
  endTime <- Sys.time()
  print(endTime - startTime)
  
  print("Invalid request indeces:")
  print(invalidRequestIndeces)
  notProcessedInChIs <- unlist(chunks[invalidRequestIndeces])
  
  #######################################################################################
  ## merge chunks
  #allDf <- data.frame()
  chunkDfs <- list()
  failedCount   <- vector(mode = "integer", length = numberOfInChIsPerQuery + 1)
  failedIndeces <- vector(mode = "integer")
  #numberOfChunks <- length(list.files(path = subFolder, pattern = paste("Chunk_\\d+_Anno_result.tsv$", sep = ""), full.names = T))
  numberOfChunks <- length(list.files(path = subFolder, pattern = paste("Chunk_\\d+_InChI.txt$", sep = ""), full.names = T))
  for(chunkIdx in seq_len(numberOfChunks)){
    #print(paste(chunkIdx, "/", length(chunks)))
    annoFile        <- paste(subFolder, "/Chunk_", chunkIdx, "_Anno_result.tsv", sep = "")
    if(!file.exists(annoFile)){
      print(paste("Not there:", chunkIdx))
      next
    }
    
    tmpDf <- read.table(file = annoFile, sep = "\t", header = TRUE, stringsAsFactors = FALSE, comment.char = "", quote = "")
    if(nrow(tmpDf) == 0){
      lines <- readLines(con = annoFile)
      data <- unlist(lapply(X = lines, FUN = function(x){strsplit(x = x, split = "\t")}))
      tmpDf <- data.frame(matrix(data = data, ncol = 8, byrow = TRUE), stringsAsFactors = FALSE)
      colnames(tmpDf) <- as.character(tmpDf[1,])
      tmpDf <- tmpDf[-1,]
      if(nrow(tmpDf) == 0){
        print(paste("Empty:", chunkIdx))
      }
    }
    if(nrow(tmpDf) != numberOfInChIsPerQuery && nrow(tmpDf) != (numberOfInChIsPerQuery-1))
      print(paste(chunkIdx, nrow(tmpDf)))
    numberOfFailed <- max(sum(is.na(tmpDf$CHEMONT_IDs)), sum(tmpDf$CHEMONT_IDs=="", na.rm = T))
    if(numberOfFailed == nrow(tmpDf)) numberOfFailed <- numberOfInChIsPerQuery
    if(numberOfFailed == numberOfInChIsPerQuery){
    #if(numberOfFailed == nrow(tmpDf)){
      print(paste(chunkIdx, "failed for all", nrow(tmpDf), "cases"))
      failedIndeces[[length(failedIndeces) + 1]] <- chunkIdx
    }
    failedCount[[numberOfFailed + 1]] <- failedCount[[numberOfFailed + 1]] + 1
    
    tmpDf[is.na(tmpDf)] <- ""
    
    chunkDfs[[chunkIdx]] <- tmpDf
    #allDf <- rbind(allDf, tmpDf)
  }
  failedCount[failedCount==0] <- NA
  plot(x = seq(from=0, to=numberOfInChIsPerQuery), y=failedCount, log="y", xlab = "Number of failed InChIs in chunk", ylim = "Number of chunks")
  
  
  allDf <- do.call("rbind", chunkDfs)
  write.table(x = allDf, file = annoFileAll, sep = "\t", row.names = FALSE)
  
  setOfInChIsNotClassyFireable <- allDf$InChI[allDf$CHEMONT_IDs == ""]
  length(setOfInChIsNotClassyFireable)
  #writeLines(text = setOfInChIsNotClassyFireable, con = "/home/htreutle/Downloads/tmp/InchisNotClassyFireable.txt")
  return(setOfInChIsNotClassyFireable)
}
