AltFunc <- function(datsamp, InclCit='no', sortindex='as') {
  
  # make list
  doiList <- list(as.vector(as.character(datsamp[,1])))
  
  ## retrieve Altmetrics data
  results <- pmap_df(doiList, alm)
  
  ## isolate fields
  # latest Altmetric scores
  scores <- as.numeric(results$score)
  
  # first author
  firstAuth <- ifelse(is.na(results$authors1) == T, results$authors, results$authors1)
  
  # abbreviated title
  titlAbbr <- paste(substr(results$title, 1, 20), " ...", sep="")
  
  # journal title
  jrnName <- as.character(results$journal)
  
  # rank-in-context percentile
  rnkContextPc1 <- as.numeric(results$context.similar_age_journal_3m.rank)/as.numeric(results$context.similar_age_journal_3m.count)
  rnkContextPc <- 100*(round(ifelse(rnkContextPc1 == 1, 0, rnkContextPc1), 3))
  
  # all-time rank percentile
  rnkAlltimePc1 <- as.numeric(results$context.journal.rank)/as.numeric(results$context.journal.count)
  rnkAlltimePc <- 100*(round(ifelse(rnkAlltimePc1 == 1, 0, rnkAlltimePc1), 3))
  
  # time (years) since publication
  publTimeUnix <- as.numeric(results$published_on)
  publTime <- as.POSIXct(publTimeUnix, origin = "1970-01-01")
  publDate <- as.Date(publTime, format="%Y-%m-%d")
  publYr <- as.numeric(format(publDate,'%Y'))
  ElapsYrs <- as.numeric(as.Date(format(Sys.Date(), "%Y-%m-%d")) - publDate)/365
  
  if (InclCit == "yes") {
    ## CrossRef citations
    cites <- rep(NA,length(scores))
    for (c in 1:length(cites)) {
      cites[c] <- cr_citation_count(doi = doiList[[1]][c])$count
    }
    citesYr <- round(cites/ElapsYrs, 3) # cites/year
   
    rnkDat <- data.frame(firstAuth, publDate, titlAbbr, jrnName, datsamp[,1], scores, rnkContextPc, rnkAlltimePc, cites, citesYr)
    colnames(rnkDat) <- c("firstAu", "PublDate","title", "Journal", "doi", "AltmScore","rnkCxtPc","rnkAllPc","CRcites","CRcitesYr")
    rnkDatAsort <- rnkDat[order(rnkDat[,6],decreasing=T),1:10]
  } # end if
  
  if (InclCit == "no") {
    rnkDat <- data.frame(firstAuth, publDate, titlAbbr, jrnName, datsamp[,1], scores, rnkContextPc, rnkAlltimePc)
    colnames(rnkDat) <- c("firstAu", "PublDate","title", "Journal", "doi","AltmScore","rnkCxtPc","rnkAllPc")
    rnkDatAsort <- rnkDat[order(rnkDat[,6],decreasing=T),1:8]
  } # end if
  
  # print final output
  return(rnkDatAsort)
  
} # end AltFunc
