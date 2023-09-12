#' Perform Variance Check
#'
#' The function is to compare the variance in one site to variance in the other sites.
#' @importFrom stats fisher.test
#' @param data A data frame with the site name/number (can be string or numeric) in the first column and any continuous variables to check in the following columns.
#' @param title This provides the prefix of the title for table. Default is empty.
#' @export
#' @returns sites with extremely low variance
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' var_check(data, '')
#' }



var_check<- function(data,title=''){
  ## The program will loop over the variables to check and test each
  ## one in turn.
  i<-2 ## start at 2 as the site number is the first column
  no.sites.checked<-0 ##will keep track of the number of sites that could be checked

  while(i<=dim(data)[2]){

    data.var<-data[,c(1,i)]	## selecting just the column to test and the site number

    ## remove missing observations
    data.var<-subset(data.var, is.na(data.var[,2])==FALSE)

    ## sort by site
    data.var<-data.var[order(data.var[,1]),]

    ## now split by site and test
    sites<- unique(data.var[,1])

    j<-1		## now we loop over the sites
    data.sites<-list() ##to store the site info
    data.allother<- list() 	##to store the info about all other sites bar the site in question.
    ##for comparison.
    results<-c("Site", "p-value")	##results will be collected here and added to the lists

    while(j<=length(sites)){
      sitej<-subset(data.var, data.var[,1]==sites[j])
      allotherj<-subset(data.var, data.var[,1]!=sites[j])
      varsites<-var(as.vector(unlist(sitej[2])))
      varother<-var(as.vector(unlist(allotherj[2])))

      data.sites[[j]]<-cbind('Variance', varsites)
      data.allother[[j]]<-cbind('Variance', varother)

      ## Now check there are enough observations in each level<-cbind(cats,freqsites)
      ## to perform a F-test
      if (dim(sitej)[1]<=2 & dim(allotherj)[1]<=2){results<-rbind(results, c(sites[j], 'NA'))}
      else {
        f.results<-var.test(as.vector(unlist(sitej[2])), as.vector(unlist(allotherj[2])), alternative = "two.sided")
        results<-rbind(results, c(sites[j], f.results$p.value))}
      ## to perform a chi squared test
      j<- j + 1

    }

    ## Outputting the results for variable
    ## with details of frequencies
    j<-1		##over the sites
    blank<-rep("",4)
    output<-data.frame(matrix(nrow=0,ncol=4))
    while(j<=length(data.sites)){
      site<-rep(results[j+1,1], dim(data.sites[[j]])[1])
      var.sites<-round(as.numeric(data.sites[[j]][,2]),2)
      var.allother<-round(as.numeric(data.allother[[j]][,2]),2)
      Fstat<-round(var.sites/var.allother, 2)
      outputj<-cbind(site, var.sites,var.allother, Fstat)
      output<-rbind(output, outputj, c("p-value:", round(as.numeric(results[j+1,2]),3), "",""))
      j<- j + 1
    }

    print(knitr::kable(output,caption=paste(title, names(data)[i]),col.names=c("Site", "Site Varaince", "All Other Variance",  "F-Statistics"),row.names = FALSE,"simple",table.envir = "table"))
    no.sites.checked[i-1]<- dim(subset(results, results[,2]!="Not enough observations"))[1] - 1 ## number of sites checked is the number which have a p value minus 1 (the header)
    i<- i + 1
  }

  ## Outputting data on the variables checked etc.
  output2<-c("variable", "Sites checked")
  i<-2
  while(i<=dim(data)[2]){
    outputtemp<-c(names(data)[i], no.sites.checked[i-1])
    output2<-rbind(output2, outputtemp)
    i<- i + 1
  }

  print(knitr::kable(head(output2[, 1:2]), row.names =FALSE,caption = title,"simple"))
}
