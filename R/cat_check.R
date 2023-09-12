#' Perform Categorical Check
#'
#' The function is to investigate categorical variables. It compares the proportion of patients in each level in a site to the proportions found in all other sites.
#' @importFrom stats fisher.test
#' @param data A data frame with the site name/number (can be string or numeric) in the first column and any categorical variables to check in the following columns.
#' @param test 'Chisq' or 'Fisher', choice of test method
#' @param title This provides the prefix of the title for table. Default is empty.
#' @export
#' @returns table with categorical check outputs
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' cat_check(data,test,title='')
#' }

cat_check <- function(data,test,title=''){
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
      cats<-unique(data.var[,2])		## all of the values of the categorical variable
      freqsites<-0	##store the frequencies within the site
      freqother<-0	##store the frequencies within all other sites

      k<-1

      while(k<=length(cats)){
        sitek<-subset(sitej, sitej[,2]==cats[k])
        otherk<-subset(allotherj, allotherj[,2]==cats[k])
        freqsites[k]<-dim(sitek)[1]
        freqother[k]<-dim(otherk)[1]
        k<- k + 1
      }
      data.sites[[j]]<-cbind(cats, freqsites)
      data.allother[[j]]<-cbind(cats, freqother)

      ## Now check there are enough observations in each level<-cbind(cats,freqsites)
      ## to perform a fisher's exact squared test
      if (test=='Fisher' ){
        fisher.mat<-cbind(freqsites, freqother)
        if (dim(fisher.mat)[1]<=1 & dim(fisher.mat)[2]<=2){results<-rbind(results, c(sites[j], 'NA'))}
        else {
          fisher.results<-fisher.test(fisher.mat)
          results<-rbind(results, c(sites[j], fisher.results$p.value))}
      }

      ## to perform a chi squared test
      if (test=='Chisq') {
        chi.mat<-cbind(freqsites, freqother)
        chi.results<-chisq.test(chi.mat)
        if(max(chi.results$expected)<5) results<- rbind(results,c(sites[j], "Not enough observations"))	##if there aren't enough observations
        if(max(chi.results$expected)>=5){
          results<-rbind(results, c(sites[j], chi.results$p.value))
        }
      }
      j<- j + 1

    }

    ## Outputting the results for variable
    # print(knitr::kable(results,caption=' '))
    ## with details of frequencies
    j<-1		##over the sites
    blank<-rep("",6)
    output<-data.frame(matrix(nrow=0,ncol=6))
    while(j<=length(data.sites)){
      site<-rep(results[j+1,1], dim(data.sites[[j]])[1])
      total.sites<-sum(as.numeric(data.sites[[j]][,2]))
      total.allother<-sum(as.numeric(data.allother[[j]][,2]))
      percentsites<-round((as.numeric(data.sites[[j]][,2])/total.sites)*100, 2)
      percentother<-round((as.numeric(data.allother[[j]][,2])/total.allother)*100, 2)
      outputj<-cbind(site, data.sites[[j]], percentsites, data.allother[[j]][,2], percentother)
      output<-rbind(output, outputj, c("p-value:", round(as.numeric(results[j+1,2]),3), "","", "", ""))
      j<- j + 1
    }

    print(knitr::kable(output,caption=paste(title, names(data)[i]),col.names=c("Site", "Group", "Site frequency", "Site percent", "All other frequency", "All other percentage"),row.names = FALSE,"simple",table.envir = "table"))
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

