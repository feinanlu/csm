#' Perform Digit Preference Check
#'
#' The function is to check the existence of digit preference in each site for continuous variables.
#' @importFrom stats fisher.test
#' @import ggplot2
#' @param data This should be a dataframe with the site number in the first column followed by the continuous measurements to check
#' @param digit leading or tailing. This tells the program whether to look at the leading (first) or tailing (last) digit of each value. Note: Benford's law only works with leading digits so if Benford is set as TRUE the value of digit will be overridden and taken as leading
#' @param benford TRUE or FALSE. If this is set as TRUE, then the frequency of the leading digits in each site will be compared to Benford's distribution. If this is set as FALSE then the frequency of the leading or tailing digits in each site will be compared to the frequency in all of the other sites put together
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @export
#' @returns Table with digit preference check result along with calculated p-values
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' digit_preferece(data,"tailing",FALSE,'title')
#' }

digit_preferece<-function(data, digit, benford,title=''){
  if(benford==TRUE) digit<-"leading"  ##Benford should use leading digits so override whatever is entered for digit
  ##### Combine the variables to make one variable to test
  data2<-data[,1:2]
  names(data2)[2]<-"var"
  i<-3
  while(i<=dim(data)[2]){
    names(data)[i]<-"var"
    data2<-rbind(data2, data[,c(1,i)])
    i<- i + 1
 }

##### removing missing obs
  data2<- subset(data2, is.na(data2[,2])==FALSE)
##### removing negative obs
  data2<-subset(data2, data2[,2]>=0)


##### find the leading or tailing digit
  data2[,3]<-as.character(data2[,2])

  if(digit=="leading") data2[,4]<-substr(data2[,3], 1,1)
  if(digit=="tailing"){
    data2[,4]<-nchar(data2[,3])
    data2[,5]<-substr(data2[,3],data2[,4], data2[,4])
    data2<- data2[,-4]
  }


  ##### If we are using the leading digit
  ##### we need to account for values that start with zero (i.e. 0.)

  if(digit=="leading") {
    data2zeros <-subset(data2, data2[,4]=="0" | data2[,4]==".") ##the subset of the data where the leading digit is a zero
    data2<- subset(data2, data2[,4]!="0" & data2[,4]!=".")	##the subset which isn't
    i<-2										##looping over the characters of the value starting at 2
    while(dim(data2zeros)[1]>0){						##loop continues until all of the zeros are gone
      data2zeros[,5]<-substr(data2zeros[,3], i,i)		##looking at the next character
      data2zeros[,4]<-data2zeros[,5]

      data2nonzeros<-subset(data2zeros, data2zeros[,4]!="0" & data2zeros[,4]!=".")	##finding the ones that are non now zero
      data2nonzeros<-data2nonzeros[,-5]								##get rid of the extra column
      data2<-rbind(data2, data2nonzeros)								##add to the main non zero data frame

      data2zeros <-subset(data2zeros, data2zeros[,4]=="0" | data2zeros[,4]==".")	##what is left that is zero?
      i<- i + 1
    }
  }

  data2[,4]<- as.numeric(data2[,4])		##set back as numeric

  ##### order the data by site
  data2<-data2[order(data2[,1]),]

  ##### Now split by site and find the frequencies of each leading/tailing digit

  sites<-unique(data2[,1])
  site.data<-list()		## a list in which to store the data for each site
  all.other.data<-list()	## a list in which to store the data for all other sites

  i<-1		##will run over the sites

  while(i<=length(sites)){
    sitei<-subset(data2, data2[,1]==sites[[i]])
    all.otheri<-subset(data2, data2[,1]!=sites[[i]])

    ##### find the frequencies of each of the leading/tailing digits
    if(digit=="leading") digits<-rep(1:9)
    if(digit=="tailing") digits<-rep(0:9)

    freq1<-0	##To store the frequencies
    freq2<-0

    j<-1
    while(j<=length(digits)){
      sitej<-subset(sitei, sitei[,4]==digits[j])
      freq1[j]<-dim(sitej)[1]

      all.otherj<-subset(all.otheri, all.otheri[,4]==digits[j])
      freq2[j]<-dim(all.otherj)[1]

      j<- j + 1
    }
    site<-rep(sitei[1,1], length(digits))
    site.data[[i]]<-cbind(site,digits, as.numeric(freq1))
    all.other.data[[i]]<-cbind(site, digits, as.numeric(freq2))
    i<- i + 1
  }

##### Deciding which sites have enough patients to be tested and outputting the results:
## Creating a matrix in which to store the results data:

results<-cbind("site number", "p-value")
i<-1		##over the sites
while(i<=length(site.data)){
    if(benford==TRUE){		##we actually want the proportion expected if it followed a benford distribution
      benford.prob<-log10(1 + (1/as.numeric(all.other.data[[i]][,2])))
      chi.results<-chisq.test(as.numeric(site.data[[i]][,3]), p=	benford.prob)
    }
    if(benford==FALSE){
      chi.mat<-cbind(as.numeric(site.data[[i]][,3]), as.numeric(all.other.data[[i]][,3]))
      chi.results<-chisq.test(chi.mat)
    }
    resultstemp<-c(site.data[[i]][1,1], chi.results$p.value)
    results<-rbind(results, resultstemp)
    i<- i + 1
}

  #####outputting results.
  if(benford==FALSE) filename<-paste(title," DIGIT_PREF_", digit, "_results", sep="")
  if(benford==TRUE) filename<-paste(title," BENFORD_results" ,sep="")

  print(knitr::kable( results, row.names = FALSE,caption = filename,"simple"))

  ## With details of frequencies
  i<-1		##over the sites
  output<-c("Site", "Digit", "Site frequency", "Site percent", "All other frequency", "All other percentage")
  blank<-rep("",6)
  while(i<=length(site.data)){
    site<-site.data[[i]][,1]
    total.sites<-sum(as.numeric(site.data[[i]][,3]))
    total.allother<-sum(as.numeric(all.other.data[[i]][,3]))
    percentsites<-round((as.numeric(site.data[[i]][,3])/as.numeric(total.sites))*100, 2)
    percentother<-round((as.numeric(all.other.data[[i]][,3])/as.numeric(total.allother))*100, 2)
    outputi<-cbind(site, site.data[[i]][,2:3], percentsites, all.other.data[[i]][,3], percentother)
    output<-rbind(output, outputi, c("p-value:", round(as.numeric(results[i+1,2]),3), "","", "", ""))
    i<- i + 1
  }

  if(benford==FALSE) filename<-paste(title," DIGIT_PREF_", digit, "_results_detail",   sep="")
  if(benford==TRUE) filename<-paste(title," BENFORD_results_detail",  sep="")

  knitr::kable( output, row.names = FALSE,caption = filename,"simple",col.names=NULL)
}
