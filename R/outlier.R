#' Perform Outlier Check
#'
#' The function is to detect outlier(s) for continuous variables.
#' @importFrom stats fisher.test
#' @param data This should be a data frame with the id in the first column,	the site name/number in the second column, followed by the variables to check in the remaining columns.
#' @param n the number of SD (or IQR) from the mean that values can fall and be considered extreme (outliers) OR (for the Mahalanobis distance only) used to find the critical value on the Chi squared distribution. i.e. for the top 2.5% n should be set as 0.975. For percentage method, it shows outliers fall outside of top and lower certain percentage values,  i.e. for the top 2.5% n should be set as 0.975.
#' @param method should be one of 5 options: Select from c("sd", "grubbs","euclid","mahal" ,"IQR","PCT"). "sd" - the sd method chooses value which lies more than n SDs from the mean as extreme； "grubbs" - very similar to the SD method but here, only the most extreme value is chosen, it is then removed and the mean and sd recalculated. The process continues until all extreme values have been found. The idea is that the most extreme values won't mask smaller extreme values by inflating the mean and sd. "euclid" - finds multivariate outliers using the Euclidian distance. "mahal" - finds multivariate outliers using the Mahalanobis distance. "IQR" - to be used with non-normally distributed data. Outliers are points which lie more than n times the IQR from the median. PCT - Percentage method shows outliers fall outside of top and lower certain percentage values.
#' @param normal.plot This declares whether a normal probability plot should be drawn for each variable. It should be set as TRUE if plots are required and FALSE if not.
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @export
#' @returns outlier for continuous data
#' @editor Feinan Lu
#' @editDate 2023-08-01
#' @examples
#' \dontrun{
#' outlier_check(data, n = 2, method = "sd",  normal.plot = FALSE,"Plot 1. ")
#' }


outlier_check<-  function(data, n, method, normal.plot,title=''){

  ## rename the id "ID" so it can be merged later
  names(data)[1]<-"ID"

  ## First create a function to make the plots. This will be used
  ## for all the methods
  ## 3x3 plotting window
  plot.outliers<-function(data, title){
    if (length(which(!is.na(data[,2])))>0){
      x<-ggplot(data, aes(x = data[,1], y = data[,2],color= as.character( data[,4])))+geom_point()+
        labs(x=names(data)[1],y=names(data)[2],title=title)+
        scale_color_manual(values =c("red",' '), labels = c("Outlier", " "),name=" ")+
        geom_hline(aes(yintercept = cut.off.lower))+
        ggplot2::annotate("text",1, cut.off.lower , hjust=-.2,vjust=-.1,label =round(cut.off.lower,2) , color = "darkred") +
        geom_hline(aes(yintercept = cut.off.upper))+
        ggplot2::annotate("text",1, cut.off.upper , hjust=-.2,vjust=-.1,label =round(cut.off.upper,2 ), color = "darkred") +
        theme(plot.title=element_text(size=10))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

      print(x)}
    else {}
  }

  ##### Univariate options
  ## SD method

  if(method=="sd"){
    ## create a data frame to store the outliers.
    outliers.data<-c("id", "site", "value", "mean", "sd", "variable")

    i<-3	##run a loop over the variables (each will have its own plot)
    while(i<= dim(data)[2]){
      data.var<- data[, c(1,2, i)]				##just the id and the ith variable
      # data.var<-subset(data.var, data.var[,3]>=0) 	##removing dummy values which are less than zero
      data.var<-na.omit(data.var)				##removing missing obs


      mean.var<-mean(as.numeric(unlist(data.var[,3])), na.rm=TRUE) 		##finding the mean
      sd.var<-sd(data.var[,3], na.rm=TRUE) 		##finding the SD
      cut.off.lower<- mean.var  - n*sd.var		##finding the lower cut off (values less than this are outliers)
      cut.off.upper<- mean.var + n*sd.var			##finding the upper cut off (values greater than this are outliers)

      outliers.var<-subset(data.var, data.var[,3]<cut.off.lower | data.var[,3]>cut.off.upper)

      outliers.mat<-outliers.var
      outliers.mat[,4]<-rep(mean.var, dim(outliers.var)[1])
      outliers.mat[,5]<-rep(sd.var, dim(outliers.var)[1])

      varname<-names(outliers.var)[3]
      outliers.mat[,6]<-rep(varname, dim(outliers.var)[1])	 ## storing the variable name to be output with the list of outliers
      names(outliers.mat)[3]<-"Variable"

      if(dim(outliers.mat)[1]>0){		##if outliers are found...
        outliers.data<-rbind(outliers.data, outliers.mat)
        outlier<- rep(1, dim(outliers.var)[1])
        outliers.var<-data.frame(outliers.var, outlier)
        outliers.var<-outliers.var[,-2]
        data.var<- merge( data.var, outliers.var, all.x=TRUE, by=c("ID", varname))
      }

      if(dim(outliers.mat)[1]==0) {     	## if outliers are not found
        data.var[,4]<-rep(NA, dim(data.var)[1])
        data.var<- data.var[, c(1,3,2,4)]  ##switching the columns so it is id, variable, hospital, outlier indicator (as it would be if outliers were found and it was merged as above)
      }

      date<- Sys.Date()
      title1<-paste(title ,varname,"OUTLIERS (SD METHOD, ", n,"SD)", sep=" ")
      par(mfrow=c(2,1))
      plot.outliers(data.var, title1)
      i<- i + 1
    }
  }


  ## Grubb's method:

  if(method=="grubbs"){

    ## create a data frame to store the outliers.
    outliers.data<-c("id", "site", "value", "mean", "sd", "variable")

    i<-3 	## over the variables
    while(i<=dim(data)[2]){
      data.var<- data[, c(1,2, i)]				## just the id and the ith variable
      data.var<-subset(data.var, data.var[,3]>=0) 	## removing dummy values which are less than zero
      data.var<-na.omit(data.var)				## removing missing obs

      data.var2<-data.var		## We will be removing outliers as we find them
      ## so I need a second copy of the data
      outliers<-TRUE
      outliers.var<-rep(0,4)        ## to temporarily store the outliers to plot (assumes that there is no person with id 0

      while(outliers==TRUE){	## stop the loop when there are no more outliers detected

        mean.var<-mean(data.var2[,3], na.rm=TRUE) ##finding the mean
        sd.var<-sd(data.var2[,3], na.rm=TRUE) 		##finding the SD
        cut.off.lower<- mean.var  - n*sd.var		  ##finding the lower cut off (values less than this are outliers)
        cut.off.upper<- mean.var + n*sd.var			  ##finding the upper cut off (values greater than this are outliers)

        # all outliers
        outliers.temp<-subset(data.var2, data.var2[,3]<cut.off.lower | data.var2[,3]>cut.off.upper)

        if(dim(outliers.temp)[1]!=0){
          # the most extreme:
          max<-max(outliers.temp[,3])
          outliers.temp<-subet(outliers.temp, outliers.temp[,3]==max)
          # adding to the outliers already found
          outliers.var<-rbind(outliers.var, outliers.temp)
          # removing the outliers from the data before we check again
          data.var2<-subset(data.var2, data.var2[,3]!=max)
        }

        # if there were none we want to stop the loop
        if(dim(outliers.temp)[1]==0) outliers<-FALSE


      }


      ##if outliers are found...

      if(is.null(dim(outliers.var))==FALSE){
        outliers.var<- subset(outliers.var, outliers.var[,1]!=0)
        outliers.mat<-outliers.var
        outliers.mat[,4]<-rep(mean.var, dim(outliers.var)[1])
        outliers.mat[,5]<-rep(sd.var, dim(outliers.var)[1])

        varname<-names(outliers.var)[3]
        outliers.mat[,6]<-rep(varname, dim(outliers.var)[1])
        names(outliers.mat)[3]<-"Variable"

        outliers.data<-rbind(outliers.data, outliers.mat)
        outlier<- rep(1, dim(outliers.var)[1])
        outliers.var<-data.frame(outliers.var, outlier)
        outliers.var<-outliers.var[,-2]

        data.var<- merge( data.var, outliers.var, all.x=TRUE, by=c("ID", varname))
      }

      if(is.null(dim(outliers.var))==TRUE){     	## if outliers are not found
        data.var[,4]<-rep(NA, dim(data.var)[1])
        data.var<- data.var[, c(1,3,2,4)]  ##switching the columns so it is id, variable, hospital, outlier indicator (as it would be if outliers were found and it was merged as above)
        varname<-names(data.var)[2]
      }

      title1<-paste(title,"OUTLIERS_GRUBBS_METHOD", varname,sep=' ')
      plot.outliers(data.var, title1)

      i<- i + 1
    }

  }

  ## IQR method

  if(method=="IQR"){
    ## create a data frame to store the outliers.
    outliers.data<-c("id", "site", "value", "median", "IQR", "variable")

    i<-3	##run a loop over the variables (each will have its own plot)
    while(i<= dim(data)[2]){
      data.var<- data[, c(1,2, i)]				##just the id and the ith variable
      data.var<-subset(data.var, data.var[,3]>=0) 	##removing dummy values which are less than zero
      data.var<-na.omit(data.var)				##removing missing observations

      median.var<-median(data.var[,3], na.rm=TRUE) 	##finding the median
      q1.var<-quantile(data.var[,3], probs = c(.25))
      q3.var<-quantile(data.var[,3], probs = c(.75))
      iqr.var<-IQR(data.var[,3], na.rm=TRUE) 		##finding the SD
      cut.off.lower<- q1.var  - n*iqr.var		##finding the lower cut off (values less than this are outliers)
      cut.off.upper<- q3.var + n*iqr.var		##finding the upper cut off (values greater than this are outliers)

      outliers.var<-subset(data.var, data.var[,3]<cut.off.lower | data.var[,3]>cut.off.upper)

      outliers.mat<-outliers.var
      outliers.mat[,4]<-rep(median.var, dim(outliers.var)[1])
      outliers.mat[,5]<-rep(iqr.var, dim(outliers.var)[1])

      varname<-names(outliers.var)[3]
      outliers.mat[,6]<-rep(varname, dim(outliers.var)[1])	 ## storing the variable name to be output with the list of outliers
      names(outliers.mat)[3]<-"Variable"

      if(dim(outliers.mat)[1]>0){		##if outliers are found...
        outliers.data<-rbind(outliers.data, outliers.mat)
        # plotting the data

        outlier<- rep(1, dim(outliers.var)[1])
        outliers.var<-data.frame(outliers.var, outlier)
        outliers.var<-outliers.var[,-2]

        data.var<- merge( data.var, outliers.var, all.x=TRUE, by=c("ID", varname))
      }

      if(dim(outliers.mat)[1]==0) {     	## if outliers are not found
        data.var[,4]<-rep(NA, dim(data.var)[1])
        data.var<- data.var[, c(1,3,2,4)]  ##switching the columns so it is id, variable, hospital, outlier indicator (as it would be if outliers were found and it was merged as above)
      }

      title1<-paste(title,"OUTLIERS_IQR_METHOD",  varname,sep=' ')
      plot.outliers(data.var, title1)

      i<- i + 1
    }
  }

  ## Percentage method, find the top and bottom x%

  if(method=="PCT"){
    ## create a data frame to store the outliers.
    outliers.data<-c("id", "site", "value", "median", "IQR", "variable")

    i<-3	##run a loop over the variables (each will have its own plot)
    while(i<= dim(data)[2]){
      data.var<- data[, c(1,2, i)]				##just the id and the ith variable

      cut.off.lower<- quantile(data.var[,3], probs = (1-n), na.rm = TRUE)		##finding the lower cut off (values less than this are outliers)
      cut.off.upper<- quantile(data.var[,3], probs = (n), na.rm = TRUE)		##finding the upper cut off (values greater than this are outliers)

      outliers.var<-subset(data.var, data.var[,3]<cut.off.lower | data.var[,3]>cut.off.upper)

      outliers.mat<-outliers.var


      varname<-names(outliers.var)[3]
      outliers.mat[,4]<-rep(varname, dim(outliers.var)[1])	 ## sorting the variable name to be output with the list of outliers
      names(outliers.mat)[3]<-"Variable"

      if(dim(outliers.mat)[1]>0){		##if outliers are found...
        outliers.data<-rbind(outliers.data, outliers.mat)
        # plotting the data

        outlier<- rep(1, dim(outliers.var)[1])
        outliers.var<-data.frame(outliers.var, outlier)
        outliers.var<-outliers.var[,-2]

        data.var<- merge( data.var, outliers.var, all.x=TRUE, by=c("ID", varname))
      }

      if(dim(outliers.mat)[1]==0) {     	## if outliers are not found
        data.var[,4]<-rep(NA, dim(data.var)[1])
        data.var<- data.var[, c(1,3,2,4)]  ##switching the columns so it is id, variable, hospital, outlier indicator (as it would be if outliers were found and it was merged as above)
      }

      date<- Sys.Date()
      title1<-paste(title,varname,"OUTLIERS (top and bottom", 100*(1-n),"%)",sep=' ')
      plot.outliers(data.var, title1)

      i<- i + 1
    }
  }

  ##### Multivariate options

  ## Euclidian distance
  if(method=="euclid"){
    d<-rep(0, dim(data)[1])	## will store the d values. d is the sum of the Normalised Euclidian distances
    i<-3		## loop over the variables, starting after the site number

    means<- rep(NA, dim(data)[2]-2)
    while(i<=dim(data)[2]){
      mean<-mean(data[,i], na.rm=TRUE)
      sd<-sd(data[,i], na.rm=TRUE)
      means[i-2] <- mean
      dist<- ((data[,i] - mean)/sd)^2
      ## if the value was missing this will be NA so I need to replace these with zero
      dist[is.na(dist)]<-0
      d<- d + dist	## add on to the sum
      i<- i + 1
    }

    ## put the d value with the main data.

    outliers.mat<- cbind(data[,1], d, data[,2])

    ## Wow we want to see which d values are extreme. Use the SD method.
    colnames(outliers.mat)[1]<-"ID"

    mean.d <- mean(as.numeric(outliers.mat[,2]), na.rm= TRUE)
    sd.d<- sd(outliers.mat[,2], na.rm= TRUE)

    upper.limit<- mean.d + n*sd.d

    extreme.high<- subset(outliers.mat, outliers.mat[,2]>upper.limit)
    extreme.high[,2]<-rep(1, dim(extreme.high)[1])
    extreme.high<-extreme.high[,1:2]

    outliers.mat<- merge(outliers.mat, extreme.high, by="ID", all.x=TRUE)
    colnames(outliers.mat)[2]<-"d"

    title1<-paste(title,"OUTLIERS_EUCLID_METHOD",sep=' ')
    plot.outliers(outliers.mat, title1)

  }

  ## mahalanobis distance
  if(method=="mahal"){
    means<-0
    cov.data<-cov(data[,3:dim(data)[2]], use="pairwise.complete.obs")
    i<-3
    j<-1
    while(i<=(dim(data)[2])){
      means[j]<- mean(data[,i], na.rm=TRUE)
      i<- i + 1
      j<- j + 1
    }
    i<- 1
    m.dist<-0
    while(i<=dim(data)[1]){
      m.dist[i]<-	mahalanobis(data[i,3:dim(data)[2]], means, cov.data)
      i<- i + 1
    }

    df<- dim(data)[2] - 2
    critical.value <- qchisq(n, df)

    outliers.mat<- as.data.frame(cbind(data[,1], m.dist, data[,2]))
    colnames(outliers.mat)[1]<-"ID"
    extremes<- subset(outliers.mat,as.numeric(outliers.mat[,2])>critical.value)
    extremes[,2] <-rep(1, dim(extremes)[1])
    extremes<- dplyr::select(extremes,c('ID','m.dist'))

    outliers.mat<- merge(outliers.mat, extremes, by="ID", all.x=TRUE)
    colnames(outliers.mat)[2]<-"d"
    outliers.mat[,2]<-as.numeric( outliers.mat[,2])
    cut.off.lower<-critical.value
    cut.off.upper<-critical.value
    title1<-paste(title,"OUTLIERS_MAHAL_METHOD_chisq level",n,sep=' ')
    plot.outliers(outliers.mat, title1)

  }

  ##### outputting the outliers found

  # If a univariate method was used:

  if(method=="sd" | method=="IQR" | method=="grubbs"| method=="PCT"){
    date<- Sys.Date()
    if(is.null(dim(outliers.data))==FALSE){
      # filename<-paste(trial.name, "_OUTLIERS_", method, "_" , date, ".txt", sep="")
      # write.table(outliers.data, filename, sep="\t", col.names=FALSE, row.names=FALSE)
      knitr::kable(outliers.data, row.names = FALSE,caption = "","simple")
    }
  }

  # if a multivariate method was used:

  if(method=="euclid" | method=="mahal"){
    date<- Sys.Date()
    outliers.mat<-outliers.mat[,c(1,4)]
    output<- merge(data, outliers.mat, by="ID")
    output<-subset(output, is.na(output[,dim(output)[2]])==FALSE)
    output<- output[,- dim(output)[2]]

    if(dim(output)[1]>0){

      if(method=="euclid"){
        means<- c("Means", "", means, "")	## add in the means if Euclid was used
        output<- rbind(output, means)
      }

      #filename<-paste("OUTLIERS_", method, "_" , date, ".txt", sep="")
      #write.table(output, filename, sep="\t", row.names=FALSE)
      knitr::kable(output, row.names = FALSE,caption = "","simple")
    }
  }


  ##### outputting normal probability plots for each variable (if requested)

  if(normal.plot==TRUE){
    i<-3
    while(i<=dim(data)[2]){
      varname<- names(data)[i]
      title1<-paste(title,"NORMAL_PROB_PLOT", varname,sep=' ')
      win.metafile(title1)
      qqnorm(data[,i], ylab="Standardized Residuals", xlab="Normal Scores", main=names(data)[i])
      qqline(data[,i])

      i<- i + 1
    }
  }

  ##### outputting info on the number of variables and values checked

  number.values<-0
  i<-3
  while(i<=dim(data)[2]){
    number.values<- number.values + dim(subset(data, is.na(data[,i])==FALSE))[1]
    i<- i + 1
  }


  checknames<-c("Number of variables:", "Number of participants:", "Number of values", "Number of outliers")
  if(method=="sd" | method=="IQR" | method=="grubbs"| method=="PCT"){
    if(is.null(dim(outliers.data))==FALSE) numberoutliers<-dim(outliers.data)[1]-1
    if(is.null(dim(outliers.data))==TRUE) numberoutliers<-"None"
  }

  if(method=="euclid"| method=="mahal") {
    if(dim(output)[1]==0) numberoutliers<- "none"
    if(dim(output)[1]>0) numberoutliers<-dim(output)[1]
  }

  check<-c(dim(data)[2]-2, dim(data)[1], number.values, numberoutliers)
  checked<-cbind(checknames, check)

  filename<-paste(title1,"OUTLIERS_VARIABLESCHECKED_", method)
  knitr::kable(checked, caption = "","simple",row.names = FALSE)

}
