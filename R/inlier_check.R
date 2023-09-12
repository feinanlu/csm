#' Perform Inlier Check
#'
#' The function is to detect inlier(s) for continuous variables.
#' @importFrom stats fisher.test
#' @import ggplot2
#' @param data This should be a data frame with the id in the first column,	the site name/number in the second column, followed by the variables to check in the remaining columns.
#' @param min.part This is the minimum number of participants a site can have and still be tested. This MUST be at least 2 (though 5 or more is suggested). Default is 3.
#' @param n Number of SDs from the mean a point should lie before being counted as an inlier.
#' @param pervar TRUE or FALSE. Default is TRUE.Pervar is logical and should be set as TRUE if you wish to calculate the average distance from the mean (i.e. the sum of the distances divided by the number of variables used to calculate it) or FALSE if not.
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @export
#' @returns Inlier check plot with inlier identified
#' @editor Feinan Lu
#' @editDate 2023-08-01
#' @examples
#' \dontrun{
#' inlier_check(DM_outlier, min.part=4, n=3, pervar=TRUE)
#' }


inlier_check<- function(data, min.part=3, n, pervar=TRUE,title=''){

  # If min.part<=1 replace with 2 (min.part must be 2 or more to run)
  if(min.part<=1) print("min.part MUST be at least 2, it has been reset as 2")
  min.part<- max(2,min.part)

  # Sort by site number
  data<-data[order(data[,2]),]

  # If pervar is TRUE, find the number of non-missing observations each participant has:
  if(pervar==TRUE) data[,dim(data)[2] + 1]<- apply(data, 1, function(x) sum(!is.na(x))) - 2 ## count the number of non-missing and take away 2 (site and id)

  # Now split by site.
  data.sites<-list()			##each site will be stored in this list
  excluded.sites<-0	 			##will store all excluded sites
  excluded.sites.size<- 0	##stores the size of the excluded site
  sites.excluded<-FALSE		##will be set as TRUE if any sites are excluded

  sites<- unique(data[,2])
  i<-1	#Over sites
  j<-1	#keeps track of the sites that are included.
  k<-1 	#keeps track of the sites that are excluded.

  while(i<=length(sites)){
    sitei<-subset(data, data[,2]==sites[i])			## all participants in site i
    if(dim(sitei)[1]>=min.part){					      ## if there are enough participants save in the site list
      data.sites[[j]]<-sitei
      j<- j + 1
    }

    if(dim(sitei)[1]<min.part){					## if there are no add to the list of excluded sites.
      sites.excluded<-TRUE
      excluded.sites[k]<-sites[i]
      excluded.sites.size[k]<-dim(sitei)[1]
      k<- k + 1
    }
    i<- i + 1
  }

  ##### If any sites have been excluded output:

  if(sites.excluded==TRUE){
    output<-data.frame(excluded_sites=excluded.sites, number_participants= excluded.sites.size)
    knitr::kable(output, row.names = FALSE,caption = "INLIERS_excluded_sites","simple")
  }

  ##### Finding the inliers in sites with enough participants
  # If none of the sites had min.parts participants, stop the function and report this.
  if(length(data.sites)==0){
    output<- paste(title,"All sites have less than ", min.part, " participants")
    print(output)
    return(output)
  }
  i<-1

  # If pervar is TRUE we need to remove the last column (number of non-missing values when calculating the d value sum)
  if(pervar==TRUE) minus<-1
  if(pervar==FALSE) minus<- 0

  while(i<=length(data.sites)){
    means<-apply(data.sites[[i]][3:(dim(data.sites[[i]])[2] - minus)],2, function(x) mean(x, na.rm=TRUE)) 	## means of the variable values in site i
    sds<- apply(data.sites[[i]][3:(dim(data.sites[[i]])[2] - minus)],2, function(x) sd(x, na.rm=TRUE))	 	## sds of the variable values in site i
    data.sitei<- apply(data.sites[[i]][3:(dim(data.sites[[i]])[2] - minus)], 1, function(x) ((x-means)/sds)^2)	## d values for each participant in site i
    d<-apply(data.sitei, 2, function(x) sum(x, na.rm=TRUE))									## sum of the d values
    if(pervar==TRUE) d<- d/data.sites[[i]][,dim(data.sites[[i]])[2]]			## if pervar is TRUE this is divided by the number of variables that were non missing for the patient
    d<- log(d) ## replacing with the log
    mean.d<- mean(d, na.rm=TRUE)									## mean of d
    sd.d<- sd(d, na.rm=TRUE)									## sd of d
    data.sites[[i]]$diff<- d							## adding the d value as another column in the matrix
    data.sites[[i]]$diffind <- ifelse(d>=((mean.d)-(n*sd.d)),NA,1)	 					## and the indicator variable.
    i<- i + 1
  }

  #Table: Output inliers
  dataplot<- data.sites[[1]]
  i<-1
  while(i<=length(data.sites)){
    dataplot<-rbind(  dataplot, data.sites[[i]])
    i<- i + 1
  }
  dataplot<-dataplot[!duplicated(dataplot[,c('SUBJID')]),]

  if (dim(subset(dataplot,dataplot$diffind==1))[1]!=0)
  {
    print(knitr::kable( subset(dataplot,dataplot$diffind==1)[,1:(length(dataplot)-3)], row.names = FALSE,caption = "Inliers","simple"))
  }
  if (dim(subset(dataplot,dataplot$diffind==1))[1]==0)
  {print(paste(title,"No inlier is detected."))}

  sitelist<-unique(subset(dataplot,dataplot$diffind==1)$SITEID)
  # dataplot<-subset(dataplot, dataplot$SITEID %in% sitelist) #to select the site with inliers to plot#
  dataplot$inlier<-ifelse(dataplot$diffind==1,'Inlier','')

  #plot the data#

  for (i in sitelist){
    x<-ggplot(subset(dataplot,dataplot$SITEID==i), aes(x = SUBJID, y =diff,color=inlier) ) +
      geom_point()+
      scale_color_manual(values =c("red",' '), labels = c("Inlier", " "),name=" ")+
      labs(y="log (d)",title=paste(title,"INLIERS: (", n,"SD) ", sep=""))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      facet_wrap(.~ SITEID, ncol=1,scales = "free")
    print(x)}

  ##### outputting info on the data checked:

  #  filename<-paste("INLIERS_DATA_TESTED_",  sep="")
  # paste(vars.tested, filename, sep=" ")
  x0<-paste(title)
  x1<-paste(c("Number of variables: "),length(dataplot)-6,sep=" ")
  x2<-paste(c("Number of sites: "),length(sites),sep=" ")
  x3<-paste(c("Number of participants: "),length(unique(data[,1])) , sep=" ")
  x4<-paste(c("Number of inliers found: "), dim(subset(dataplot,dataplot$diffind==1))[1],  sep=" ")
  x5<-ifelse(pervar=="TRUE",paste(c("log d values were averaged over the number of non-missing variables for each patient")),paste(c("Sum of log d values were calculated for each patient")))
  y<-rbind(x0,x1,x2,x3,x4,x5)
  print( knitr::kable(y,row.names = FALSE,col.names = "",caption = "","simple"))

}


