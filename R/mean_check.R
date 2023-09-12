#' Perform Mean Check
#'
#' The function is to investigate multiple continuous variables. It compares mean value for each site and visit using face plots.
#' @param data A data frame with the subject ID in the first column; site ID in the second column; VISIT information in the third column; any continuous variables to check in the following columns.
#' @param min minimum number of observations a site can have and still be tested.
#' @export
#' @returns figures chernoff face plots
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' mean_check(data,min=2)
#' }



mean_check<- function(data,min){


  data<-replace(data, data<0, NA)
  ## Order by site
  data<-data[order(data[,2]),]



  ## Then remove sites and visit with too few participants:
  removed.sites<-c("Site", "Number of participants")	## somewhere to store the sites with small numbers of participants which have been removed.
  sites<-unique(data[,2])
  i<-1		##to be used in the loop over the site numbers
  while(i<=length(sites)){
    sitei<-subset(data, data[,2]==sites[i])
    patsi<-unique(sitei[,1])	##a list of the participants in the site
    if(length(patsi)<min) {
      removed.sites<-rbind(removed.sites, c(sitei[1,2], length(patsi)))
      data<-subset(data, data[,2]!=sites[i])
    }
    i<- i + 1
  }

  ## Now look at each variable and find the means by site and visit

  visitlist<-unique( data[,3])
  sitelist<-unique( data[,2])

  data[,2]<-paste(data[,2],data[,3],sep=' ')

  sitevisit<-unique(data[,2])
  mean.mat<- sitevisit		##matrix to store the means
  i<-4 ##loop over the variables and start at column 3 as 1 and 2 are the id and site.
  while(i<=dim(data)[2]){
    j<-1 ##loops over the sites
    meansi<-0	##stores the means of variable i for each site (included)
    while(j<=length( sitevisit)){
      sitej<-subset(data, data[,2]==sitevisit[j])
      meansi[j]<-mean(sitej[,i], na.rm=TRUE)
      j<- j + 1
    }
    mean.mat<- data.frame(mean.mat, meansi)
    names(mean.mat)[i-2]<-names(data)[i]
    i<- i + 1
  }

  mean.mat<- mean.mat[,-1]
  label.faces<- sitevisit
  output<-faces(mean.mat, labels= label.faces, face.type=1,nrow.plot=3,cex=0.7,print.info = TRUE,plot.faces = FALSE,main=title)
  n.row<-5
  n.col<-5
  n.bucket.sitelist<-ceiling(length(sitelist)/n.row)
  n.bucket.visitlist<-ceiling(length(visitlist)/n.col)

  for (ns in 1:n.bucket.sitelist) {
    for (nv in 1:n.bucket.visitlist) {
      n.site.plot<-ifelse(ns<n.bucket.sitelist,n.row,length(sitelist)-n.row*(ns-1))
      n.visit.plot<-ifelse(nv<n.bucket.visitlist,n.col,length(visitlist)-n.col*(nv-1))
      # dev.new()
      par(mfrow=c(n.site.plot,n.visit.plot))
      par(mar=c(1,1,1,1))
      for (s in (n.row*(ns-1)+1):(n.row*(ns-1)+n.site.plot)){
        for (v in (n.col*(nv-1)+1):(n.col*(nv-1)+n.visit.plot)){
          x<-paste(sitelist[s],visitlist[v],sep=' ')
          if (x %in% names(output$faces)){
            j<-1
            while (j <=length(sitevisit)){

              if (names(output$faces[j])==x) {
                output1<-output
                output1[1][1][1]<-NA
                output1$faces[1]<-output$faces[j]
                plot(output1,width =.8, height = .8,labels= label.faces[j],cex=.7,face.type=1)

                j<-j+1
              } else {j<-j+1}
            }
          }
          else {plot(0,type='n',axes=FALSE,ann=FALSE)}
        }
      }
    }
  }
}




