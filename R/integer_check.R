#' Perform Integer Check
#'
#' The function is to detect outlier(s) for continuous variables.
#' @importFrom stats fisher.test
#' @import ggplot2
#' @param data This should be in the form of a data frame with the site number in the first column (must be numeric-if a string is given please recode as numeric), the date that the measurement was taken in the second column (in the form dd/mm/yyyy) and the measurements to check in the following columns (i.e. columns 3+). If measurements were taken at different dates please split the data into more than one data frame and run the function on each data frame.
#' @param min.part This is the minimum number of observations a site can have and still be tested. Default is 3.
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @export
#' @returns Plot with number of cumulative integer pattern
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' integers_check(data, min_num=3, title='')
#' }


integers_check<- function(data, min_num=3, title=''){

  ##### First split the data so there is one data frame for each variable
  ##### Sort the data by site

  data<-data[order(data[,1]),]

  data.vars<- list()

  i<-3
  while(i<=dim(data)[2]){
    j <- i-2
    data.vars[[j]] <- data[,c(1,2,i)]
    # remove any missing observations
    data.vars[[j]] <- subset(data.vars[[j]], is.na(data.vars[[j]][,3])==FALSE)
    i<- i + 1
  }

  ##### Now run a loop over the variables:

  # As I go along I will collect info on what has been tested
  total.number.sites<- length(unique(data[,1]))
  variables<- ""		##will store the variable names
  number.sites<-0		##will store the number of sites tested per variable (may vary with missing data)

  i<-1

  while(i<=length(data.vars)){

    variables[i]<-names(data)[i+2]

    data2<- data.vars[[i]]
    data.sites<-list()		##split the data by site and save in here.
    sites<- unique(data2[,1])

    j<-1	## increases for each site
    k<-1	## will be used to put the sites into the list as we only want to include sites with >= min patients

    while(j<=length(sites)){	## over the sites
      sitej<-subset(data2, data2[,1]==sites[j])

      sitej[,4]<-as.numeric(as.Date(sitej[,2], "%d/%m/%Y"))	## create a numeric version of the date

      # Sort by this numeric version
      sitej<- sitej[order(sitej[,4]),]
      ## Create a column to show if the value of was an integer or not
      sitej[,5]<- floor(as.numeric(sitej[,3]))		##find the floor of the value
      sitej[,6]<- ceiling(as.numeric(sitej[,3]) - as.numeric(sitej[,5]))	## find the ceiling of the value- its floor (0=integer, 1=non integer)
      sitej[,7]<- cumsum(as.numeric(sitej[,6]))		## find the cumulative sum of these
      ## make an index to plot.
      sitej[,8]<- rep(1:dim(sitej)[1])

      if(dim(sitej)[1]>=min_num){
        data.sites[[k]]<- sitej[,c(1,7,8)]
        k<- k + 1
      }

      j<- j + 1

    }

    number.sites[i]<- length(data.sites)
    info<-paste("None of the sites have more than ", min_num, " participants", sep="")
    if(number.sites[i]==0) number.sites[i]<-info

    ##### Creating the plots.

    no.plots<- length(data.sites) ##counts the number of plots

    if(no.plots>0){
      ##### This plots up to 9 plots to a window

      plots.remaining<- no.plots

      ##### Program to make the plots:
      integer.plot<- function(datatoplot){
        # 	title<-paste("Site: ", datatoplot[1,1], sep="")
        x<- plot(datatoplot[,3], datatoplot[,2], main=paste("Site: ", datatoplot[1,1],"  ",title,  " ",names(data)[i+2],sep=""), xlab="Subjects", ylab="cumulative n of non-integers",cex.lab = 2)
        #
        #  x<-ggplot(datatoplot, aes(x = datatoplot[,3], y = datatoplot[,2]))+
        #   geom_point()+
        #  labs(x="Patients",y="cumulative frequency of non-integers",title=paste("Site: ", datatoplot[1,1],"  ",title,  " ",names(data)[i+2],sep=""))
        print(x)
      }

      # print(names(data))


      while(plots.remaining>0){
        if(plots.remaining>=9){

          sitestart<-data.sites[[1]][1,1]		##number of the first site
          siteend<-data.sites[[9]][1,1]			##number of the last site
          var.name<- names(data)[i+2]
          # plot.title<-paste("Integers", "_", title,"_",names(data)[i+2], "_", date,"_sites_", sitestart, "-", siteend, ".wmf", sep="")
          # win.metafile(plot.title)
          par(mfrow=c(3,3))		## 3x3 plotting window
          par(bg = "snow")
          par(cex.main=0.8)
          j<-1
          while(j<=9){
            integer.plot(data.sites[[1]])
            data.sites[[1]]<-NULL
            j<- j + 1
          }
          plots.remaining <- plots.remaining - 9

        }

        if(plots.remaining<9){
          sitestart<-data.sites[[1]][1,1]					##number of the first site
          siteend<-data.sites[[plots.remaining]][1,1]			##number of the last site
          # plot.title<-paste("Integers", "_", title,"_",names(data)[i+2], "_", date,"_sites_", sitestart, "_", siteend, ".wmf", sep="")
          # win.metafile(plot.title)
          if(plots.remaining==1) par(mfrow=c(1,1))		## 1x1 plotting window
          if(plots.remaining==2) par(mfrow=c(1,2))		## 1x2 plotting window
          if(plots.remaining==3 | plots.remaining==4) par(mfrow=c(2,2))		## 2x2 plotting window
          if(plots.remaining==5 | plots.remaining==6) par(mfrow=c(2,3))		## 2x3 plotting window
          if(plots.remaining==7 | plots.remaining==8) par(mfrow=c(3,3))		## 3x3 plotting window
          par(cex.main=0.8)
          j<- 1
          while(j<=plots.remaining){
            integer.plot(data.sites[[1]])
            data.sites[[1]]<-NULL
            j<- j + 1
          }
          plots.remaining = 0

        }
      }
    }

    i<- i + 1
  }


  ##### Outputting information on the variables tested:

  output<-cbind(variables, number.sites)
  total.number.sites<-rep(total.number.sites, dim(output)[1])
  output<- cbind(output, total.number.sites)
  # filename<-paste("Integers_variables_tested_", title, "_", date, ".txt", sep="")
  # write.table(output, filename, sep="\t", row.names=FALSE)
  x<-knitr::kable(output, caption = paste(title," Integers_variables_tested" ),"simple",row.names = FALSE)
  print(x)
}
