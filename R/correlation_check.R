#' Perform Correlation Check
#'
#' The function is to compare correlations for multiple continuous variables for different sites.
#' @importFrom stats fisher.test
#' @import gplots
#' @param data This should be a data frame the first column should contain the site number the remaining columns should have the variables to be tested.
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @param min.obs This is the minimum number of observations a site can have and still be tested.
#' @export
#' @returns figures with correlation check outputs
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' correlation_check(lb_mout1,title='',min.obs=3)
#' }

correlation_check<-function(data, title='', min.obs){

  data2<-data		## A new version of the data with only the variables to be checked included
  removed.vars<-0	## A vector to store any variables which are removed
  sds<-apply(data[,-1], 2, sd, na.rm=TRUE)		##Finding the SDs of the variables

  if(min(sds)<1e-05){			## if you find a small SD..
    i<-2		## Moves over each variable at a time
    j<-1		## Used to store the removed variables
    while(i<=dim(data2)[2]){
      sd<-sd(data2[,i], na.rm=TRUE)
      if(sd>=1e-05) i<- i + 1
      if(sd<1e-05){
        removed.vars[j]<- names(data2)[i]
        data2<- data2[,-i]
        j<- j + 1
      }
    }
  }



  #####################################################################
  ##### 2. Splitting the data up by site, finding the correlations and the d value

  data2<- data2[order(data2[,1]),]
  corr.all.data<- as.matrix(cor(data2[,-1],use="pairwise.complete.obs"))	## correlation matrix for all the data
  site.mat<- list()			## a list to store the data for each site
  corr.site.mat<-list()		## a list to store the correlation matrices
  diff.site.mat<-list()		## a list to store the differences between the sites correlation matrix and the matrix for the rest of the data.
  sites<-unique(data2[,1])	## a list of all the site numbers
  removed.sites <- 0

  i<-1		## We will move over the site numbers and add them to the list i is the position of the site number in the list of sites
  j<-1		## j is the position of the site in the site list.
  k<-1 		## k will keep track of the removed sites
  while(i<=length(sites)){
    site<-subset(data2, data2[,1]==sites[i])
    if(dim(site)[1]>=min.obs){		##only add the site to the list if there are more than min.obs observations.
      site.mat[[j]]<- site
      corr.site.mat[[j]]<- as.matrix(cor(site[,-1],use="pairwise.complete.obs"))
      diff.site.mat[[j]]<- as.matrix((corr.site.mat[[j]] - corr.all.data)^2)
      j<- j + 1
    }
    if(dim(site)[1]<min.obs){		##If there are not enough patients store the sites.
      removed.sites[k]<-sites[i]
      k<- k + 1
    }

    i<- i + 1
  }


  #####################################################################
  ##### 3. Run simulations to find a p-value for each site

  i<-1		##This will be run over the sites. i.e. the ith matrix in site.mat
  d.sum<-0	##A vector to store the sums of the differences for each site.
  p<-0
  while(i<=length(site.mat)){
    output<-paste("Testing site:", i, "of", length(site.mat))		##This is output as the program runs.
    ##This loop can take a while so it is nice to
    print(output)									##know the program is still working.
    d.sum = sum(diff.site.mat[[i]])
    greater.than.site.d<-0								##This will count how many simulated (pseudo) sites have
    ##a d-value greater than site i.
    j<-1											##j represents the number of simulated sites
    while(j<=5000){									##we create 5000 simulations.
      no.obs<- dim(site.mat[[i]])[1]					##The number of patients in a site.

      ##randomly choosing this number of patients from all of the patients in the original data
      pseudo.rows<-sample(rep(1:dim(data2)[1]), no.obs, replace=FALSE, prob=NULL)		##This chooses row numbers
      pseudo.site<-data2[pseudo.rows,]									##this makes a new site out of the chosen rows
      corr.pseudo.site = as.matrix(cor(pseudo.site[,-1],use="pairwise.complete.obs"))	##finding the correlation matrix for the pseudo site
      diff.pseudo.site = as.matrix((corr.pseudo.site - corr.all.data)^2)			##finding the difference between this and the correlation matrix for all of the data
      d.sum.pseudo.site= sum(diff.pseudo.site)								##finding the d-value
      if(is.na(d.sum.pseudo.site)==FALSE){								##If we have zero SD anywhere this will be NA so we need to create another site
        if(d.sum.pseudo.site>d.sum) greater.than.site.d<- greater.than.site.d + 1	##If it isn't NA we look at if it is bigger than the ith site. If it is we count it.
        j<- j + 1
      }
    }
    p[i]<- greater.than.site.d/5000		##We use the number of pseudo sites (converted to a percentage) that were more extreme to create a p-value
    i<- i + 1						##then test the next site
  }


  #####################################################################
  ##### 4. Grey scale plots

  # A function which will produce a grey scale plot for you chosen site "site"

  grey.scale.plot <- function(site, cor.site, no.patients, p) {

    # replacing the top triangle with -1 correlations so they will appear white....

    r<-1
    while(r<=dim(cor.site)[1]){
      c<- r + 1
      while(c<=dim(cor.site)[1]){
        cor.site[r,c]<--1
        c<-c + 1
      }
      r<- r+ 1
    }

    n.rows<-dim(cor.site)[1]
    n.cols<-dim(cor.site)[2]

    ledge<- paste("p=",p, sep="")
    title <- paste("Site ", site , " (", no.patients, "patients)")
    image(x=1:n.rows/2, y=1:n.cols, cor.site, axes=FALSE, xlab="", ylab = "", frame.plot=TRUE,col = colorpanel(20, "white", "grey10"), main=title)
    novars<-dim(cor.site)[2]
    legend(0.5,n.rows, ledge)
  }

  # Nine plots to a grid - if you wish to alter this to have more/fewer plots on each grid change the bits of code shown.

  total.plots<- length(corr.site.mat)
  number.grids<-ceiling(total.plots/9)		##this sets the number of grids of plots as the number of multiples of 9

  i<-1
  j<-1
  while(i<=number.grids){

    site1<-site.mat[[j]][1,1]
    j2<- min(j+8, total.plots)
    site2<- site.mat[[j2]][1,1]
    #   plot.title<-paste("Correlation_", title, "_",  , "_", date, "plot_sites_", site1, "-", site2, ".wmf", sep="")
    #dev.new()
    par(mfrow=c(3,3))	## tells R how many plots in each plotting window, in this case 9 (3x3).
    while(j<=j2){
      grey.scale.plot(site.mat[[j]][1,1], corr.site.mat[[j]], dim(site.mat[[j]])[1], p[j])
      j<- j + 1
    }
    i<- i + 1
  }


  #####################################################################
  ##### 5. Outputting info on the data which has been checked and details of the formal test

  #Info for each site checked:
  output<-c("Site", "d-value", "Number of patients", "p-value")
  i<-1
  while(i<=length(site.mat)){
    output<-rbind(output, c(site.mat[[i]][1,1], round(sum(diff.site.mat[[i]]),2), dim(site.mat[[i]])[1], p[i]))
    i<-i+1
  }
  knitr::kable(output, row.names = FALSE,caption = "","simple")

  checknames<-c("Number variables checked:","Number of variables removed:","Number Sites:","Number Sites checked:")
  if(removed.vars[1]!=0) vars.removed<-length(removed.vars)
  if(removed.vars[1]==0) vars.removed<-"None"
  check<-c(dim(data2)[2]-1,vars.removed,length(sites),length(sites) - length(removed.sites))

  checked<-cbind(checknames, check)
  knitr::kable(checked, caption = "","simple",row.names = FALSE)
}

