#' Perform Outlier Check for AE
#'
#' The function is to check the existence of outliers accounting for time (/month) and number of subjects (/person) in each site for continuous variables.
#' @importFrom stats fisher.test
#' @import ggplot2
#' @param data0 This should be an AE dataframe with the site number in the first column; Subject ID in the second column; Each row represents a different AE.
#' @param data1 This should be an subject level dataframe with the site number in the first column; Subject ID in the second folumn; patient's enrollment/first dose date in %m/%d/%Y format in the third column; patient's last date of AE collection in %m/%d/%Y format in the forth column; Each row represents a different subject.
#' @param n the number of SD (or IQR) from the mean that values can fall and be considered extreme (outliers) OR (for the Mahalanobis distance only) used to find the critical value on the Chi squared distribution. i.e. for the top 2.5% n should be set as 0.975. For percentage method, it shows outliers fall outside of top and lower certain percentage values,  i.e. for the top 2.5% n should be set as 0.975.
#' @param method should be one of 5 options: Select from c("sd", "grubbs","euclid","mahal" ,"IQR","PCT"). "sd" - the sd method chooses value which lies more than n SDs from the mean as extreme； "grubbs" - very similar to the SD method but here, only the most extreme value is chosen, it is then removed and the mean and sd recalculated. The process continues until all extreme values have been found. The idea is that the most extreme values won't mask smaller extreme values by inflating the mean and sd. "euclid" - finds multivariate outliers using the Euclidian distance. "mahal" - finds multivariate outliers using the Mahalanobis distance. "IQR" - to be used with non-normally distributed data. Outliers are points which lie more than n times the IQR from the median. PCT - Percentage method shows outliers fall outside of top and lower certain percentage values.
#' @param var.name should be a variable name with no space or special character in between. Default is 'AE_Rate'
#' @param title This provides the prefix of the title for each plot. Default is empty.
#' @export
#' @returns outliers for event incidence data
#' @editor Feinan Lu
#' @editDate 2022-11-16
#' @examples
#' \dontrun{
#' outlier_aedata(data0,data1,n=.95, method='PCT',var.name='AE_Rate',title='')
#' }

outlier_aedata<-  function(data0,data1,n, method,var.name='AE_Rate',title='')
{
  colnames(data0)[1]<-'SITEID'
  colnames(data0)[2]<-'SUBJID'
  colnames(data1)[1]<-'SITEID'
  colnames(data1)[2]<-'SUBJID'
  colnames(data1)[3]<-'EXSTDTC'
  colnames(data1)[4]<-'LASTDTC'
  options(dplyr.summarise.inform = FALSE)
  data1$AEOTT<-as.Date(data1$LASTDTC,format="%m/%d/%Y")-as.Date(data1$EXSTDTC,format="%m/%d/%Y")+1

  data2<-data0 %>%
    group_by(SITEID,SUBJID) %>%
    summarise(PT_TEAE_Incidence_Rate = n())

  data3<-merge(data1,data2,all.x=TRUE,by=c('SITEID','SUBJID'))


  data4<-data3 %>%
    group_by(SITEID) %>%
    summarise(n1=sum(PT_TEAE_Incidence_Rate,na.rm = TRUE),n2=sum(AEOTT,na.rm = TRUE))  %>%
    mutate(varname = 30.4375*n1 /as.numeric(n2),SITEID2=1) #incidence per person month#

  data<-data.frame(dplyr::select(data4,c(SITEID,SITEID2,varname)))
  data$varname<-as.numeric(data$varname)
  data$SITEID<-as.character (data$SITEID)
  colnames(data)[length(data)] <- var.name

  outlier_check(data, n = n, method = method,  normal.plot = FALSE,title)
}
