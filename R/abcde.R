library(qwraps2)
#' A Summary Table Function
#'
#' This function allows you to generate a summary table of your data.
#' @param xx Data of which a summary table is needed.
#' @param ref A parent data, of which xx is a subset; useful when do stratified analyses
#' @keywords Summary Table
#' @export
#' @examples
#' summary_tab()
summary_tab = function(xx, ref = NULL){
  cat("N =", dim(xx)[1], "\n")
  n = dim(xx)[2]
  namesss = colnames(xx)

  outp = NULL
  for(i in 1:n){
    na.ct = sum(is.na(xx[i]))

    if(is.numeric(xx[, i])){
      #######################################
      #### change to others, eg mean_sd() ###
      stattt = median_iqr(xx[, i], na_rm = T, show_n = "never")
      #######################################
      if(na.ct > 0)
        stattt = paste(stattt, "; NA = ", na.ct, sep = "")
      outp = rbind(outp, c(namesss[i], "", stattt))
    }

    if(is.character(xx[, i]) | is.factor(xx[, i]) ){
      foo = table(xx[, i])
      if(!is.null(ref))
        foo = table(ref[, i])
      foo.nm = names(foo)
      for(j in 1:length(foo)){
        #######################################
        #### change to others, eg n_perc0() ###
        stattt = n_perc(xx[, i] == foo.nm[j], na_rm = T)
        #######################################
        if(j == 1)
          outp = rbind(outp, c(namesss[i], foo.nm[j], stattt))
        else
          outp = rbind(outp, c("", foo.nm[j], stattt))
      }
      if(na.ct > 0)
        outp = rbind(outp, c("", "NA", na.ct))
    }
  }
  colnames(outp) = c("Variable", "Level", paste("Median (IQR) or count (%)", " (N = ", dim(xx)[1],")", sep=""))
  outp
}
