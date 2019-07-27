
#' A Summary Table Function
#'
#' This function allows you to generate a summary table of your data.
#' @param xx Data of which a summary table is needed.
#' @param ref A parent data, of which xx is a subset; useful when do stratified analyses
#' @keywords Summary Table
#' @export
#' @examples
#' summary_tab()
#' summary_tab(iris)
#'
#'
#'
#'
#'




con_sum00 = function(xx){
  na.ct = sum(is.na(xx))
  ###########################
  aaa = round(quantile(xx, prob = c(.5, 0.25, 0.75), na.rm = T, type=2), 2)
  ############################
  stattt = paste(aaa[1], " (", aaa[2], ", ", aaa[3], ")", sep = "")
  if(na.ct > 0)
    stattt = paste(stattt, "; NA = ", na.ct, sep = "")
  stattt
}


con_summ = function(x, by, test){
  by_cat = unique(by)
  output = c("", "")
  for(vv in by_cat){
    foo = con_sum00(x[by == vv])
    output = c(output, foo)
  }
  all = con_sum00(x)
  output = c(output, all)
  if(test)
    output = c(output,
               paste(signif(t.test(x ~ by)$p.value, 3), "(t-test);",
                     signif(wilcox.test(x ~ by, exact = F)$p.value, 3), "(Rank-sum test)"))
  output
}






dis_sum00 = function(xx, ref = table(xx), includeNA = F){
  na.ct = sum(is.na(xx))
  foo.nm = names(ref)
  outp = NULL
  for(j in 1:length(ref)){
    #######################################
    #### change to others, eg n_perc0() ###
    # stattt = n_perc(xx[, i] == foo.nm[j], na_rm = T, show_denom = "never")
    stattt = paste(sum(xx == foo.nm[j], na.rm = T), " (",
                   round(mean(xx == foo.nm[j], na.rm = T)*100, 2),
                   "%)", sep = "")
    #######################################
    if(j == 1)
      outp = rbind(outp, c(foo.nm[j], stattt))
    else
      outp = rbind(outp, c(foo.nm[j], stattt))
  }
  if(na.ct > 0 | includeNA){
    outp = rbind(outp, c("NA", na.ct))
  }
  outp
}



dis_summ = function(x, by, test){
  by_cat = unique(by)
  output = names(table(x, useNA = "ifany"))
  for(vv in by_cat){
    foo = dis_sum00(x[by == vv], ref = table(x), includeNA = sum(is.na(x))>0)
    output = cbind(output, foo[, 2])
  }
  output[, 1] = foo[, 1]
  all = dis_sum00(x)
  output = cbind("", output, all[, 2])
  if(test){
    testtype = ""
    tt = tryCatch(chisq.test(table(x, by, useNA = "no")), error=function(e) e, warning=function(w) w)
    if(is(tt, "warning")){
      tt = fisher.test(table(x, by, useNA = "no"))
      testtype = "(exact)"
    }

    pvall = rep("", dim(output)[1])
    pvall[1] = paste(signif(tt$p.value, 3), testtype)
    output = cbind(output, pvall)
  }

  output
}






summary_tab = function(xlist = c("Age", "Absolute.CD4.Count.(cells/uL)", "Gender"), by = "Told.had.HCV",
                       data = hcv, test = F){
  if(is.null(by)){
    test = F
    by = "noMeaning"
    data$noMeaning = rbinom(dim(data)[1], 1, prob = 0.5)
  }

  outp = NULL
  for(i in xlist){
    #######################################
    if(is.numeric(data[, i])){
      tmp = con_summ(data[, i], by= data[, by], test)
      tmp[1] = i
      outp = rbind(outp, tmp)
    }
    ########################################
    if(is.character(data[, i]) | is.factor(data[, i]) ){
      tmp = dis_summ(data[, i], by= data[, by], test)
      tmp[1, 1] = i
      outp = rbind(outp, tmp)
    }
  }
  rownames(outp) = NULL

  middle =  NULL
  for(ww in unique(data[, by])){
    middle = c(middle, paste(by, "  (", ww, "; N = ", sum(data[, by] == ww), ")", sep = ""))
  }

  if(!test) colnames(outp) = c("Variable", "Level", middle,
                               paste("All  (N = ", dim(data)[1],  ")", sep = ""))
  if(test)
    colnames(outp) = c("Variable", "Level", middle,
                       paste("All  (N = ", dim(data)[1],  ")", sep = ""),
                       "p value")


  if(by == "noMeaning")
    outp = outp[, c(1, 2, 5)]
  outp
}

