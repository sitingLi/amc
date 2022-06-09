#' Adaptive Mixture Categorization
#'
#' @description
#' This is the function for the Adaptive Mixture Categorization, which can categorize variables in a flexible way using F-statistic.
#' @param data The input data for categorization (should be a data frame)
#'
#' @return The categorized data, the number of categories, and the categorization thresholds.
#' \item{output}{The categorized data}
#' \item{cutoff}{The thresholds/cutoffs of the categorization for each variable}
#' \item{group}{The number of categories for each variable}
#' @export
#'
amc = function (data) {
  output = data
  num_var = dim(output)[2]
  num_sub = dim(output)[1]
  mat_threshold = matrix(NA, num_sub - 1, num_var)
  group = matrix(NA, 1, num_var)


  if (num_sub <= 1)
  {
    stop ('please check the data')
  }


  for (temp in 1:num_var)
  {
    data_temp = sort(data[,temp])
    data_temp = data.frame(data_temp)
    names(data_temp)[1] = 'value'
    F_value = rep(0, num_sub-1)

    # 2 groups
    for (i in 1:(num_sub-1))
    {
      threshold_temp = data_temp$value[i]
      data_temp[,'value.level'] <- cut(data_temp$value, breaks=c(-Inf, threshold_temp, Inf), labels=c('0', '1'))
      one.way <- aov(value ~ value.level, data = data_temp)
      F_value[i] = summary(one.way)[[1]][["F value"]][1]
    }

    position = which.max(F_value)
    threshold = data_temp$value[position]
    mat_threshold[1, temp] = threshold

    F_best = max(F_value)
    group[1, temp] = 2
    mat_threshold_best = mat_threshold
    output[,temp] <- cut(data[,temp], breaks = c(-Inf,threshold,Inf), labels = FALSE)


    if(num_sub == 2)
    {
      next
    }


    # 3 groups or more
    for (g in 3:min(10, num_sub))
    {
      seq = vector(length = 0)
      start = 1
      flag = FALSE

      for(s in 1:length(position))
      {
        end = position[s] - 1

        if(start <= end)
        {
          seq = c(seq, start:end)
          flag = TRUE
        }

        start = position[s] + 1
      }

      end = num_sub - 1

      if(start <= end)
      {
        seq = c(seq, start:end)
        flag = TRUE
      }

      if(!flag)
      {
        next
      }


      F_value=rep(0, num_sub - 1)

      label = vector(mode = 'character', length = 0)
      for(l in 0:(g - 1))
      {
        label = c(label, l)
      }



      for (i in seq)
      {
        threshold_temp = data_temp$value[i]

        if(!(threshold_temp %in% threshold))
        {
          threshold_temp = sort(c(threshold, threshold_temp))
          data_temp[,'value.level'] <- cut(data_temp$value, breaks = c(-Inf, threshold_temp, Inf), labels = label)
          one.way <- aov(value ~ value.level, data = data_temp)
          F_value[i] = summary(one.way)[[1]][["F value"]][1]
        }

      }

      position1 = which.max(F_value)
      threshold1 = data_temp$value[position1]
      mat_threshold[g-1,temp] = threshold1

      position = sort(c(position, position1))
      threshold = sort(c(threshold, threshold1))
      F_temp = max(F_value)

      if(F_best < F_temp)
      {
        group[1, temp] = g
        mat_threshold_best = mat_threshold
        output[,temp] <- cut(data[,temp], breaks = c(-Inf, threshold, Inf), labels = FALSE)
        F_best = F_temp

        break
      }

    }

  }

  cutoff <- apply(mat_threshold_best, 2, sort)

  returnlist <- list(output = output, cutoff = cutoff, group = group)

  return(returnlist)
}
