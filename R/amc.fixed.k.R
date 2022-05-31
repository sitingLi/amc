#' Title Adaptive Mixture Categorization
#' @description This function is the fixed-k version of Adaptive Mixture Categorization, which categorizes variables into specified number of categories (k). The number of categories for different variables (columns) is the same.
#' @param data The input data for categorization
#' @param group The number of categories, which can be set as an integer from 2 to 10 by the user. (default = 2) We recommend the number is less than 6.
#' @param min_num The minimum sample size of each category, which can be set as a positive integer. (default = 1) We recommend the number is larger than 5 percent of the overall sample size.
#'
#' @return The categorized data and the categorization thresholds
#' \item{output}{The categorized data}
#' \item{cutoff}{The thresholds/cutoffs of the categorization for each variable}
#' @export
#'
amc.fixed.k = function(data, group = 2, min_num = 1) {
  output = data
  num_var = dim(output)[2]
  num_sub = dim(output)[1]
  mat_position = matrix(NA, group - 1, num_var)
  mat_threshold = matrix(NA, group - 1, num_var)

  # Parameter check
  if (group>5 | group<2 | min_num<=0 | num_sub < (group * min_num))

  {

    stop ('please check the number of groups and/or minimum obs in each group')

  }


  for (temp in 1:num_var)
  {
    data_temp = sort(data[,temp])
    data_temp = data.frame(data_temp)
    names(data_temp)[1] = 'value'
    F_value = rep(0, num_sub-1)

    for (i in min_num:(num_sub - min_num))
    {
      threshold_temp = data_temp$value[i]
      data_temp[,'value.level'] <- cut(data_temp$value, breaks=c(-Inf, threshold_temp, Inf), labels=c('0', '1'))
      one.way <- aov(value ~ value.level, data = data_temp)
      F_value[i] = summary(one.way)[[1]][["F value"]][1]
    }

    position1 = which.max(F_value)
    threshold1 = data_temp$value[position1]
    mat_position[1, temp] = position1
    mat_threshold[1, temp] = threshold1

    position = position1
    threshold = threshold1

    output[,temp] <- cut(data[,temp], breaks = c(-Inf,threshold,Inf), labels = FALSE)

    # k = 2
    if(group == 2)
    {
      next
    }


    # k > 2
    for (g in 3:10)
    {
      seq = vector(length = 0)
      start = min_num

      for(s in 1:length(position))
      {
        end = position[s] - min_num

        if(start <= end)
        {
          seq = c(seq, start:end)
          flag = TRUE
        }

        else
        {
          flag = FALSE
        }

        start = position[s] + min_num
      }

      end = num_sub - min_num

      if(start <= end)
      {
        seq = c(seq, start:end)
        flag = TRUE
      }

      if(!flag)
      {
        cat('Column ', temp, 'can not meet the request for categorization','\n')
        cat('The actual categories for column ', temp, ' is ', g-1,'\n')
        break
      }


      F_value=rep(0, num_sub - g + 1)
      label = vector(mode = 'character', length = 0)

      for(l in 0:(g - 1))
      {
        label = c(label, l)
      }

      for (i in seq)
      {
        threshold_temp = data_temp$value[i]
        threshold_temp = sort(c(threshold, threshold_temp))
        data_temp[,'value.level'] <- cut(data_temp$value, breaks = c(-Inf, threshold_temp, Inf), labels = label)
        one.way <- aov(value ~ value.level, data = data_temp)
        F_value[i] = summary(one.way)[[1]][["F value"]][1]
      }

      position1 = which.max(F_value)
      threshold1 = data_temp$value[position1]
      mat_position[g-1,temp] = position1
      mat_threshold[g-1,temp] = threshold1

      position = sort(c(position, position1))
      threshold = sort(c(threshold, threshold1))

      output[,temp] <- cut(data[,temp], breaks = c(-Inf, threshold, Inf), labels = FALSE)

      if(g == group)
      {
        break
      }

    }

  }

  cutoff <- apply(mat_threshold, 2, sort)

  returnlist <- list(output = output, cutoff = cutoff)
  return(returnlist)
}
