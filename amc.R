amc = function(data, group=2, min_obs=0.1) {
  
  output=data              
  
  num_var=dim(output)[2]  
  
  num_sub=dim(output)[1]  
  
  mat_position=matrix(NA,group-1,num_var)    
  
  mat_threshold=matrix(NA,group-1,num_var)  
  
  
  
  min_obsn = floor(num_sub*min_obs)  
  
  
  
  
  
  if (group>5 | group<2 | min_obs<=0 | min_obs>=1 | num_sub < (group * min_obsn))    
    
  {
    
    stop ('please check the number of groups and/or minimum obs in each group')
    
  }
  
  
  
  
  
  for (temp in 1:num_var)      
    
  {
    
    data_temp=sort(data[,temp])      
    
    data_temp = data.frame(data_temp)  
    
    names(data_temp)[1] = 'value'      
    
    
    
    
    
    ###########################
    
    F_value=rep(0, num_sub-1)                
    
    
    
    for (i in min_obsn:(num_sub - min_obsn))  
      
    {
      
      threshold_temp = data_temp$value[i]      
      
      data_temp[,'value.level'] <- cut(data_temp$value,breaks=c(-Inf,threshold_temp,Inf),labels=c('0','1'))
      
      one.way <- aov(value ~ value.level, data = data_temp)    
      
      F_value[i]=summary(one.way)[[1]][["F value"]][1]        
      
    }
    
    position1 = which.max(F_value)        
    
    threshold1 = data_temp$value[position1]
    
    mat_position[1,temp]=position1      
    
    mat_threshold[1,temp]=threshold1        
    
    
    
    position = position1
    
    threshold = threshold1
    
    
    
    output[,temp] <- cut(data[,temp],breaks=c(-Inf,threshold,Inf),labels=FALSE)
    
    
    
    
    
    if(group==2)  
      
    {
      
      next
      
    }
    
    
    
    
    
    ###########################
    
    for (g in 3:5)
      
    {
      
      
      
      #######################################################
      
      seq = vector(length = 0)    
      
      start = min_obsn          
      
      
      
      
      
      for(s in 1:length(position))
        
      {
        
        end = position[s]-min_obsn
        
        
        
        if(start <= end)
          
        {
          
          seq = c(seq, start:end)
          
          flag = TRUE          
          
        }
        
        else
          
        {
          
          flag = FALSE
          
        }
        
        
        
        start = position[s]+min_obsn
        
      }
      
      
      
      end = num_sub - min_obsn
      
      if(start <= end)
        
      {
        
        seq = c(seq, start:end)
        
        flag = TRUE
        
      }
      
      
      
      
      
      if(!flag)        
        
      {
        
        cat('column ', temp, 'can not meet the request for grouping','\n')
        
        cat('the actual groups for column ', temp, ' is ', g-1,'\n')
        
        break      
        
      }
      
      
      
      
      
      #######################################################
      
      F_value=rep(0, num_sub-g+1)
      
      
      
      
      
      label = vector(mode='character',length = 0)
      
      for(l in 0:(g-1))
        
      {
        
        label = c(label, l)
        
      }
      
      
      
      
      
      for (i in seq)
        
      {
        
        threshold_temp = data_temp$value[i]                      
        
        threshold_temp = sort(c(threshold, threshold_temp))      
        
        data_temp[,'value.level'] <- cut(data_temp$value, breaks=c(-Inf,threshold_temp,Inf), labels=label)
        
        one.way <- aov(value ~ value.level, data = data_temp)    
        
        F_value[i]=summary(one.way)[[1]][["F value"]][1]        
        
      }
      
      position1 = which.max(F_value)        
      
      threshold1 = data_temp$value[position1]  
      
      mat_position[g-1,temp]=position1        
      
      mat_threshold[g-1,temp]=threshold1        
      
      
      
      
      
      position = sort(c(position, position1))
      
      threshold = sort(c(threshold, threshold1))
      
      
      
      output[,temp] <- cut(data[,temp],breaks=c(-Inf,threshold,Inf),labels=FALSE)
      
      if(g==group)  
        
      {
        
        break
        
      }
      
      
      
    }
    
    
    
  }
  
  
  
  returnlist <- list(output, mat_position, mat_threshold)
  
  
  
  return(returnlist)
  
  
  
}



input <- data.frame(x1,x2,x3)

resultset = amc(data = input, group = 2, min_obs = 0.1)

output <- resultset[[1]]

mat_position <- resultset[[2]]

mat_threshold <- resultset[[3]]
