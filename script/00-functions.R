

transpose = function(data, id, blocks, variables, variables_block){
  
  data$id = data[, id]
  
  nblocks = length(blocks)
  
  result = data.frame(cbind(id = rep(data$id, nblocks), 
                            plot =  rep(letters[1:nblocks], each = nrow(data)),
                            tech = as.vector(unlist(data[blocks]))))
  
  result$id = paste(result$id, result$plot, sep = "-")
  
  for (i in seq_along(variables)) {
    
    val_i = data.frame(id = rep(data$id, nblocks),
                       plot =  rep(letters[1:nblocks], each = nrow(data)),
                       value = as.numeric(unlist(data[paste0(variables[i], 
                                                              variables_block)])))
    
    val_i$id = paste(val_i$id, val_i$plot, sep = "-")
    
    val_i = val_i[, -c(2)]
    
    result = merge(result, val_i, by = "id")
    
    names(result)[names(result) == "value"] = variables[i]

  }
  
  return(result)
  
}
