decode_explicit_info <- function(code){
  if(code == 0){return('Not Explicit')}
  if(code == 1){return('Explicit')}
  if(code == 2){return('Unknown')}
  if(code == 3){return('Edited')}
  if(code == 6){return('No Advice Available')}
  return(NA)
}
