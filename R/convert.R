convert_to_draw_array <- function(x, ndraws_per_chain = length(x), nchains = 1){
  matrix(nr = ndraws_per_chain, nc = nchains, data = x)
}
