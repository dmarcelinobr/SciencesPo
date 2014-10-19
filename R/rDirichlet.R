rDirichlet <-
function( n, alpha ) {
  
  l = length( alpha )
  
  theta = matrix( 0, n, l )
  
  for ( j in 1:l ) {
    
    theta[ , j ] = rgamma( n, alpha[ j ], 1 )
    
  }
  
  theta = theta / apply( theta, 1, sum )
  
  return( theta )
  
}
