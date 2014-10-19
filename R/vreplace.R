vreplace <-
function (vector, from, to) 
{ 
  to[match(vector, from)]
}
