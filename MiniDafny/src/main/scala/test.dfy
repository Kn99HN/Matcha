method m(x : int) 
requires x == 0
ensures x == 0
{
  var x := 2 * x; 
}