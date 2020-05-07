method m(x : int) returns (y: int)
requires x == 0
ensures y == 0
{
  return x;
}

// method foo(x : int) returns (y : int)
// requires x == 2
// ensures y == 3
// {
//   x
// }