method m(x : int) returns (y: int)
requires x == 0
ensures y == 0
{
  y := foo(0);
}

method foo(x : int) returns (y : int)
requires x == 0
ensures y == 1
{
  y := x + 1;
}