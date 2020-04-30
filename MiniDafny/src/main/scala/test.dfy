method m(x : int) 
requires x == 0
ensures x == 3
{
    var x := x + 1;
}