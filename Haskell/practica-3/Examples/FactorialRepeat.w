program FactorialRepeat(x, f);

x:= 5;
f := 1;
repeat
   f := f * x;
   x:= x - 1
until x = 0
