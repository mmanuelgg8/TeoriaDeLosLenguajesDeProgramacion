program Divide(q,r,error);

// a simple program to compute quotient and remainder

x := 19;
y := 5;

if !(y = 0) then  // do not divide by zero
   begin
      q:= 0;
      while (y <= x) do begin
         x := x - y;
         q := q + 1
      end;
      r := x
   end
else
   error := 1  // signal error somehow
