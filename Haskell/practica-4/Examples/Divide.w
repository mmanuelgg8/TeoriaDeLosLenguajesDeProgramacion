program Divide;

// a simple program to compute quotient and remainder

begin

   var x:= 27;
   var y:= 5;
   var q:= 0;
   var r:= 0;
   var error:= 0;

   if !(y = 0) then  // do not divide by zero
      begin
         q:= 0;
         while (y <= x) do begin
            x:= x - y;
            q:= q + 1
         end;
         r:= x
      end
   else
      error:= 1  // signal error somehow
end
