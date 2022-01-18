program RecursiveFactorial;

begin
   var y:= 1;
   var x:= 5;

   proc factorial is
   begin
      var z:= x;

      if x = 1 then
         skip
      else begin
         x:= x - 1;
         call factorial;
         y:= y * z
      end
    end;

    call factorial
end
