program FactorialFor(x, ac, i, start, stop);

x := 6;

start := 1;
stop := x;
ac := 1;
for i:= start to stop do begin
   ac := ac * i;
   start := start + 2;
   stop := stop - 2
end
