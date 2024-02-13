uses math;
var
  n, k, i, maxt : int32;
  d : array[0..100000] of int32;
begin
  readln(n, k);
  d[0] := 0;
  for i := 1 to n do begin
    read(d[i]);
    inc(d[i], d[i-1]);
  end;
  maxt := d[k];
  for i := k+1 to n do
    maxt := max(maxt, d[i] - d[i-k]);
  write(maxt);
end.