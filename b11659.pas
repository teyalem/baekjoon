var n, m, i, j : int32;
  s : array[0..100000] of int32;
begin
  readln(n, m);
  s[0] := 0;
  for i := 1 to n do begin
    read(s[i]);
    inc(s[i], s[i-1]);
  end;
  while m > 0 do begin
    readln(i, j);
    writeln(s[j] - s[i-1]);
    dec(m);
  end;
end.