var n, i, a, b, c : int32;
begin
  read(n);
  a := 1; b := 1;
  for i := 3 to n do begin
    c := b;
    inc(b, a);
    a := c;
  end;
  write(b, ' ', n - 2);
end.