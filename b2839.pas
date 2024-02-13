var n, k : integer;
begin
  read(n);
  k := n div 5;
  while (k >= 0) and ((n - 5 * k) mod 3 <> 0) do
    dec(k);
  if k >= 0 then
    write(k + (n - 5 * k) div 3)
  else
    write(-1);
end.