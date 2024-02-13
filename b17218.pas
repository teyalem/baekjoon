uses math;
var
  n, m, i, j : integer;
  a, b, out : string;
  l : array[0..40, 0..40] of integer;
begin
  readln(a); n := length(a);
  readln(b); m := length(b);
  
  for i := 0 to n do l[i, 0] := 0;
  for j := 0 to n do l[j, 0] := 0;
  
  for i := 1 to n do begin
    for j := 1 to m do begin
      if a[i] = b[j] then l[i, j] := l[i-1, j-1] + 1
      else l[i, j] := max(l[i-1, j], l[i, j-1]);
    end;
  end;
  
  out := '';
  i := n; j := m;
  repeat
    if a[i] = b[j] then begin
      out := a[i] + out;
      dec(i); dec(j);
    end
    else begin
      if l[i-1, j] > l[i, j-1] then
        dec(i)
      else
        dec(j);
    end;
  until (i = 0) or (j = 0);
  
  write(out);
end.