var
  (* NOTE: you should always care about the type of string in Pascal! Without {$H+} switch, string = ShortString and this will limit length to 255! *) 
  s : AnsiString;
  q, l, r, i : int32;
  a : char;
  cs : array of array['a'..'z'] of int32;
begin
  readln(s);
  setLength(cs, length(s) + 1); (* 0..length(s) *)
  readln(q);
  
  for a := 'a' to 'z' do cs[0, a] := 0;
  for i := 1 to length(s) do begin
    for a := 'a' to 'z' do cs[i, a] := cs[i-1, a];
    inc(cs[i, s[i]]);
  end;
  
  while q > 0 do begin
    readln(a, l, r);
    writeln(cs[r+1, a] - cs[l, a]);
    dec(q);
  end;
end.