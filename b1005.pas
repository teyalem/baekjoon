const
  MaxN = 1000;

var
  deps : array[1..MaxN] of array of int32;
  ds : array[1..MaxN] of int32;
  cds : array[1..MaxN] of int32;
  t : int32;
  n, k, w : int32;
  i, a, b : int32;

function dtime(i : int32) : int32;
var j, sd, maxd : int32;
begin
  if cds[i] = -1 then begin
    cds[i] := ds[i];
    maxd := 0;
    for j := 0 to high(deps[i]) do begin
      sd := dtime(deps[i][j]);
      if sd > maxd then maxd := sd;
    end;
    inc(cds[i], maxd);
  end;

  dtime := cds[i];
end;

begin
  readln(t);
  
  while t > 0 do begin
   readln(n, k);
   for i := 1 to n do begin
     read(ds[i]);
	 cds[i] := -1;
     setLength(deps[i], 0);
   end;
   for i := 1 to k do begin
     readln(a, b);
     setLength(deps[b], length(deps[b]) + 1);
     deps[b][high(deps[b])] := a;
   end;
   
   readln(w);
   writeln(dtime(w));
   dec(t);
  end;
end.
