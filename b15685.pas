uses math;
var
  x, y, d, g, n, i : integer;
  dcurve : array[1..1024] of integer;
  t : array[0..100, 0..100] of boolean;
  
procedure fill_dcurve;
var i, j, a, b : integer;
begin
  dcurve[1] := 0; dcurve[2] := 1;
  i := 2; j := 3;
  while j <= 1024 do begin
    case dcurve[i] of
      0: begin a := 0; b := 1; end;
      1: begin a := 2; b := 1; end;
      2: begin a := 2; b := 3; end;
      3: begin a := 0; b := 3; end;
      else;
    end;
    dcurve[j] := a; dcurve[j+1] := b;
    inc(i); inc(j, 2);
  end;
end;

procedure clear_t;
var i, j : integer;
begin
  for i := 0 to 100 do
    for j := 0 to 100 do
      t[i, j] := false;
end;

procedure draw_t;
var dx, dy, i : integer;
begin
  t[x, y] := true;
  
  for i := 1 to 2**g do begin
    case (d + dcurve[i]) mod 4 of
      0: begin dx :=  1; dy :=  0; end;
      1: begin dx :=  0; dy := -1; end;
      2: begin dx := -1; dy :=  0; end;
      3: begin dx :=  0; dy :=  1; end;
      else;
    end;
    
    inc(x, dx); inc(y, dy);
    t[x, y] := true;
  end;
end;

function nump() : integer;
var i, j : integer;
begin
  nump := 0;
  for i := 0 to 99 do
    for j := 0 to 99 do
      if t[i, j] and t[i+1, j] and t[i, j+1] and t[i+1, j+1]
      then inc(nump);
end;

begin
  fill_dcurve;
  readln(n);
  
  clear_t;
  for i := 1 to n do begin
    readln(x, y, d, g);
    draw_t;
  end;

  writeln(nump());
end.