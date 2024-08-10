uses math;
type
  team = set of 1..20;
var
  n, i, j : int32;
  mat : array[1..20, 1..20] of int32;
  u : team;
  start : team;
  link : team;
  mindiff : int32;

function score(s : team) : int32;
var i, j : int32;
begin
  score := 0;
  for i in s do
    for j in s do
      inc(score, mat[i, j]);
end;

procedure UpdateScore;
var sc, lc : int32;
begin
  link := u - start;
  sc := score(start);
  lc := score(link);
  mindiff := min(mindiff, abs(sc - lc));
end;

procedure SplitTest(d : int32; s : int32; e : int32);
var i : int32;
begin
  for i := s to e - d + 1 do begin
    include(start, i);
    if d = 1 then UpdateScore
    else SplitTest(d - 1, i + 1, e);
    exclude(start, i);
  end;
end;

begin
  readln(n);
  for i := 1 to n do
    for j := 1 to n do
      read(mat[i, j]);
  
  mindiff := MaxInt;
  u := [];
  for i := 1 to n do include(u, i);
  SplitTest(n div 2, 1, n);
  write(mindiff);
end.