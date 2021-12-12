function max(a, b: longint) : longint;
begin
    if a > b then max := a else max := b;
end;

var
    tri: array[1..500, 1..500] of longint;
    i, j, n: longint;
begin
    readln(n);
    for i := 1 to n do begin
        for j := 1 to i do read(tri[i, j]);
    end;
    for i := n-1 downto 1 do begin
        for j := 1 to i do begin
            tri[i, j] := tri[i, j] + max(tri[i+1, j], tri[i+1, j+1]);
        end;
    end;
    write(tri[1,1]);
end.
