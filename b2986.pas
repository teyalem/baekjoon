uses Math;

function max_divisor(N: longint) : longint;
var i, m : longint;
begin
    m := 1;
    for i := 2 to floor(sqrt(N)) do begin
        if N mod i = 0 then begin
            m := i;
            break;
        end;
    end;
    max_divisor := n div m;
end;

var N, m: longint;
begin
    readln(N);
    m := max_divisor(N);
    if m = N then writeln(N-1) { N is prime }
    else writeln(N-m); { non-prime }
end.
