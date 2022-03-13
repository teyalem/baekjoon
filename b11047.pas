var
    n, k, i, m : longint;
    coins : array[1..10] of longint;
begin
    m := 0;
    readln(n, k);
    for i := 1 to n do readln(coins[i]);
    for i := n downto 1 do begin
        m := m + k div coins[i];
        k := k mod coins[i];
    end;
    write(m);
end.
