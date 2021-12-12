program b2075
    integer, parameter :: l = 1500
    integer :: mat(l, l), top(l), ns(l), i, n, mi

    read *, n, mat(1:n, 1:n)
    top(1:n) = n
    ns(1:n) = mat(1:n, n)
    do i = 1, n-1
        mi = maxloc(ns(1:n), 1)
        top(mi) = top(mi) - 1
        ns(mi) = mat(mi, top(mi))
    end do
    print '(I0)', maxval(ns(1:n), 1)
end program
