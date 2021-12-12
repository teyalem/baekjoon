program b1149
    implicit none
    integer :: n, i, arr(1000, 3), cache(1000, 3)

    cache = -1
    read *, n
    do i = 1, n
        read *, arr(i,:)
    end do
    print '(I0)', min(s(1, 1), min(s(1, 2), s(1, 3)))
contains
    recursive integer function s(i, c) result(p)
        integer, value :: i, c
        if (cache(i, c) /= -1) then
            p = cache(i, c)
            return
        end if
        p = arr(i, c)
        if (i == n) return
        if (c == 1) then
            p = p + min(s(i+1, 2), s(i+1, 3))
        else if (c == 2) then
            p = p + min(s(i+1, 1), s(i+1, 3))
        else
            p = p + min(s(i+1, 1), s(i+1, 2))
        end if
        if (cache(i, c) == -1) cache(i, c) = p
    end function
end program
