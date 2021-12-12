program b9184
    implicit none
    integer :: a, b, c
    integer :: cache(20, 20, 20)
    100 format ('w(', (I0), ', ', (I0), ', ', (I0), ') = ', (I0))

    cache = -1
    do
        read *, a, b, c
        if (a == -1 .and. b == -1 .and. c == -1) exit
        print 100, a, b, c, w(a, b, c)
    end do
contains
    recursive integer function w(a, b, c) result(r)
        integer, value :: a, b, c

        if (a <= 0 .or. b <= 0 .or. c <= 0) then
            r = 1
            return
        else if (a > 20 .or. b > 20 .or. c > 20) then
            r = w(20, 20, 20)
            return
        end if

        if (cache(a, b, c) /= -1) then
            r = cache(a, b, c)
            return
        end if

        if (a < b .and. b < c) then
            r = w(a, b, c-1) + w(a, b-1, c-1) - w(a, b-1, c)
        else
            r = w(a-1, b, c) + w(a-1, b-1, c) + w(a-1, b, c-1) &
                - w(a-1, b-1, c-1)
        end if 

        cache(a, b, c) = r
    end function
end program
