program b2630
    implicit none
    integer :: a(128, 128)
    integer :: n, w, b

    w = 0; b = 0
    read *, n
    read *, a(1:n, 1:n)
    call paper(a(1:n, 1:n))
    print '(I0)', w
    print '(I0)', b
contains
    recursive subroutine paper(a)
        integer, intent(in) :: a(:, :)
        integer :: n, hn

        if (all(a == 0)) then
            w = w + 1
        else if (all(a == 1)) then
            b = b + 1
        else
            n = size(a(1,:))
            hn = n/2
            call paper(a(1:hn, 1:hn))
            call paper(a(hn+1:n, 1:hn))
            call paper(a(1:hn, hn+1:n))
            call paper(a(hn+1:n, hn+1:n))
        end if
    end subroutine
end program
