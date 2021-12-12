program b3273
    implicit none
    integer :: arr(100000), n, x

    read *, n
    read *, arr(1:n)
    read *, x
    call sort(arr(1:n))
    print '(I0)', count_pair(arr(1:n), x)
contains
    integer function count_pair(a, x) result(cnt)
        integer, intent(in) :: a(:)
        integer, value :: x
        integer :: i, j, k

        cnt = 0; i = 1; j = size(a)
        do while (i < j)
            k = arr(i) + arr(j)
            if (k < x) i = i + 1
            if (k > x) j = j - 1
            if (k == x) then
                cnt = cnt + 1
                i = i + 1
                j = j - 1
            end if
        end do
    end function

    subroutine mergetwo(arr, mid)
        integer, intent(inout) :: arr(:)
        integer, value :: mid
        integer :: i, j, tmp

        j = mid
        do i = 1, size(arr) - 1
            if (arr(j) < arr(i)) then
                tmp = arr(j)
                arr(j) = arr(i)
                arr(i) = tmp
            end if
        end do
    end subroutine

    subroutine sort(arr)
    end subroutine
end program
