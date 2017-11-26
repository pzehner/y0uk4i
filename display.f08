module display
    use iso_fortran_env
    implicit none

    contains

        subroutine display2(content, pattern)
            real(kind=real64), dimension(:,:), intent(in) :: content
            character(len=*), intent(in) :: pattern
            character(len=255) :: pattern_final
            character(len=10) :: nj_char

            integer :: ni, nj
            integer :: i

            ni = size(content, 1)
            nj = size(content, 2)

            write(nj_char, "(i10)") nj
            pattern_final = "(" // trim(nj_char) // "(" // trim(pattern) // ",1x))"

            do i=1,ni
                write(output_unit, pattern_final) content(i, :)
            end do

        end subroutine
end module
