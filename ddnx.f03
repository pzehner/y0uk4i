module ddnx
    use iso_fortran_env

    implicit none


    contains

        function ddnx_2(stencil_field, stencil_x)
            real(kind=real64) :: ddnx_2
            real(kind=real64), dimension(:) :: stencil_field, stencil_x

            ddnx_2 = (stencil_field(2) - stencil_field(1)) / &
                (stencil_x(2) - stencil_x(1))

        end function

end module
