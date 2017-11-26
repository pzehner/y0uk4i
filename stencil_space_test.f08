program test
    use iso_fortran_env
    use stencil_space, only : get_stencil_space_factory, stencil_space_factory
    use display, only : display2

    implicit none

    real(kind=real64), dimension(5, 5, 1), target :: field
    real(kind=real64), dimension(:,:,:), pointer :: stencil
    type(stencil_space_factory) :: sf
    integer :: i, j

    field = 5

    print "(a)", "before:"
    call display2(field(:,:,1), "f3.1")


    ! sf = get_stencil_space_factory(4, dimens=shape(field), forward=.false.)
    sf = get_stencil_space_factory(4, dimens=shape(field))
    print *, sf%jmin, sf%jmax
    print *, sf%imin, sf%imax

    print "(a, 1x, 3(i0.2, 1x))", "size of field:", shape(field)
    print "(a, 1x, 3(i0.2, 1x))", "size of stencil:", shape(stencil)

    do j = sf%jmin, sf%jmax
        do i = sf%imin, sf%imax
            call sf%apply(stencil, field, i, j, 1)
            stencil = 0
        end do
    end do

    print "(a)", "after:"
    call display2(field(:,:,1), "f3.1")
end program
