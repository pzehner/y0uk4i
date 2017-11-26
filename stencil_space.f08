module stencil_space
    use iso_fortran_env
    implicit none

    type stencil_space_factory
        integer :: order
        logical :: forward
        integer :: shift_before_x, shift_after_x
        integer :: shift_before_y, shift_after_y
        integer :: shift_before_z, shift_after_z
        integer :: imin, imax
        integer :: jmin, jmax
        integer :: kmin, kmax

        contains
            procedure, pass :: apply
    end type

    public :: get_stencil_space_factory, stencil_space_factory

    private

    contains

        function get_stencil_space_factory(order, ni, nj, nk, dimens, forward)
            integer, intent(in) :: order
            integer, intent(in), optional :: ni, nj, nk
            integer, dimension(3), intent(in), optional :: dimens
            logical, intent(in), optional :: forward

            type(stencil_space_factory) :: get_stencil_space_factory

            integer :: ni_, nj_, nk_
            logical :: forward_
            integer :: shift_before_x, shift_after_x
            integer :: shift_before_y, shift_after_y
            integer :: shift_before_z, shift_after_z
            integer :: imin, imax
            integer :: jmin, jmax
            integer :: kmin, kmax

            if (present(ni) .and. present(nj) .and. present(nk)) then
                ni_ = ni
                nj_ = nj
                nk_ = nk

            elseif (present(dimens)) then
                ni_ = dimens(1)
                nj_ = dimens(2)
                nk_ = dimens(3)

            else
                print *, "error: you must set either 'ni', &
                    &'nj', 'nk' or 'dimens'"
                error stop 1

            endif

            if (present(forward)) then
                forward_ = forward

            else
                forward_ = .true.

            endif

            call set_shift(shift_before_x, shift_after_x, imin, imax, &
                ni_, order, forward_)
            call set_shift(shift_before_y, shift_after_y, jmin, jmax, &
                nj_, order, forward_)
            call set_shift(shift_before_z, shift_after_z, kmin, kmax, &
                nk_, order, forward_)

            get_stencil_space_factory%order = order
            get_stencil_space_factory%forward = forward_
            get_stencil_space_factory%shift_before_x = shift_before_x
            get_stencil_space_factory%shift_before_y = shift_before_y
            get_stencil_space_factory%shift_before_z = shift_before_z
            get_stencil_space_factory%shift_after_x = shift_after_x
            get_stencil_space_factory%shift_after_y = shift_after_y
            get_stencil_space_factory%shift_after_z = shift_after_z
            get_stencil_space_factory%imin = imin
            get_stencil_space_factory%jmin = jmin
            get_stencil_space_factory%kmin = kmin
            get_stencil_space_factory%imax = imax
            get_stencil_space_factory%jmax = jmax
            get_stencil_space_factory%kmax = kmax

        end function

        subroutine apply(this, stencil, field, i, j, k)
            class(stencil_space_factory), intent(in) :: this
            real(kind=real64), dimension(:,:,:), target, intent(in) :: field
            integer, intent(in) :: i, j, k

            real(kind=real64), dimension(:,:,:), pointer, intent(inout) :: stencil

            if (associated(stencil)) nullify(stencil)

            stencil => field( &
                i - this%shift_before_x : i + this%shift_after_x, &
                j - this%shift_before_y : j + this%shift_after_y, &
                k - this%shift_before_z : k + this%shift_after_z  &
            )

        end subroutine

        subroutine set_shift(shift_before, shift_after, imin, imax, &
                dimens, order, forward)
            integer, intent(in) :: dimens, order
            logical, intent(in) :: forward

            integer, intent(out) :: shift_after, shift_before
            integer, intent(out) :: imin, imax

            if (dimens == 1) then
                shift_before = 0
                shift_after = 0
                imin = 1
                imax = dimens

            elseif (order <= dimens) then
                if (mod(order, 2) == 0) then
                    shift_before = order / 2 - transfer(forward, 1)
                    imin = order / 2 + transfer(.not. forward, 1)
                    imax = dimens - imin + 2 * transfer(.not. forward, 1)

                else
                    shift_before = order / 2
                    imin = shift_before + 1
                    imax = dimens - imin + 1

                end if

                shift_after = order - shift_before - 1

            else
                print *, "error: dimensions too narrow for this stencil"
                error stop 1

            end if
        end subroutine
end module
