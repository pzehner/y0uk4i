module stencil_space
    use iso_fortran_env
    implicit none

    ! space stencil factory object
    type stencil_space_factory
        ! order of the stencil
        integer :: order

        ! direction of work of the stencil
        logical :: forward

        ! amount of cells in the each directions of the stencil, before and
        ! after the application point
        integer :: shift_before_x, shift_after_x
        integer :: shift_before_y, shift_after_y
        integer :: shift_before_z, shift_after_z

        ! limits of application of the stencil
        integer :: imin, imax
        integer :: jmin, jmax
        integer :: kmin, kmax

        contains
            procedure, pass :: apply
    end type

    public :: create_stencil_space_factory, stencil_space_factory

    private

    contains

        ! create a space stencil factory object
        function create_stencil_space_factory(order, ni, nj, nk, dimens, field, forward)
            ! size of the stencil
            integer, intent(in) :: order

            ! explicit dimensions of the array where the stencil works
            integer, intent(in), optional :: ni, nj, nk

            ! array-shaped dimensions of the array where the stencil works
            integer, dimension(3), intent(in), optional :: dimens

            ! array where the stencil works, provided to extract its dimensions
            real(kind=real64), dimension(:, :, :), intent(in), optional :: field

            ! direction of work of the stencil
            logical, intent(in), optional :: forward

            type(stencil_space_factory) :: create_stencil_space_factory

            ! internal representation of the arguments
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

            elseif (present(field)) then
                ni_ = size(field, 1)
                nj_ = size(field, 2)
                nk_ = size(field, 3)

            else
                print *, "error: you must set either ('ni', &
                    & 'nj', 'nk') or 'dimens' or 'field'"
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

            create_stencil_space_factory%order = order
            create_stencil_space_factory%forward = forward_
            create_stencil_space_factory%shift_before_x = shift_before_x
            create_stencil_space_factory%shift_before_y = shift_before_y
            create_stencil_space_factory%shift_before_z = shift_before_z
            create_stencil_space_factory%shift_after_x = shift_after_x
            create_stencil_space_factory%shift_after_y = shift_after_y
            create_stencil_space_factory%shift_after_z = shift_after_z
            create_stencil_space_factory%imin = imin
            create_stencil_space_factory%jmin = jmin
            create_stencil_space_factory%kmin = kmin
            create_stencil_space_factory%imax = imax
            create_stencil_space_factory%jmax = jmax
            create_stencil_space_factory%kmax = kmax

        end function

        ! apply the stencil to the field
        ! it gives a shorthand to the field
        subroutine apply(this, stencil, field, i, j, k)
            class(stencil_space_factory), intent(in) :: this
            real(kind=real64), dimension(:, :, :), target, intent(in) :: field
            integer, intent(in) :: i, j, k

            real(kind=real64), dimension(:, :, :), pointer, intent(inout) :: stencil

            ! check if the location of the stencil center is coherent
            if (i < this%imin .or. i > this%imax) then
                print *, "error: 'i' location of the stencil is out of limits"
                error stop 1
            end if

            if (j < this%jmin .or. j > this%jmax) then
                print *, "error: 'j' location of the stencil is out of limits"
                error stop 1
            end if

            if (k < this%kmin .or. k > this%kmax) then
                print *, "error: 'k' location of the stencil is out of limits"
                error stop 1
            end if

            ! deassociate stencil if previously used
            if (associated(stencil)) nullify(stencil)

            ! associate stencil
            stencil => field( &
                i - this%shift_before_x : i + this%shift_after_x, &
                j - this%shift_before_y : j + this%shift_after_y, &
                k - this%shift_before_z : k + this%shift_after_z  &
            )

        end subroutine

        ! set limits where the stencil can be applied
        subroutine set_shift(shift_before, shift_after, imin, imax, &
                dimens, order, forward)
            ! size of the stencil
            integer, intent(in) :: order

            ! size of the array where the stencil is applied
            integer, intent(in) :: dimens

            ! direction of work of the stencil
            logical, intent(in) :: forward

            ! amount of cells before and after the application point
            integer, intent(out) :: shift_after, shift_before

            ! limits where the stencil can be applied
            integer, intent(out) :: imin, imax

            ! stencil of dimension 1 are degenerated kinds of stencils
            if (dimens == 1) then
                shift_before = 0
                shift_after = 0
                imin = 1
                imax = dimens

            elseif (order <= dimens) then
                ! for even orders
                if (mod(order, 2) == 0) then
                    shift_before = order / 2 - transfer(forward, 1)
                    imin = order / 2 + transfer(.not. forward, 1)
                    imax = dimens - imin + 2 * transfer(.not. forward, 1)

                ! for odd orders
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
