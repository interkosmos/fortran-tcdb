! tcversion.f90
!
! Provides access to the Tokyo Cabinet version string.
!
! Author:   Philipp Engel
! Licence:  ISC
module tcversion
    use, intrinsic :: iso_c_binding, only: c_associated, c_ptr
    use :: tcutil, only: c_f_string_ptr, c_strlen
    implicit none
    private
    public :: tc_version

    interface
        ! const char *tc_version()
        function tc_version_() bind(c, name='tc_version')
            import :: c_ptr
            type(c_ptr) :: tc_version_
        end function tc_version_
    end interface
contains
    ! const char *tc_version()
    function tc_version()
        !! Wrapper for `tc_version_()` that converts the returned C
        !! pointer to Fortran character.
        character(len=:), allocatable :: tc_version
        type(c_ptr)                   :: ptr
        integer(kind=8)               :: size

        ptr = tc_version_()

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_version)
        call c_f_string_ptr(ptr, tc_version)
        ! call c_free(ptr)
    end function tc_version
end module tcversion
