! tcdb.f90
!
! General routines for the Fortran 2008 interface to Tokyo Cabinet.
!
! Author:   Philipp Engel
! Licence:  ISC
module tcdb
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_char, &
                                           c_null_char, c_ptr, c_size_t
    implicit none
    private

    public :: c_f_string_ptr
    public :: c_strlen
    public :: c_free

    ! Function and routine interfaces to libc.
    interface
        function c_strlen(str) bind(c, name='strlen')
            import :: c_ptr, c_size_t
            type(c_ptr), intent(in), value :: str
            integer(c_size_t)              :: c_strlen
        end function c_strlen

        subroutine c_free(ptr) bind(c, name='free')
            import :: c_ptr
            type(c_ptr), intent(in), value :: ptr
        end subroutine c_free
    end interface
contains
    subroutine c_f_string_ptr(c_string, f_string)
        !! Utility routine that copies a C string, passed as a C pointer, to a
        !! Fortran string.
        type(c_ptr),      intent(in)           :: c_string
        character(len=*), intent(out)          :: f_string
        character(kind=c_char, len=1), pointer :: p_chars(:)
        integer                                :: i

        if (.not. c_associated(c_string)) then
            f_string = ' '
        else
            call c_f_pointer(c_string, p_chars, [huge(0)])

            i = 1

            do while (p_chars(i) /= c_null_char .and. i <= len(f_string))
                f_string(i:i) = p_chars(i)
                i = i + 1
            end do

            if (i < len(f_string)) &
                f_string(i:) = ' '
        end if
    end subroutine c_f_string_ptr
end module tcdb
