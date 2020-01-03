! tcmisc.f90
!
! Fortran 2008 interface to miscellaneous utility functions of Tokyo Cabinet.
!
! Author:   Philipp Engel
! Licence:  ISC
module tcmisc
    use, intrinsic :: iso_c_binding, only: c_char, c_int, c_int64_t, c_ptr
    implicit none
    private

    public :: tc_date_str_www
    public :: tc_md5_hash
    public :: tc_str_mk_time

    interface
        ! int64_t tcstrmktime(const char *str)
        function tc_str_mk_time(str) bind(c, name='tcstrmktime')
            import :: c_int64_t, c_char
            character(kind=c_char, len=1), intent(in) :: str
            integer(kind=c_int64_t)                   :: tc_str_mk_time
        end function tc_str_mk_time

        ! void tcdatestrwww(int64_t t, int jl, char *buf)
        subroutine tc_date_str_www(t, jl, buf) bind(c, name='tcdatestrwww')
            import :: c_char, c_int, c_int64_t
            integer(kind=c_int64_t),       intent(in), value :: t
            integer(kind=c_int),           intent(in), value :: jl
            character(kind=c_char, len=1), intent(inout)     :: buf
        end subroutine tc_date_str_www

        ! void tcmd5hash(const void *ptr, int size, char *buf)
        subroutine tc_md5_hash(ptr, size, buf) bind(c, name='tcmd5hash')
            import :: c_char, c_int, c_ptr
            type(c_ptr),                   intent(in), value :: ptr
            integer(kind=c_int),           intent(in), value :: size
            character(kind=c_char, len=1), intent(inout)     :: buf
        end subroutine tc_md5_hash
    end interface
end module tcmisc
