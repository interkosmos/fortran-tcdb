! tcmdb.f90
!
! Fortran 2008 interface to the on-memory hash database API of Tokyo Cabinet.
!
! Author:   Philipp Engel
! Licence:  ISC
module tcmdb
    use, intrinsic :: iso_c_binding, only: c_associated, c_double, &
                                           c_f_pointer, c_bool, c_char, c_int, &
                                           c_int8_t, c_int32_t, c_int64_t, &
                                           c_null_char, c_ptr, c_size_t
    use :: tcutil
    implicit none
    private

    ! Interfaces and wrapper routines.
    public :: tc_mdb_add_double
    public :: tc_mdb_add_int
    public :: tc_mdb_cut_front
    public :: tc_mdb_del
    public :: tc_mdb_fwm_keys
    public :: tc_mdb_fwm_keys2
    public :: tc_mdb_get
    public :: tc_mdb_get2
    public :: tc_mdb_iter_init
    public :: tc_mdb_iter_next
    public :: tc_mdb_iter_next2
    public :: tc_mdb_new
    public :: tc_mdb_new2
    public :: tc_mdb_out
    public :: tc_mdb_out2
    public :: tc_mdb_put
    public :: tc_mdb_put2
    public :: tc_mdb_put_cat
    public :: tc_mdb_put_cat2
    public :: tc_mdb_put_keep
    public :: tc_mdb_put_keep2
    public :: tc_mdb_rnum
    public :: tc_mdb_vanish
    public :: tc_mdb_vsiz
    public :: tc_mdb_vsiz2

    ! Raw interfaces, require null termination of string arguments.
    public :: tc_mdb_fwm_keys2_
    public :: tc_mdb_get2_
    public :: tc_mdb_out2_
    public :: tc_mdb_put2_
    public :: tc_mdb_put_cat2_
    public :: tc_mdb_put_keep2_
    public :: tc_mdb_vsiz2_

    ! Function and routine interfaces to Tokyo Cabinet.
    interface
        ! double tcmdbadddouble(TCMDB *mdb, const void *kbuf, int ksiz, double num)
        function tc_mdb_add_double(mdb, kbuf, ksiz, num) bind(c, name='tcmdbadddouble')
            import ::  c_double, c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            real(kind=c_double), intent(in), value :: num
            real(kind=c_double)                    :: tc_mdb_add_double
        end function tc_mdb_add_double

        ! int tcmdbaddint(TCMDB *mdb, const void *kbuf, int ksiz, int num)
        function tc_mdb_add_int(mdb, kbuf, ksiz, num) bind(c, name='tcmdbaddint')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            integer(kind=c_int), intent(in), value :: num
            integer(kind=c_int)                    :: tc_mdb_add_int
        end function tc_mdb_add_int

        ! void tcmdbcutfront(TCMDB *mdb, int num)
        function tc_mdb_cut_front(mdb, num) bind(c, name='tcmdbcutfront')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            integer(kind=c_int), intent(in), value :: num
            type(c_ptr)                            :: tc_mdb_cut_front
        end function tc_mdb_cut_front

        ! TCLIST *tcmdbfwmkeys(TCMDB *mdb, const void *pbuf, int psiz, int max)
        function tc_mdb_fwm_keys(mdb, pbuf, psiz, max) bind(c, name='tcmdbfwmkeys')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: pbuf
            integer(kind=c_int), intent(in), value :: psiz
            integer(kind=c_int), intent(in), value :: max
            type(c_ptr)                            :: tc_mdb_fwm_keys
        end function tc_mdb_fwm_keys

        ! TCLIST *tcmdbfwmkeys2(TCMDB *mdb, const char *pstr, int max);
        function tc_mdb_fwm_keys2_(mdb, pstr, max) bind(c, name='tcmdbfwmkeys2')
            import ::  c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: pstr
            integer(kind=c_int),    intent(in), value :: max
            type(c_ptr)                               :: tc_mdb_fwm_keys2_
        end function tc_mdb_fwm_keys2_

        ! void *tcmdbget(TCMDB *mdb, const void *kbuf, int ksiz, int *sp)
        function tc_mdb_get(mdb, kbuf, ksiz) bind(c, name='tcmdbget')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr)                            :: tc_mdb_get
        end function tc_mdb_get

        ! char *tcmdbget2(TCMDB *mdb, const char *kstr)
        function tc_mdb_get2_(mdb, kstr) bind(c, name='tcmdbget2')
            import :: c_char, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            type(c_ptr)                               :: tc_mdb_get2_
        end function tc_mdb_get2_

        ! bool tcmdbiterinit(TCMDB *mdb)
        function tc_mdb_iter_init(mdb) bind(c, name='tcmdbiterinit')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: mdb
            logical(kind=c_bool)           :: tc_mdb_iter_init
        end function tc_mdb_iter_init

        ! void *tcmdbiternext(TCMDB *mdb, int *sp)
        function tc_mdb_iter_next(mdb, sp) bind(c, name='tcmdbiternext')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_mdb_iter_next
        end function tc_mdb_iter_next

        ! char *tcmdbiternext2(TCMDB *mdb)
        function tc_mdb_iter_next2_(mdb) bind(c, name='tcmdbiternext2')
            import :: c_ptr
            type(c_ptr), intent(in), value :: mdb
            type(c_ptr)                    :: tc_mdb_iter_next2_
        end function tc_mdb_iter_next2_

        ! uint64_t tcmdbmsiz(TCMDB *mdb);
        function tc_mdb_msiz(mdb) bind(c, name='tcmdbmsiz')
            import :: c_int64_t, c_ptr
            type(c_ptr), intent(in), value :: mdb
            integer(kind=c_int64_t)        :: tc_mdb_msiz
        end function tc_mdb_msiz

        ! TCMDB *tcmdbnew(void)
        function tc_mdb_new() bind(c, name='tcmdbnew')
            import :: c_ptr
            type(c_ptr) :: tc_mdb_new
        end function tc_mdb_new

        ! TCMDB *tcmdbnew2(uint32_t bnum)
        function tc_mdb_new2(bnum) bind(c, name='tcmdbnew2')
            import :: c_int32_t, c_ptr
            integer(kind=c_int32_t), intent(in), value :: bnum
            type(c_ptr)                                :: tc_mdb_new2
        end function tc_mdb_new2

        ! bool tcmdbout(TCMDB *mdb, const void *kbuf, int ksiz)
        function tc_mdb_out(mdb, kbuf, ksiz) bind(c, name='tcmdbout')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            logical(kind=c_bool)                   :: tc_mdb_out
        end function tc_mdb_out

        ! bool tcmdbout2(TCMDB *mdb, const char *kstr)
        function tc_mdb_out2_(mdb, kstr) bind(c, name='tcmdbout2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            logical(kind=c_bool)                      :: tc_mdb_out2_
        end function tc_mdb_out2_

        ! bool tcmdbput(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_mdb_put(mdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tcmdbput')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_mdb_put
        end function tc_mdb_put

        ! bool tcmdbput2(TCMDB *mdb, const char *kstr, const char *vstr)
        function tc_mdb_put2_(mdb, kstr, vstr) bind(c, name='tcmdbput2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_mdb_put2_
        end function tc_mdb_put2_

        ! bool tcmdbputcat(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_mdb_put_cat(mdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tcmdbputcat')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_mdb_put_cat
        end function tc_mdb_put_cat

        ! bool tcmdbputcat2(TCMDB *mdb, const char *kstr, const char *vstr)
        function tc_mdb_put_cat2_(mdb, kstr, vstr) bind(c, name='tcmdbputcat2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_mdb_put_cat2_
        end function tc_mdb_put_cat2_

        ! bool tcmdbputkeep(TCMDB *mdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_mdb_put_keep(mdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tcmdbputkeep')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_mdb_put_keep
        end function tc_mdb_put_keep

        ! bool tcmdbputkeep2(TCMDB *mdb, const char *kstr, const char *vstr)
        function tc_mdb_put_keep2_(mdb, kstr, vstr) bind(c, name='tcmdbputkeep2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_mdb_put_keep2_
        end function tc_mdb_put_keep2_

        ! uint64_t tcmdbrnum(TCMDB *mdb)
        function tc_mdb_rnum(mdb) bind(c, name='tcmdbrnum')
            import :: c_int64_t, c_ptr
            type(c_ptr), intent(in), value :: mdb
            integer(kind=c_int64_t)        :: tc_mdb_rnum
        end function tc_mdb_rnum

        ! bool tcmdbvanish(TCMDB *mdb)
        function tc_mdb_vanish(mdb) bind(c, name='tcmdbvanish')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: mdb
            logical(kind=c_bool)           :: tc_mdb_vanish
        end function tc_mdb_vanish

        ! int tcmdbvsiz(TCMDB *mdb, const void *kbuf, int ksiz)
        function tc_mdb_vsiz(mdb, kbuf, ksiz) bind(c, name='tcmdbvsiz')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: mdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            integer(kind=c_int)                    :: tc_mdb_vsiz
        end function tc_mdb_vsiz

        ! int tcmdbvsiz2(TCMDB *mdb, const char *kstr)
        function tc_mdb_vsiz2_(mdb, kstr) bind(c, name='tcmdbvsiz2')
            import :: c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: mdb
            character(kind=c_char), intent(in)        :: kstr
            integer(kind=c_int)                       :: tc_mdb_vsiz2_
        end function tc_mdb_vsiz2_

        ! void tcmdbdel(TCMDB *mdb)
        subroutine tc_mdb_del(mdb) bind(c, name='tcmdbdel')
            import :: c_ptr
            type(c_ptr), intent(in), value :: mdb
        end subroutine tc_mdb_del
    end interface
contains
    ! TCLIST *tcmdbfwmkeys2(TCMDB *mdb, const char *pstr, int max);
    function tc_mdb_fwm_keys2(mdb, pstr, max)
        !! Wrapper for `tc_mdb_fwm_keys2_()` that adds `c_null_char` to `pstr`.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: pstr
        integer,          intent(in) :: max
        type(c_ptr)                  :: tc_mdb_fwm_keys2

        tc_mdb_fwm_keys2 = tc_mdb_fwm_keys2_(mdb, pstr // c_null_char, max)
    end function tc_mdb_fwm_keys2

    ! char *tcmdbget2(TCMDB *mdb, const char *kstr)
    function tc_mdb_get2(mdb, kstr)
        !! Wrapper for `tc_mdb_get2_()` that converts the returned C pointer to
        !! Fortran character.
        type(c_ptr),      intent(in)  :: mdb
        character(len=*), intent(in)  :: kstr
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: tc_mdb_get2
        integer(kind=8)               :: size

        ptr = tc_mdb_get2_(mdb, kstr // c_null_char)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_mdb_get2)
        call c_f_string_ptr(ptr, tc_mdb_get2)
        call c_free(ptr)
    end function tc_mdb_get2

    ! char *tcmdbiternext2(TCMDB *mdb)
    function tc_mdb_iter_next2(mdb)
        !! Wrapper for `tc_mdb_iter_next2()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),      intent(in)  :: mdb
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: tc_mdb_iter_next2
        integer(kind=8)               :: size

        ptr = tc_mdb_iter_next2_(mdb)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_mdb_iter_next2)
        call c_f_string_ptr(ptr, tc_mdb_iter_next2)
        call c_free(ptr)
    end function tc_mdb_iter_next2

    ! bool tcmdbout2(TCMDB *mdb, const char *kstr)
    function tc_mdb_out2(mdb, kstr)
        !! Wrapper for `tc_mdb_out2_()` that adds `c_null_char` to key.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: kstr
        logical                      :: tc_mdb_out2

        tc_mdb_out2 = tc_mdb_out2_(mdb, kstr // c_null_char)
    end function tc_mdb_out2

    ! bool tcmdbput2(TCMDB *mdb, const char *kstr, const char *vstr)
    function tc_mdb_put2(mdb, kstr, vstr)
        !! Wrapper for `tc_mdb_put2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_mdb_put2

        tc_mdb_put2 = tc_mdb_put2_(mdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_mdb_put2

    ! bool tcmdbputcat2(TCMDB *mdb, const char *kstr, const char *vstr)
    function tc_mdb_put_cat2(mdb, kstr, vstr)
        !! Wrapper for `tc_mdb_put_cat2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_mdb_put_cat2

        tc_mdb_put_cat2 = tc_mdb_put_cat2_(mdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_mdb_put_cat2

    ! bool tcmdbputkeep2(TCMDB *mdb, const char *kstr, const char *vstr)
    function tc_mdb_put_keep2(mdb, kstr, vstr)
        !! Wrapper for `tc_mdb_put_keep2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_mdb_put_keep2

        tc_mdb_put_keep2 = tc_mdb_put_keep2_(mdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_mdb_put_keep2

    ! int tcmdbvsiz2(TCMDB *mdb, const char *kstr)
    function tc_mdb_vsiz2(mdb, kstr)
        !! Wrapper for `tc_mdb_vsiz2_()` that adds `c_null_char` to the kstr.
        type(c_ptr),      intent(in) :: mdb
        character(len=*), intent(in) :: kstr
        integer                      :: tc_mdb_vsiz2

        tc_mdb_vsiz2 = tc_mdb_vsiz2_(mdb, kstr // c_null_char)
    end function tc_mdb_vsiz2
end module tcmdb
