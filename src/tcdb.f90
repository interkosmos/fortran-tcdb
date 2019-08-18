! tcdb.f90
!
! Fortran 2008 interface to Tokyo Cabinet 1.4.48.
!
! Author:   Philipp Engel
! Licence:  ISC
module tcdb
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_bool, &
                                           c_char, c_int, c_int8_t, c_int32_t, &
                                           c_int64_t, c_null_char, c_ptr, &
                                           c_size_t
    implicit none
    private

    public :: TC_ESUCCESS
    public :: TC_ETHREAD
    public :: TC_EINVALID
    public :: TC_ENOFILE
    public :: TC_ENOPERM
    public :: TC_EMETA
    public :: TC_ERHEAD
    public :: TC_EOPEN
    public :: TC_ECLOSE
    public :: TC_ETRUNC
    public :: TC_ESYNC
    public :: TC_ESTAT
    public :: TC_ESEEK
    public :: TC_EREAD
    public :: TC_EWRITE
    public :: TC_EMMAP
    public :: TC_ELOCK
    public :: TC_EUNLINK
    public :: TC_ERENAME
    public :: TC_EMKDIR
    public :: TC_ERMDIR
    public :: TC_EKEEP
    public :: TC_ENOREC
    public :: TC_EMISC

    public :: TCDB_THASH
    public :: TCDB_TBTREE
    public :: TCDB_TFIXED
    public :: TCDB_TTABLE

    public :: HDB_TLARGE
    public :: HDB_TDEFLATE
    public :: HDB_TBZIP
    public :: HDB_TTCBS
    public :: HDB_TEXCODEC

    public :: HDB_OREADER
    public :: HDB_OWRITER
    public :: HDB_OCREAT
    public :: HDB_OTRUNC
    public :: HDB_ONOLCK
    public :: HDB_OLCKNB
    public :: HDB_OTSYNC

    public :: tc_hdb_close
    public :: tc_hdb_copy
    public :: tc_hdb_del
    public :: tc_hdb_ecode
    public :: tc_hdb_err_msg
    public :: tc_hdb_fsiz
    public :: tc_hdb_get2
    public :: tc_hdb_iter_init
    public :: tc_hdb_iter_next2
    public :: tc_hdb_new
    public :: tc_hdb_open
    public :: tc_hdb_optimize
    public :: tc_hdb_put2
    public :: tc_hdb_rnum
    public :: tc_hdb_set_cache
    public :: tc_hdb_set_mutex
    public :: tc_hdb_sync
    public :: tc_hdb_tran_abort
    public :: tc_hdb_tran_begin
    public :: tc_hdb_tran_commit
    public :: tc_hdb_tune
    public :: tc_hdb_vanish
    public :: tc_hdb_vsiz2

    ! Enumerations.
    enum, bind(c)
        enumerator :: TC_ESUCCESS       ! success
        enumerator :: TC_ETHREAD        ! threading error
        enumerator :: TC_EINVALID       ! invalid operation
        enumerator :: TC_ENOFILE        ! file not found
        enumerator :: TC_ENOPERM        ! no permission
        enumerator :: TC_EMETA          ! invalid meta data
        enumerator :: TC_ERHEAD         ! invalid record header
        enumerator :: TC_EOPEN          ! open error
        enumerator :: TC_ECLOSE         ! close error
        enumerator :: TC_ETRUNC         ! trunc error
        enumerator :: TC_ESYNC          ! sync error
        enumerator :: TC_ESTAT          ! stat error
        enumerator :: TC_ESEEK          ! seek error
        enumerator :: TC_EREAD          ! read error
        enumerator :: TC_EWRITE         ! write error
        enumerator :: TC_EMMAP          ! mmap error
        enumerator :: TC_ELOCK          ! lock error
        enumerator :: TC_EUNLINK        ! unlink error
        enumerator :: TC_ERENAME        ! rename error
        enumerator :: TC_EMKDIR         ! mkdir error
        enumerator :: TC_ERMDIR         ! rmdir error
        enumerator :: TC_EKEEP          ! existing record
        enumerator :: TC_ENOREC         ! no record found
        enumerator :: TC_EMISC = 9999   ! miscellaneous error
    end enum

    enum, bind(c)
        enumerator :: TCDB_THASH        ! hash table
        enumerator :: TCDB_TBTREE       ! B+ tree
        enumerator :: TCDB_TFIXED       ! fixed-length
        enumerator :: TCDB_TTABLE       ! table
    end enum

    enum, bind(c)
        enumerator :: HDB_TLARGE   = shiftl(1, 0)
        enumerator :: HDB_TDEFLATE = shiftl(1, 1)
        enumerator :: HDB_TBZIP    = shiftl(1, 2)
        enumerator :: HDB_TTCBS    = shiftl(1, 3)
        enumerator :: HDB_TEXCODEC = shiftl(1, 4)
    end enum

    enum, bind(c)
        enumerator :: HDB_OREADER = shiftl(1, 0)
        enumerator :: HDB_OWRITER = shiftl(1, 1)
        enumerator :: HDB_OCREAT  = shiftl(1, 2)
        enumerator :: HDB_OTRUNC  = shiftl(1, 3)
        enumerator :: HDB_ONOLCK  = shiftl(1, 4)
        enumerator :: HDB_OLCKNB  = shiftl(1, 5)
        enumerator :: HDB_OTSYNC  = shiftl(1, 6)
    end enum

    ! Interface functions and routines to libc.
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

    ! Interface functions and routines to Tokyo Cabinet.
    interface
        ! bool tchdbclose(TCHDB *hdb)
        function tc_hdb_close(hdb) bind(c, name='tchdbclose')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_close
        end function tc_hdb_close

        ! bool tchdbcopy(TCHDB *hdb, const char *path)
        function tc_hdb_copy_(hdb, path) bind(c, name='tchdbcopy')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: path
            logical(kind=c_bool)                      :: tc_hdb_copy_
        end function tc_hdb_copy_

        ! int tchdbecode(TCHDB *hdb)
        function tc_hdb_ecode(hdb) bind(c, name='tchdbecode')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: hdb
            integer(kind=c_int)            :: tc_hdb_ecode
        end function tc_hdb_ecode

        ! const char *tchdberrmsg(int ecode)
        function tc_hdb_err_msg_(ecode) bind(c, name='tchdberrmsg')
            import:: c_int, c_ptr
            integer(kind=c_int), intent(in), value :: ecode
            type(c_ptr)                            :: tc_hdb_err_msg_
        end function tc_hdb_err_msg_

        ! uint64_t tchdbfsiz(TCHDB *hdb)
        function tc_hdb_fsiz(hdb) bind(c, name='tchdbrnum')
            import :: c_int64_t, c_ptr
            type(c_ptr), intent(in), value :: hdb
            integer(kind=c_int64_t)        :: tc_hdb_fsiz
        end function tc_hdb_fsiz

        ! char *tchdbget2(TCHDB *hdb, const char *kstr)
        function tc_hdb_get2_(hdb, kstr) bind(c, name='tchdbget2')
            import :: c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            type(c_ptr)                               :: tc_hdb_get2_
        end function tc_hdb_get2_

        ! bool tchdbiterinit(TCHDB *hdb)
        function tc_hdb_iter_init(hdb) bind(c, name='tchdbiterinit')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_iter_init
        end function tc_hdb_iter_init

        ! char *tchdbiternext2(TCHDB *hdb)
        function tc_hdb_iter_next2_(hdb) bind(c, name='tchdbiternext2')
            import :: c_ptr
            type(c_ptr), intent(in), value :: hdb
            type(c_ptr)                    :: tc_hdb_iter_next2_
        end function tc_hdb_iter_next2_

        ! TCMAP *tchdbnew(void)
        function tc_hdb_new() bind(c, name='tchdbnew')
            import :: c_ptr
            type(c_ptr) :: tc_hdb_new
        end function tc_hdb_new

        ! bool tchdbopen(TCHDB *hdb, const char *path, int omode)
        function tc_hdb_open_(hdb, path, omode) bind(c, name='tchdbopen')
            import :: c_bool, c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: path
            integer(kind=c_int),    intent(in), value :: omode
            logical(kind=c_bool)                      :: tc_hdb_open_
        end function tc_hdb_open_

        ! bool tchdboptimize(TCHDB *hdb, int64_t bnum, int8_t apow, int8_t fpow, uint8_t opts)
        function tc_hdb_optimize(hdb, bnum, apow, fpow, opts) bind(c, name='tchdboptimize')
            import :: c_bool, c_int8_t, c_int64_t, c_ptr
            type(c_ptr),             intent(in), value :: hdb
            integer(kind=c_int64_t), intent(in), value :: bnum
            integer(kind=c_int8_t),  intent(in), value :: apow
            integer(kind=c_int8_t),  intent(in), value :: fpow
            integer(kind=c_int8_t),  intent(in), value :: opts
            logical(kind=c_bool)                       :: tc_hdb_optimize
        end function tc_hdb_optimize

        ! bool tchdbput2(TCHDB *hdb, const char *kstr, const char *vstr)
        function tc_hdb_put2_(hdb, kstr, vstr) bind(c, name='tchdbput2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_hdb_put2_
        end function tc_hdb_put2_

        ! uint64_t tchdbrnum(TCHDB *hdb)
        function tc_hdb_rnum(hdb) bind(c, name='tchdbrnum')
            import :: c_int64_t, c_ptr
            type(c_ptr), intent(in), value :: hdb
            integer(kind=c_int64_t)        :: tc_hdb_rnum
        end function tc_hdb_rnum

        ! bool tchdbsetcache(TCHDB *hdb, int32_t rcnum)
        function tc_hdb_set_cache(hdb, rcnum) bind(c, name='tchdbsetcache')
            import :: c_bool, c_int32_t, c_ptr
            type(c_ptr),             intent(in), value :: hdb
            integer(kind=c_int32_t), intent(in), value :: rcnum
            logical(kind=c_bool)                       :: tc_hdb_set_cache
        end function tc_hdb_set_cache

        ! bool tchdbsetmutex(TCHDB *hdb)
        function tc_hdb_set_mutex(hdb) bind(c, name='tchdbsetmutex')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_set_mutex
        end function tc_hdb_set_mutex

        ! bool tchdbsync(TCHDB *hdb)
        function tc_hdb_sync(hdb) bind(c, name='tchdbsync')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_sync
        end function tc_hdb_sync

        ! bool tchdbtranabort(TCHDB *hdb)
        function tc_hdb_tran_abort(hdb) bind(c, name='tchdbtranabort')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_tran_abort
        end function tc_hdb_tran_abort

        ! bool tchdbtranbegin(TCHDB *hdb)
        function tc_hdb_tran_begin(hdb) bind(c, name='tchdbtranbegin')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_tran_begin
        end function tc_hdb_tran_begin

        ! bool tchdbtrancommit(TCHDB *hdb)
        function tc_hdb_tran_commit(hdb) bind(c, name='tchdbtrancommit')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_tran_commit
        end function tc_hdb_tran_commit

        ! bool tchdbtune(TCHDB *hdb, int64_t bnum, int8_t apow, int8_t fpow, uint8_t opts)
        function tc_hdb_tune(hdb, bnum, apow, fpow, opts) bind(c, name='tchdbtune')
            import :: c_bool, c_int8_t, c_int64_t, c_ptr
            type(c_ptr),             intent(in), value :: hdb
            integer(kind=c_int64_t), intent(in), value :: bnum
            integer(kind=c_int8_t),  intent(in), value :: apow
            integer(kind=c_int8_t),  intent(in), value :: fpow
            integer(kind=c_int8_t),  intent(in), value :: opts
            logical(kind=c_bool)                       :: tc_hdb_tune
        end function tc_hdb_tune

        ! bool tchdbvanish(TCHDB *hdb)
        function tc_hdb_vanish(hdb) bind(c, name='tchdbvanish')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_vanish
        end function tc_hdb_vanish

        ! int tchdbvsiz2(TCHDB *hdb, const char *kstr)
        function tc_hdb_vsiz2_(hdb, kstr) bind(c, name='tchdbvsiz2')
            import :: c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            integer(kind=c_int)                       :: tc_hdb_vsiz2_
        end function tc_hdb_vsiz2_

        ! void tchdbdel(TCHDB *hdb)
        subroutine tc_hdb_del(hdb) bind(c, name='tchdbdel')
            import :: c_ptr
            type(c_ptr), intent(in), value :: hdb
        end subroutine tc_hdb_del
    end interface
contains
    ! const char *tchdberrmsg(int ecode)
    function tc_hdb_err_msg(ecode)
        !! Wrapper for `tc_hdb_err_msg_()` that converts the returned C pointer
        !! to Fortran character.
        integer,          intent(in)  :: ecode
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: tc_hdb_err_msg
        integer(kind=8)               :: size

        ptr = tc_hdb_err_msg_(ecode)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_hdb_err_msg)
        call c_f_string_ptr(ptr, tc_hdb_err_msg)
    end function tc_hdb_err_msg

    ! bool tchdbcopy(TCHDB *hdb, const char *path)
    function tc_hdb_copy(hdb, path)
        !! Wrapper for `tc_hdb_copy_()` that adds `c_null_char` to `path`.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: path
        logical                      :: tc_hdb_copy

        tc_hdb_copy = tc_hdb_copy_(hdb, path // c_null_char)
    end function tc_hdb_copy

    ! char *tchdbget2(TCHDB *hdb, const char *kstr)
    function tc_hdb_get2(hdb, kstr)
        !! Wrapper for `tc_hdb_get2_()` that converts the returned C pointer to
        !! Fortran character.
        type(c_ptr),      intent(in)  :: hdb
        character(len=*), intent(in)  :: kstr
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: tc_hdb_get2
        integer(kind=8)               :: size

        ptr = tc_hdb_get2_(hdb, kstr // c_null_char)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_hdb_get2)
        call c_f_string_ptr(ptr, tc_hdb_get2)
        call c_free(ptr)
    end function tc_hdb_get2

    ! char *tchdbiternext2(TCHDB *hdb)
    function tc_hdb_iter_next2(hdb)
        !! Wrapper for `tc_hdb_iter_next2()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),      intent(in)  :: hdb
        type(c_ptr)                   :: ptr
        character(len=:), allocatable :: tc_hdb_iter_next2
        integer(kind=8)               :: size

        ptr = tc_hdb_iter_next2_(hdb)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_hdb_iter_next2)
        call c_f_string_ptr(ptr, tc_hdb_iter_next2)
        call c_free(ptr)
    end function tc_hdb_iter_next2

    ! bool tchdbopen(TCHDB *hdb, const char *path, int omode)
    function tc_hdb_open(hdb, path, omode)
        !! Wrapper for `tc_hdb_open_()` that adds `c_null_char` to `path`.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: path
        integer,          intent(in) :: omode
        logical                      :: tc_hdb_open

        tc_hdb_open = tc_hdb_open_(hdb, path // c_null_char, omode)
    end function tc_hdb_open

    ! bool tchdbput2(TCHDB *hdb, const char *kstr, const char *vstr)
    function tc_hdb_put2(hdb, kstr, vstr)
        !! Wrapper for `tc_hdb_put2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_hdb_put2

        tc_hdb_put2 = tc_hdb_put2_(hdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_hdb_put2

    ! int tchdbvsiz2(TCHDB *hdb, const char *kstr)
    function tc_hdb_vsiz2(hdb, kstr)
        !! Wrapper for `tc_hdb_vsiz2_()` that adds `c_null_char` to the key.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        integer                      :: tc_hdb_vsiz2

        tc_hdb_vsiz2 = tc_hdb_vsiz2_(hdb, kstr // c_null_char)
    end function tc_hdb_vsiz2

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
