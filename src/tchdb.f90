! tchdb.f90
!
! Fortran 2008 interface to the hash database API of Tokyo Cabinet.
!
! Author:   Philipp Engel
! Licence:  ISC
module tchdb
    use, intrinsic :: iso_c_binding, only: c_associated, c_double, &
                                           c_f_pointer, c_bool, c_char, c_int, &
                                           c_int8_t, c_int32_t, c_int64_t, &
                                           c_null_char, c_ptr, c_size_t
    use :: tcutil
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

    ! Interfaces and wrapper routines.
    public :: tc_hdb_add_double
    public :: tc_hdb_add_int
    public :: tc_hdb_close
    public :: tc_hdb_copy
    public :: tc_hdb_copy_
    public :: tc_hdb_del
    public :: tc_hdb_ecode
    public :: tc_hdb_err_msg
    public :: tc_hdb_fsiz
    public :: tc_hdb_fwm_keys
    public :: tc_hdb_fwm_keys2
    public :: tc_hdb_fwm_keys2_
    public :: tc_hdb_get
    public :: tc_hdb_get2
    public :: tc_hdb_get2_
    public :: tc_hdb_get3
    public :: tc_hdb_iter_init
    public :: tc_hdb_iter_next
    public :: tc_hdb_iter_next2
    public :: tc_hdb_iter_next3
    public :: tc_hdb_new
    public :: tc_hdb_open
    public :: tc_hdb_open_
    public :: tc_hdb_optimize
    public :: tc_hdb_out
    public :: tc_hdb_out2
    public :: tc_hdb_out2_
    public :: tc_hdb_path
    public :: tc_hdb_put
    public :: tc_hdb_put2
    public :: tc_hdb_put2_
    public :: tc_hdb_put_async
    public :: tc_hdb_put_async2
    public :: tc_hdb_put_async2_
    public :: tc_hdb_put_cat
    public :: tc_hdb_put_cat2
    public :: tc_hdb_put_cat2_
    public :: tc_hdb_put_keep
    public :: tc_hdb_put_keep2
    public :: tc_hdb_put_keep2_
    public :: tc_hdb_rnum
    public :: tc_hdb_set_cache
    public :: tc_hdb_set_dfunit
    public :: tc_hdb_set_mutex
    public :: tc_hdb_set_xmsiz
    public :: tc_hdb_sync
    public :: tc_hdb_tran_abort
    public :: tc_hdb_tran_begin
    public :: tc_hdb_tran_commit
    public :: tc_hdb_tune
    public :: tc_hdb_vanish
    public :: tc_hdb_vsiz
    public :: tc_hdb_vsiz2
    public :: tc_hdb_vsiz2_

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

    ! Function and routine interfaces to Tokyo Cabinet.
    interface
        ! double tchdbadddouble(TCHDB *hdb, const void *kbuf, int ksiz, double num)
        function tc_hdb_add_double(hdb, kbuf, ksiz, num) bind(c, name='tchdbadddouble')
            import ::  c_double, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            real(kind=c_double), intent(in), value :: num
            real(kind=c_double)                    :: tc_hdb_add_double
        end function tc_hdb_add_double

        ! int tchdbaddint(TCHDB *hdb, const void *kbuf, int ksiz, int num)
        function tc_hdb_add_int(hdb, kbuf, ksiz, num) bind(c, name='tchdbaddint')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            integer(kind=c_int), intent(in), value :: num
            integer(kind=c_int)                    :: tc_hdb_add_int
        end function tc_hdb_add_int

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

        ! TCLIST *tchdbfwmkeys(TCHDB *hdb, const void *pbuf, int psiz, int max)
        function tc_hdb_fwm_keys(hdb, pbuf, psiz, max) bind(c, name='tchdbfwmkeys')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: pbuf
            integer(kind=c_int), intent(in), value :: psiz
            integer(kind=c_int), intent(in), value :: max
            type(c_ptr)                            :: tc_hdb_fwm_keys
        end function tc_hdb_fwm_keys

        ! TCLIST *tchdbfwmkeys2(TCHDB *hdb, const char *pstr, int max);
        function tc_hdb_fwm_keys2_(hdb, pstr, max) bind(c, name='tchdbfwmkeys2')
            import ::  c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: pstr
            integer(kind=c_int),    intent(in), value :: max
            type(c_ptr)                               :: tc_hdb_fwm_keys2_
        end function tc_hdb_fwm_keys2_

        ! void *tchdbget(TCHDB *hdb, const void *kbuf, int ksiz, int *sp)
        function tc_hdb_get(hdb, kbuf, ksiz) bind(c, name='tchdbget')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr)                            :: tc_hdb_get
        end function tc_hdb_get

        ! char *tchdbget2(TCHDB *hdb, const char *kstr)
        function tc_hdb_get2_(hdb, kstr) bind(c, name='tchdbget2')
            import :: c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            type(c_ptr)                               :: tc_hdb_get2_
        end function tc_hdb_get2_

        ! int tchdbget3(TCHDB *hdb, const void *kbuf, int ksiz, void *vbuf, int max)
        function tc_hdb_get3(hdb, kbuf, ksiz, vbuf, max) bind(c, name='tchdbget3')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: max
            type(c_ptr)                            :: tc_hdb_get3
        end function tc_hdb_get3

        ! bool tchdbiterinit(TCHDB *hdb)
        function tc_hdb_iter_init(hdb) bind(c, name='tchdbiterinit')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_iter_init
        end function tc_hdb_iter_init

        ! void *tchdbiternext(TCHDB *hdb, int *sp)
        function tc_hdb_iter_next(hdb, sp) bind(c, name='tchdbiternext')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_hdb_iter_next
        end function tc_hdb_iter_next

        ! char *tchdbiternext2(TCHDB *hdb)
        function tc_hdb_iter_next2_(hdb) bind(c, name='tchdbiternext2')
            import :: c_ptr
            type(c_ptr), intent(in), value :: hdb
            type(c_ptr)                    :: tc_hdb_iter_next2_
        end function tc_hdb_iter_next2_

        ! bool tchdbiternext3(TCHDB *hdb, TCXSTR *kxstr, TCXSTR *vxstr)
        function tc_hdb_iter_next3(hdb, kxstr, vxstr) bind(c, name='tchdbiternext3')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            type(c_ptr), intent(in), value :: kxstr
            type(c_ptr), intent(in), value :: vxstr
            logical(kind=c_bool)           :: tc_hdb_iter_next3
        end function tc_hdb_iter_next3

        ! TCHDB *tchdbnew(void)
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

        ! bool tchdbout(TCHDB *hdb, const void *kbuf, int ksiz)
        function tc_hdb_out(hdb, kbuf, ksiz) bind(c, name='tchdbout')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            logical(kind=c_bool)                   :: tc_hdb_out
        end function tc_hdb_out

        ! bool tchdbout2(TCHDB *hdb, const char *kstr)
        function tc_hdb_out2_(hdb, kstr) bind(c, name='tchdbout2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            logical(kind=c_bool)                      :: tc_hdb_out2_
        end function tc_hdb_out2_

        ! const char *tchdbpath(TCHDB *hdb)
        function tc_hdb_path_(hdb) bind(c, name='tchdbpath')
            import :: c_ptr
            type(c_ptr), intent(in), value :: hdb
            type(c_ptr)                    :: tc_hdb_path_
        end function tc_hdb_path_

        ! bool tchdbput(TCHDB *hdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_hdb_put(hdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tchdbput')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_hdb_put
        end function tc_hdb_put

        ! bool tchdbput2(TCHDB *hdb, const char *kstr, const char *vstr)
        function tc_hdb_put2_(hdb, kstr, vstr) bind(c, name='tchdbput2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_hdb_put2_
        end function tc_hdb_put2_

        ! bool tchdbputasync(TCHDB *hdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_hdb_put_async(hdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tchdbputasync')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_hdb_put_async
        end function tc_hdb_put_async

        ! bool tchdbputasync2(TCHDB *hdb, const char *kstr, const char *vstr)
        function tc_hdb_put_async2_(hdb, kstr, vstr) bind(c, name='tchdbputasync2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_hdb_put_async2_
        end function tc_hdb_put_async2_

        ! bool tchdbputcat(TCHDB *hdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_hdb_put_cat(hdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tchdbputcat')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_hdb_put_cat
        end function tc_hdb_put_cat

        ! bool tchdbputcat2(TCHDB *hdb, const char *kstr, const char *vstr)
        function tc_hdb_put_cat2_(hdb, kstr, vstr) bind(c, name='tchdbputcat2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_hdb_put_cat2_
        end function tc_hdb_put_cat2_

        ! bool tchdbputkeep(TCHDB *hdb, const void *kbuf, int ksiz, const void *vbuf, int vsiz)
        function tc_hdb_put_keep(hdb, kbuf, ksiz, vbuf, vsiz) bind(c, name='tchdbputkeep')
            import :: c_bool, c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            type(c_ptr),         intent(in), value :: vbuf
            integer(kind=c_int), intent(in), value :: vsiz
            logical(kind=c_bool)                   :: tc_hdb_put_keep
        end function tc_hdb_put_keep

        ! bool tchdbputkeep2(TCHDB *hdb, const char *kstr, const char *vstr)
        function tc_hdb_put_keep2_(hdb, kstr, vstr) bind(c, name='tchdbputkeep2')
            import :: c_bool, c_char, c_ptr
            type(c_ptr),            intent(in), value :: hdb
            character(kind=c_char), intent(in)        :: kstr
            character(kind=c_char), intent(in)        :: vstr
            logical(kind=c_bool)                      :: tc_hdb_put_keep2_
        end function tc_hdb_put_keep2_

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

        ! bool tchdbsetdfunit(TCHDB *hdb, int32_t dfunit)
        function tc_hdb_set_dfunit(hdb, dfunit) bind(c, name='tchdbsetdfunit')
            import :: c_bool, c_int32_t, c_ptr
            type(c_ptr),             intent(in), value :: hdb
            integer(kind=c_int32_t), intent(in), value :: dfunit
            logical(kind=c_bool)                       :: tc_hdb_set_dfunit
        end function tc_hdb_set_dfunit

        ! bool tchdbsetmutex(TCHDB *hdb)
        function tc_hdb_set_mutex(hdb) bind(c, name='tchdbsetmutex')
            import :: c_bool, c_ptr
            type(c_ptr), intent(in), value :: hdb
            logical(kind=c_bool)           :: tc_hdb_set_mutex
        end function tc_hdb_set_mutex

        ! bool tchdbsetxmsiz(TCHDB *hdb, int64_t xmsiz)
        function tc_hdb_set_xmsiz(hdb, xmsiz) bind(c, name='tchdbsetxmsiz')
            import :: c_bool, c_int64_t, c_ptr
            type(c_ptr),             intent(in), value :: hdb
            integer(kind=c_int64_t), intent(in), value :: xmsiz
            logical(kind=c_bool)                       :: tc_hdb_set_xmsiz
        end function tc_hdb_set_xmsiz

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

        ! int tchdbvsiz(TCHDB *hdb, const void *kbuf, int ksiz)
        function tc_hdb_vsiz(hdb, kbuf, ksiz) bind(c, name='tchdbvsiz')
            import ::  c_int, c_ptr
            type(c_ptr),         intent(in), value :: hdb
            type(c_ptr),         intent(in), value :: kbuf
            integer(kind=c_int), intent(in), value :: ksiz
            integer(kind=c_int)                    :: tc_hdb_vsiz
        end function tc_hdb_vsiz

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
        character(len=:), allocatable :: tc_hdb_err_msg
        type(c_ptr)                   :: ptr
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

    ! TCLIST *tchdbfwmkeys2(TCHDB *hdb, const char *pstr, int max);
    function tc_hdb_fwm_keys2(hdb, pstr, max)
        !! Wrapper for `tc_hdb_fwm_keys2_()` that adds `c_null_char` to `pstr`.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: pstr
        integer,          intent(in) :: max
        type(c_ptr)                  :: tc_hdb_fwm_keys2

        tc_hdb_fwm_keys2 = tc_hdb_fwm_keys2_(hdb, pstr // c_null_char, max)
    end function tc_hdb_fwm_keys2

    ! char *tchdbget2(TCHDB *hdb, const char *kstr)
    function tc_hdb_get2(hdb, kstr)
        !! Wrapper for `tc_hdb_get2_()` that converts the returned C pointer to
        !! Fortran character.
        type(c_ptr),      intent(in)  :: hdb
        character(len=*), intent(in)  :: kstr
        character(len=:), allocatable :: tc_hdb_get2
        type(c_ptr)                   :: ptr
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
        !! Wrapper for `tc_hdb_iter_next2_()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),      intent(in)  :: hdb
        character(len=:), allocatable :: tc_hdb_iter_next2
        type(c_ptr)                   :: ptr
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

    ! bool tchdbout2(TCHDB *hdb, const char *kstr)
    function tc_hdb_out2(hdb, kstr)
        !! Wrapper for `tc_hdb_out2_()` that adds `c_null_char` to key.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        logical                      :: tc_hdb_out2

        tc_hdb_out2 = tc_hdb_out2_(hdb, kstr // c_null_char)
    end function tc_hdb_out2

    ! const char *tchdbpath(TCHDB *hdb)
    function tc_hdb_path(hdb)
        !! Wrapper for `tc_hdb_path_()` that converts the returned C pointer to
        !! Fortran character.
        type(c_ptr),      intent(in)  :: hdb
        character(len=:), allocatable :: tc_hdb_path
        type(c_ptr)                   :: ptr
        integer(kind=8)               :: size

        ptr = tc_hdb_path_(hdb)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_hdb_path)
        call c_f_string_ptr(ptr, tc_hdb_path)
        call c_free(ptr)
    end function tc_hdb_path

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

    ! bool tchdbputasync2(TCHDB *hdb, const char *kstr, const char *vstr)
    function tc_hdb_put_async2(hdb, kstr, vstr)
        !! Wrapper for `tc_hdb_put_async2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_hdb_put_async2

        tc_hdb_put_async2 = tc_hdb_put_async2_(hdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_hdb_put_async2

    ! bool tchdbputcat2(TCHDB *hdb, const char *kstr, const char *vstr)
    function tc_hdb_put_cat2(hdb, kstr, vstr)
        !! Wrapper for `tc_hdb_put_cat2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_hdb_put_cat2

        tc_hdb_put_cat2 = tc_hdb_put_cat2_(hdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_hdb_put_cat2

    ! bool tchdbputkeep2(TCHDB *hdb, const char *kstr, const char *vstr)
    function tc_hdb_put_keep2(hdb, kstr, vstr)
        !! Wrapper for `tc_hdb_put_keep2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        character(len=*), intent(in) :: vstr
        logical                      :: tc_hdb_put_keep2

        tc_hdb_put_keep2 = tc_hdb_put_keep2_(hdb, kstr // c_null_char, vstr // c_null_char)
    end function tc_hdb_put_keep2

    ! int tchdbvsiz2(TCHDB *hdb, const char *kstr)
    function tc_hdb_vsiz2(hdb, kstr)
        !! Wrapper for `tc_hdb_vsiz2_()` that adds `c_null_char` to the kstr.
        type(c_ptr),      intent(in) :: hdb
        character(len=*), intent(in) :: kstr
        integer                      :: tc_hdb_vsiz2

        tc_hdb_vsiz2 = tc_hdb_vsiz2_(hdb, kstr // c_null_char)
    end function tc_hdb_vsiz2
end module tchdb
