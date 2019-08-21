! hdb.f90
!
! Example program that writes to a hash database `casket.tch` and then reads
! the key-value pairs from it.
!
! Author:   Philipp Engel
! Licence:  ISC
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use :: tchdb
    implicit none
    ! Number of elements of the bucket array. If it is not defined or not more
    ! than 0, the default value is specified. The default value is 131071.
    ! Suggested size of the bucket array is about from 0.5 to 4 times of the
    ! number of all records to be stored.
    integer(kind=8), parameter :: BNUM = 131071

    ! Size of record alignment by power of 2. If it is negative, the default
    ! value is specified. The default value is 4 standing for 2^4 = 16.
    integer(kind=1), parameter :: APOW = 4

    ! Maximum number of elements of the free block is not more than 0, the
    ! default value is specified. The default value is 131071. Suggested size
    ! of the bucket array is about from 0.5 to 4 times of the number of all
    ! records to be stored.
    integer(kind=1), parameter :: FPOW = 10

    ! Options by bitwise-or: `HDB_TLARGE` specifies that the size of the database
    ! can be larger than 2 GB by using 64-bit bucket array, `HDB_TDEFLATE`
    ! specifies that each record is compressed with Deflate encoding, `HDB_TBZIP`
    ! specifies that each record is compressed with BZIP2 encoding, `HDB_TTCBS`
    ! specifies that each record is compressed with TCBS encoding.
    integer(kind=1), parameter :: OPTS = int(HDB_TLARGE, kind=1)

    ! Maximum number of records to be cached. If it is not more than 0, the
    ! record cache is disabled. It is disabled by default.
    integer(kind=4), parameter :: RCNUM = 0

    type(c_ptr)                   :: hdb
    integer                       :: ecode
    integer                       :: size
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value

    ! Create hash database handle.
    hdb = tc_hdb_new()

    ! Optional tuning.
    if (.not. tc_hdb_tune(hdb, BNUM, APOW, FPOW, OPTS)) then
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Optional setting of cache size.
    if (.not. tc_hdb_set_cache(hdb, RCNUM)) then
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Open hash database file.
    if (.not. tc_hdb_open(hdb, 'casket.tch', ior(HDB_OWRITER, HDB_OCREAT))) then
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Output database file name.
    print '(2a, /)', 'File: ', tc_hdb_path(hdb)

    ! Put key-value pairs into database.
    if (.not. tc_hdb_put2(hdb, 'foo', 'hop') .or. &
        .not. tc_hdb_put2(hdb, 'bar', 'step') .or. &
        .not. tc_hdb_put2(hdb, 'qux', 'roll') .or. &
        .not. tc_hdb_put2(hdb, 'baz', 'jump')) then
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Get single value and its size.
    value = tc_hdb_get2(hdb, 'foo')
    size  = tc_hdb_vsiz2(hdb, 'foo')

    ! Print value.
    if (len(value) > 0) then
        print '(3a, i0, a, /)', 'value: ', value, ' (length: ', size, ')'
    else
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Iterate over whole hash database and print key-value pairs.
    if (tc_hdb_iter_init(hdb)) then
        do
            key = tc_hdb_iter_next2(hdb)

            if (len(key) == 0) &
                exit

            value = tc_hdb_get2(hdb, key)
            print '(3a)', key, ': ', value
        end do
    end if

    ! Close hash database handle.
    if (.not. tc_hdb_close(hdb)) then
        ecode = tc_hdb_ecode(hdb)
        print '(2a)', 'Error: ', tc_hdb_err_msg(ecode)
    end if

    ! Delete hash database handle.
    call tc_hdb_del(hdb)
end program main
