! mdb.f90
!
! Example program that writes to and reads from an on-memory hash database.
!
! Author:   Philipp Engel
! Licence:  ISC
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use :: tcmdb
    implicit none
    type(c_ptr)                   :: mdb
    integer                       :: size
    character(len=:), allocatable :: key
    character(len=:), allocatable :: value

    ! Create on-memory hash database handle.
    mdb = tc_mdb_new()

    ! Put key-value pairs into database.
    call tc_mdb_put2(mdb, 'foo', 'hop')
    call tc_mdb_put2(mdb, 'bar', 'step')
    call tc_mdb_put2(mdb, 'qux', 'roll')
    call tc_mdb_put2(mdb, 'baz', 'jump')

    ! Get single value and its size.
    value = tc_mdb_get2(mdb, 'foo')
    size  = tc_mdb_vsiz2(mdb, 'foo')

    ! Print value.
    print '(3a, i0, a, /)', 'value: ', value, ' (length: ', size, ')'

    ! Iterate over whole hash database and print key-value pairs.
    call tc_mdb_iter_init(mdb)

    do
        key = tc_mdb_iter_next2(mdb)

        if (len(key) == 0) &
            exit

        value = tc_mdb_get2(mdb, key)
        print '(3a)', key, ': ', value
    end do

    ! Clear hash database handle.
    call tc_mdb_vanish(mdb)
end program main
