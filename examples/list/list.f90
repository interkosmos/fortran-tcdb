! list.f90
!
! Example program that does some list operations.
!
! Author:   Philipp Engel
! Licence:  ISC
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use :: tclist
    implicit none
    type(c_ptr)                   :: list
    character(len=:), allocatable :: str
    integer                       :: i

    ! Create Array List.
    list = tc_list_new()

    ! Add strings to list.
    call tc_list_push2(list, 'foo')
    call tc_list_push2(list, 'bar')
    call tc_list_push2(list, 'qux')

    call tc_list_sort(list)

    ! Traverse list.
    do, i = 0, tc_list_num(list) - 1
        str = tc_list_val2(list, i)
        print '(a)', str
    end do

    ! Delete list.
    call tc_list_del(list)
end program main
