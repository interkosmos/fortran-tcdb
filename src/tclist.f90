! tclist.f90
!
! Fortran 2008 interface to the array list API of Tokyo Cabinet.
!
! Author:   Philipp Engel
! Licence:  ISC
module tclist
    use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_char, &
                                           c_int, c_null_char, c_ptr
    use :: cutil
    implicit none
    private

    public :: tc_list_bsearch
    public :: tc_list_clear
    public :: tc_list_del
    public :: tc_list_dump
    public :: tc_list_dup
    public :: tc_list_insert
    public :: tc_list_insert2
    public :: tc_list_insert2_
    public :: tc_list_load
    public :: tc_list_lsearch
    public :: tc_list_new
    public :: tc_list_new2
    public :: tc_list_num
    public :: tc_list_over
    public :: tc_list_over2
    public :: tc_list_over2_
    public :: tc_list_pop
    public :: tc_list_pop2
    public :: tc_list_push
    public :: tc_list_push2
    public :: tc_list_push2_
    public :: tc_list_remove
    public :: tc_list_remove2
    public :: tc_list_remove2_
    public :: tc_list_shift
    public :: tc_list_shift2
    public :: tc_list_shift2_
    public :: tc_list_sort
    public :: tc_list_unshift
    public :: tc_list_unshift2
    public :: tc_list_unshift2_
    public :: tc_list_val
    public :: tc_list_val2
    public :: tc_list_val2_

    ! Function and routine interfaces to Tokyo Cabinet.
    interface
        ! int tclistbsearch(const TCLIST *list, const void *ptr, int size)
        function tc_list_bsearch(list, ptr, size) bind(c, name='tclistbsearch')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
            integer(kind=c_int)                    :: tc_list_bsearch
        end function tc_list_bsearch

        ! void *tclistdump(const TCLIST *list, int *sp)
        function tc_list_dump(list, sp) bind(c, name='tclistdump')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_list_dump
        end function tc_list_dump

        ! TCLIST *tclistdup(const TCLIST *list)
        function tc_list_dup(list) bind(c, name='tclistdup')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
            type(c_ptr)                    :: tc_list_dup
        end function tc_list_dup

        ! TCLIST *tclistload(const void *ptr, int size)
        function tc_list_load(ptr, size) bind(c, name='tclistload')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
            type(c_ptr)                            :: tc_list_load
        end function tc_list_load

        ! int tclistlsearch(const TCLIST *list, const void *ptr, int size)
        function tc_list_lsearch(list, ptr, size) bind(c, name='tclistlsearch')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
            integer(kind=c_int)                    :: tc_list_lsearch
        end function tc_list_lsearch

        ! TCLIST *tclistnew(void)
        function tc_list_new() bind(c, name='tclistnew')
            import :: c_ptr
            type(c_ptr) :: tc_list_new
        end function tc_list_new

        ! TCLIST *tclistnew2(int anum)
        function tc_list_new2(anum) bind(c, name='tclistnew2')
            import :: c_int, c_ptr
            integer(kind=c_int), intent(in), value :: anum
            type(c_ptr)                            :: tc_list_new2
        end function tc_list_new2

        ! int tclistnum(const TCLIST *list)
        function tc_list_num(list) bind(c, name='tclistnum')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: list
            integer(kind=c_int)            :: tc_list_num
        end function tc_list_num

        ! char *tclistpop2(TCLIST *list)
        function tc_list_pop2_(list) bind(c, name='tclistpop2')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
            type(c_ptr)                    :: tc_list_pop2_
        end function tc_list_pop2_

        ! void *tclistremove(TCLIST *list, int index, int *sp)
        function tc_list_remove(list, index, sp) bind(c, name='tclistremove')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_list_remove
        end function tc_list_remove

        ! char *tclistremove2(TCLIST *list, int index)
        function tc_list_remove2_(list, index) bind(c, name='tclistremove2')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            type(c_ptr)                            :: tc_list_remove2_
        end function tc_list_remove2_

        ! void *tclistshift(TCLIST *list, int *sp)
        function tc_list_shift(list, sp) bind(c, name='tclistshift')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_list_shift
        end function tc_list_shift

        ! char *tclistshift2(TCLIST *list)
        function tc_list_shift2_(list) bind(c, name='tclistshift2')
            import ::  c_ptr
            type(c_ptr), intent(in), value :: list
            type(c_ptr)                    :: tc_list_shift2_
        end function tc_list_shift2_

        ! const void *tclistval(const TCLIST *list, int index, int *sp)
        function tc_list_val(list, index, sp) bind(c, name='tclistval')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            integer(kind=c_int), intent(in)        :: sp
            type(c_ptr)                            :: tc_list_val
        end function tc_list_val

        ! const char *tclistval2(const TCLIST *list, int index)
        function tc_list_val2_(list, index) bind(c, name='tclistval2')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            type(c_ptr)                            :: tc_list_val2_
        end function tc_list_val2_

        ! void tclistclear(TCLIST *list)
        subroutine tc_list_clear(list) bind(c, name='tclistclear')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
        end subroutine tc_list_clear

        ! void tclistdel(TCLIST *list)
        subroutine tc_list_del(list) bind(c, name='tclistdel')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
        end subroutine tc_list_del

        ! void tclistinsert(TCLIST *list, int index, const void *ptr, int size)
        subroutine tc_list_insert(list, index, ptr, size) bind(c, name='tclistinsert')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
        end subroutine tc_list_insert

        ! void tclistinsert2(TCLIST *list, int index, const char *str)
        subroutine tc_list_insert2_(list, index, str) bind(c, name='tclistinsert2')
            import :: c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: list
            integer(kind=c_int),    intent(in), value :: index
            character(kind=c_char), intent(in)        :: str
        end subroutine tc_list_insert2_

        ! void tclistover(TCLIST *list, int index, const void *ptr, int size)
        subroutine tc_list_over(list, index, ptr, size) bind(c, name='tclistover')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in), value :: index
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
        end subroutine tc_list_over

        ! void tclistover2(TCLIST *list, int index, const char *str)
        subroutine tc_list_over2_(list, index, str) bind(c, name='tclistover2')
            import :: c_char, c_int, c_ptr
            type(c_ptr),            intent(in), value :: list
            integer(kind=c_int),    intent(in), value :: index
            character(kind=c_char), intent(in)        :: str
        end subroutine tc_list_over2_

        ! void *tclistpop(TCLIST *list, int *sp)
        subroutine tc_list_pop(list, sp) bind(c, name='tclistpop')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            integer(kind=c_int), intent(in)        :: sp
        end subroutine tc_list_pop

        ! void tclistpush(TCLIST *list, const void *ptr, int size)
        subroutine tc_list_push(list, ptr, size) bind(c, name='tclistpush')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
        end subroutine tc_list_push

        ! void tclistpush2(TCLIST *list, const char *str)
        subroutine tc_list_push2_(list, str) bind(c, name='tclistpush2')
            import :: c_char, c_ptr
            type(c_ptr),            intent(in), value :: list
            character(kind=c_char), intent(in)        :: str
        end subroutine tc_list_push2_

        ! void tclistsort(TCLIST *list)
        subroutine tc_list_sort(list) bind(c, name='tclistsort')
            import :: c_ptr
            type(c_ptr), intent(in), value :: list
        end subroutine tc_list_sort

        ! void tclistunshift(TCLIST *list, const void *ptr, int size)
        subroutine tc_list_unshift(list, ptr, size) bind(c, name='tclistunshift')
            import :: c_int, c_ptr
            type(c_ptr),         intent(in), value :: list
            type(c_ptr),         intent(in), value :: ptr
            integer(kind=c_int), intent(in), value :: size
        end subroutine tc_list_unshift

        ! void tclistunshift2(TCLIST *list, const char *str)
        subroutine tc_list_unshift2_(list, str) bind(c, name='tclistunshift2')
            import ::  c_char, c_ptr
            type(c_ptr),            intent(in), value :: list
            character(kind=c_char), intent(in)        :: str
        end subroutine tc_list_unshift2_
    end interface
contains
    ! char *tclistpop2(TCLIST *list)
    function tc_list_pop2(list)
        !! Wrapper for `tc_list_pop2_()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr), intent(in)       :: list
        character(len=:), allocatable :: tc_list_pop2
        type(c_ptr)                   :: ptr
        integer(kind=8)               :: size

        ptr = tc_list_pop2_(list)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_list_pop2)
        call c_f_string_ptr(ptr, tc_list_pop2)
        call c_free(ptr)
    end function tc_list_pop2

    ! char *tclistremove2(TCLIST *list, int index)
    function tc_list_remove2(list, index)
        !! Wrapper for `tc_list_remove2_()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),         intent(in)  :: list
        integer(kind=c_int), intent(in)  :: index
        character(len=:),    allocatable :: tc_list_remove2
        type(c_ptr)                      :: ptr
        integer(kind=8)                  :: size

        ptr = tc_list_remove2_(list, index)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_list_remove2)
        call c_f_string_ptr(ptr, tc_list_remove2)
        call c_free(ptr)
    end function tc_list_remove2

    ! char *tclistshift2(TCLIST *list)
    function tc_list_shift2(list)
        !! Wrapper for `tc_list_shift2_()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),      intent(in)  :: list
        character(len=:), allocatable :: tc_list_shift2
        type(c_ptr)                   :: ptr
        integer(kind=8)               :: size

        ptr = tc_list_shift2_(list)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_list_shift2)
        call c_f_string_ptr(ptr, tc_list_shift2)
        call c_free(ptr)
    end function tc_list_shift2

    ! const char *tclistval2(const TCLIST *list, int index)
    function tc_list_val2(list, index)
        !! Wrapper for `tc_list_val2_()` that converts the returned C
        !! pointer to Fortran character.
        type(c_ptr),         intent(in)  :: list
        integer(kind=c_int), intent(in)  :: index
        character(len=:),    allocatable :: tc_list_val2
        type(c_ptr)                      :: ptr
        integer(kind=8)                  :: size

        ptr = tc_list_val2_(list, index)

        if (.not. c_associated(ptr)) &
            return

        size = c_strlen(ptr)
        allocate (character(len=size) :: tc_list_val2)
        call c_f_string_ptr(ptr, tc_list_val2)
        call c_free(ptr)
    end function tc_list_val2

    ! void tclistinsert2(TCLIST *list, int index, const char *str)
    subroutine tc_list_insert2(list, index, str)
        !! Wrapper for `tc_list_insert2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: list
        integer,          intent(in) :: index
        character(len=*), intent(in) :: str

        call tc_list_insert2_(list, index, str // c_null_char)
    end subroutine tc_list_insert2

    ! void tclistover2(TCLIST *list, int index, const char *str)
    subroutine tc_list_over2(list, index, str)
        !! Wrapper for `tc_list_over2_()` that adds `c_null_char` to key and
        !! value.
        type(c_ptr),      intent(in) :: list
        integer,          intent(in) :: index
        character(len=*), intent(in) :: str

        call tc_list_over2_(list, index, str // c_null_char)
    end subroutine tc_list_over2

    ! void tclistpush2(TCLIST *list, const char *str)
    subroutine tc_list_push2(list, str)
        !! Wrapper for `tc_list_push2()` that adds `c_null_char` to the string.
        type(c_ptr),      intent(in) :: list
        character(len=*), intent(in) :: str

        call tc_list_push2_(list, str // c_null_char)
    end subroutine tc_list_push2

    ! void tclistunshift2(TCLIST *list, const char *str)
    subroutine tc_list_unshift2(list, str)
        !! Wrapper for `tc_list_unshift2()` that adds `c_null_char` to the string.
        type(c_ptr),      intent(in) :: list
        character(len=*), intent(in) :: str

        call tc_list_unshift2_(list, str // c_null_char)
    end subroutine tc_list_unshift2
end module tclist
