! tcv.f90
!
! Example that simply prints the Tokyo Cabinet to stdout.
!
! Author:   Philipp Engel
! Licence:  ISC
program main
    use :: tcversion

    print '(2a)', 'Toky Cabinet, v.', tc_version()
end program main
