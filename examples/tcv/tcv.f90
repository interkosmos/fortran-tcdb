! tcv.f90
!
! Example that simply prints the Tokyo Cabinet version to stdout.
!
! Author:   Philipp Engel
! Licence:  ISC
program main
    use :: tcversion

    print '(2a)', 'Tokyo Cabinet, v.', tc_version()
end program main
