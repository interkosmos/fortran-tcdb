# f08tcdb: Fortran 2008 Interface to Tokyo Cabinet
An ISO C binding interface to the Tokyo Cabinet key-value database for Fortran
2008. At the moment, only string-based hash databases are supported.

![Tokyo Cabinet](logo.png)

Tested with Tokyo Cabinet 1.4.48 on FreeBSD 12 with GNU Fortran 8, but should be
compatible to other Unix-like operating systems and Fortran 2008 compilers.

## Build
At first, install Tokyo Cabinet. On FreeBSD, run:

```
# pkg install databases/tokyocabinet
```

Then, run:

```
$ make tcdb
```

Make sure that `LDFLAGS` points to the correct paths. `RPATH` might be
optional. Link Tokyo Cabinet with `-ltokyocabinet -lz -lbz2 -lrt -lpthread -lm
-lc`.

## Example

```fortran
! example.f90
program main
    use, intrinsic :: iso_c_binding, only: c_ptr
    use :: tcdb
    implicit none
    type(c_ptr)                   :: hdb
    character(len=:), allocatable :: value
    logical                       :: err

    hdb = tc_hdb_new()
    err = tc_hdb_open(hdb, 'casket.tch', ior(HDB_OWRITER, HDB_OCREAT))
    err = tc_hdb_put2(hdb, 'foo', 'bar')

    value = tc_hdb_get2(hdb, 'foo')
    print '(2a)', 'value: ', value

    err = tc_hdb_close(hdb)
    call tc_hdb_del(hdb)
end program main
```

Compile and run the example with:

```
$ gfortran8 -Wl,-rpath=/usr/local/lib/gcc8/ -I/usr/local/include/ -L/usr/local/lib/ \
  -O1 -o example example.f90 tcdb.o -ltokyocabinet -lz -lbz2 -lrt -lpthread -lm -lc
$ ./example
```

## Further Examples

* **hdb** opens a hash database and does read/write operations.

Build the examples with:

```
$ make examples
```

## Coverage

| Function Name     | Interface Name       | Bound |
|-------------------|----------------------|-------|
| `tchdbclose`      | `tc_hdb_close`       |   ✓   |
| `tchdbcopy`       | `tc_hdb_copy`        |   ✓   |
| `tchdbdel`        | `tc_hdb_del`         |   ✓   |
| `tchdbecode`      | `tc_hdb_ecode`       |   ✓   |
| `tchdberrmsg`     | `tc_hdb_err_msg`     |   ✓   |
| `tchdbfsiz`       | `tc_hdb_fsiz`        |   ✓   |
| `tchdbget2`       | `tc_hdb_get2`        |   ✓   |
| `tchdbiterinit`   | `tc_hdb_iter_init`   |   ✓   |
| `tchdbiternext2`  | `tc_hdb_iter_next2`  |   ✓   |
| `tchdbnew`        | `tc_hdb_new`         |   ✓   |
| `tchdbopen`       | `tc_hdb_open`        |   ✓   |
| `tchdboptimize`   | `tc_hdb_optimize`    |   ✓   |
| `tchdbput2`       | `tc_hdb_put2`        |   ✓   |
| `tchdbrnum`       | `tc_hdb_rnum`        |   ✓   |
| `tchdbsetcache`   | `tc_hdb_set_cache`   |   ✓   |
| `tchdbsetmutex`   | `tc_hdb_set_mutex`   |   ✓   |
| `tchdbsync`       | `tc_hdb_sync`        |   ✓   |
| `tchdbtranabort`  | `tc_hdb_tran_abort`  |   ✓   |
| `tchdbtranbegin`  | `tc_hdb_tran_begin`  |   ✓   |
| `tchdbtrancommit` | `tc_hdb_tran_commit` |   ✓   |
| `tchdbtune`       | `tc_hdb_tune`        |   ✓   |
| `tchdbvanish`     | `tc_hdb_vanish`      |   ✓   |
| `tchdbvsiz2`      | `tc_hdb_vsiz2`       |   ✓   |

## Licence
ISC
