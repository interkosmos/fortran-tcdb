# f08tcdb: Fortran 2008 Interface to Tokyo Cabinet
An ISO C binding interface to the
[Tokyo Cabinet](https://fallabs.com/tokyocabinet/) key-value database for Fortran
2008. At the moment, only string-based hash databases are supported.

![Tokyo Cabinet](logo.png)

Tested with Tokyo Cabinet 1.4.48 on FreeBSD 12 with GNU Fortran 9, but should be
compatible to other Unix-like operating systems and Fortran 2008 compilers.

In some cases, wrapper routines are used to add `c_null_char` to string
arguments automatically. Performance is therefore slightly decreased, as an
additional function call is performed. To avoid these wrappers, some interfaces
are exposed directly, with a postfix underscore in their name, but null
termination must be handled manually:

```fortran
! Calling the wrapper function that does null termination for us:
err = tc_hdb_put2(hdb, 'foo', 'bar')
! Calling the interface directly:
err = tc_hdb_put2_(hdb, 'foo' // c_null_char, 'bar' // c_null_char)
```

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
    use :: tchdb
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
$ gfortran9 -Wl,-rpath=/usr/local/lib/gcc9/ -I/usr/local/include/ -L/usr/local/lib/ \
  -O1 -o example example.f90 tcdb.o tchdb.o -ltokyocabinet
$ ./example
```

## Further Examples

* **hdb** opens a hash database and does read/write operations.

Build the examples with:

```
$ make examples
```

## Coverage
### Hash Database API

| Function Name     | Fortran Interface Name                   | Bound |
|-------------------|------------------------------------------|-------|
| `tchdbclose`      | `tc_hdb_close`                           |   ✓   |
| `tchdbcopy`       | `tc_hdb_copy`, `tc_hdb_copy_`            |   ✓   |
| `tchdbdel`        | `tc_hdb_del`                             |   ✓   |
| `tchdbecode`      | `tc_hdb_ecode`                           |   ✓   |
| `tchdberrmsg`     | `tc_hdb_err_msg`                         |   ✓   |
| `tchdbfsiz`       | `tc_hdb_fsiz`                            |   ✓   |
| `tchdbfwmkeys`    | `tc_hdb_fwm_keys`                        |   ✓   |
| `tchdbfwmkeys2`   | `tc_hdb_fwm_keys2`, `tc_hdb_fwm_keys2_`  |   ✓   |
| `tchdbget`        | `tc_hdb_get`                             |   ✓   |
| `tchdbget2`       | `tc_hdb_get2`, `tc_hdb_get2_`            |   ✓   |
| `tchdbget3`       | `tc_hdb_get3`                            |   ✓   |
| `tchdbiterinit`   | `tc_hdb_iter_init`                       |   ✓   |
| `tchdbiternext`   | `tc_hdb_iter_next`                       |   ✓   |
| `tchdbiternext2`  | `tc_hdb_iter_next2`                      |   ✓   |
| `tchdbiternext3`  | `tc_hdb_iter_next3`                      |   ✓   |
| `tchdbnew`        | `tc_hdb_new`                             |   ✓   |
| `tchdbopen`       | `tc_hdb_open`, `tc_hdb_open_`            |   ✓   |
| `tchdboptimize`   | `tc_hdb_optimize`                        |   ✓   |
| `tchdbout`        | `tc_hdb_out`                             |   ✓   |
| `tchdbout2`       | `tc_hdb_out2`, `tc_hdb_out2_`            |   ✓   |
| `tchdbpath`       | `tc_hdb_path`                            |   ✓   |
| `tchdbput`        | `tc_hdb_put`                             |   ✓   |
| `tchdbput2`       | `tc_hdb_put2`, `tc_hdb_put2_`            |   ✓   |
| `tchdbputasync`   | `tc_hdb_put_async`                       |   ✓   |
| `tchdbputasync2`  | `tc_hdb_put_async2`, `tc_hdb_put_async2` |   ✓   |
| `tchdbputcat`     | `tc_hdb_put_cat`                         |   ✓   |
| `tchdbputcat2`    | `tc_hdb_put_cat2`, `tc_hdb_put_cat2_`    |   ✓   |
| `tchdbputkeep`    | `tc_hdb_put_keep`                        |   ✓   |
| `tchdbputkeep2`   | `tc_hdb_put_keep2`, `tc_hdb_put_keep2_`  |   ✓   |
| `tchdbrnum`       | `tc_hdb_rnum`                            |   ✓   |
| `tchdbsetdfunit`  | `tc_hdb_set_dfunit`                      |   ✓   |
| `tchdbsetcache`   | `tc_hdb_set_cache`                       |   ✓   |
| `tchdbsetmutex`   | `tc_hdb_set_mutex`                       |   ✓   |
| `tchdbsetxmsiz`   | `tc_hdb_set_xmsiz`                       |   ✓   |
| `tchdbsync`       | `tc_hdb_sync`                            |   ✓   |
| `tchdbtranabort`  | `tc_hdb_tran_abort`                      |   ✓   |
| `tchdbtranbegin`  | `tc_hdb_tran_begin`                      |   ✓   |
| `tchdbtrancommit` | `tc_hdb_tran_commit`                     |   ✓   |
| `tchdbtune`       | `tc_hdb_tune`                            |   ✓   |
| `tchdbvanish`     | `tc_hdb_vanish`                          |   ✓   |
| `tchdbvsiz`       | `tc_hdb_vsiz`                            |   ✓   |
| `tchdbvsiz2`      | `tc_hdb_vsiz2`, `tc_hdb_vsiz2_`          |   ✓   |

## Licence
ISC
