# fortran-tcdb
A collection of ISO C binding interfaces to the
[Tokyo Cabinet](https://fallabs.com/tokyocabinet/) key-value database for Fortran
2008. At the moment, only hash databases are supported.

![Tokyo Cabinet](logo.png)

The interfaces are split across several modules:

  * **tchdb.f90**: File-based hash database API.
  * **tcmdb.f90**: On-memory hash database API.
  * **tclist.f90**: Array List API.
  * **tcmisc.f90**: Miscellaneous utilities API (date & time, MD5).
  * **tcutil.f90**: Includes hack to access Tokyo Cabinet version number.

Tested with Tokyo Cabinet 1.4.48 on FreeBSD 12 with GNU Fortran 9, but should be
compatible to other Unix-like operating systems and Fortran 2008 compilers.

### String Arguments
In some cases, convenience routines are used to add `c_null_char` to string
arguments automatically. Performance is therefore slightly decreased, as an
additional function call is necessary. To avoid these wrappers, affected
interfaces are exposed directly, with a trailing underscore in their name, but
null termination must be handled manually:

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

Development headers may be required on Linux. Then, compile the static library
`libfortran-tcdb.a` with:

```
$ make static
```

Make sure that `PREFIX` points to the correct path. Link Tokyo Cabinet with
`libfortran-tcdb.a` and `-ltokyocabinet -lz -lbz2 -lrt -lpthread -lm -lc`.

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
$ gfortran -I/usr/local/include/ -L/usr/local/lib/ -o example example.f90 libfortran-tcdb.a -ltokyocabinet
$ ./example
```

## Further Examples
Additional examples can be found in `examples/`:

  * **hdb** opens a hash database and does read/write operations.
  * **mdb** creates an on-memory hash database to store strings.
  * **list** pushes strings to an array list.
  * **tcv** prints the Tokyo Cabinet version number (in case you really need this info …).

Build them with:

```
$ make examples
```

## Coverage
### Array List API
| C Function Name   | Fortran Interface Name                    | Bound |
|-------------------|-------------------------------------------|-------|
| `tclistbsearch`   | `tc_list_bsearch`                         |   ✓   |
| `tclistclear`     | `tc_list_clear`                           |   ✓   |
| `tclistdel`       | `tc_list_del`                             |   ✓   |
| `tclistdump`      | `tc_list_dump`                            |   ✓   |
| `tclistdup`       | `tc_list_dup`                             |   ✓   |
| `tclistinsert`    | `tc_list_insert`                          |   ✓   |
| `tclistinsert2`   | `tc_list_insert2`, `tc_list_insert2_`     |   ✓   |
| `tclistload`      | `tc_list_load`                            |   ✓   |
| `tclistlsearch`   | `tc_list_lsearch`                         |   ✓   |
| `tclistnew`       | `tc_list_new`                             |   ✓   |
| `tclistnew2`      | `tc_list_new2`                            |   ✓   |
| `tclistnew3`      |                                           |       |
| `tclistnum`       | `tc_list_num`                             |   ✓   |
| `tclistover`      | `tc_list_over`                            |   ✓   |
| `tclistover2`     | `tc_list_over2`, `tc_list_over2_`         |   ✓   |
| `tclistpop`       | `tc_list_pop`                             |   ✓   |
| `tclistpop2`      | `tc_list_pop2`                            |   ✓   |
| `tclistpush`      | `tc_list_push`                            |   ✓   |
| `tclistpush2`     | `tc_list_push2`, `tc_list_push2_`         |   ✓   |
| `tclistremove`    | `tc_list_remove`                          |   ✓   |
| `tclistremove2`   | `tc_list_remove2`, `tc_list_remove2_`     |   ✓   |
| `tclistshift`     | `tc_list_shift`                           |   ✓   |
| `tclistshift2`    | `tc_list_shift2`, `tc_list_shift2_`       |   ✓   |
| `tclistsort`      | `tc_list_sort`                            |   ✓   |
| `tclistunshift`   | `tc_list_unshift`                         |   ✓   |
| `tclistunshift2`  | `tc_list_unshift2`, `tc_list_unshift2_`   |   ✓   |
| `tclistval`       | `tc_list_val`                             |   ✓   |
| `tclistval2`      | `tc_list_val2`, `tc_list_val2_`           |   ✓   |

### On-Memory Hash Database API
| C Function Name   | Fortran Interface Name                    | Bound |
|-------------------|-------------------------------------------|-------|
| `tcmdbadddouble`  | `tc_mdb_add_double`                       |   ✓   |
| `tcmdbaddint`     | `tc_mdb_add_int`                          |   ✓   |
| `tcmdbcutfront`   | `tc_mdb_cut_front`                        |   ✓   |
| `tcmdbdel`        | `tc_mdb_del`                              |   ✓   |
| `tcmdbfwmkeys`    | `tc_mdb_fwm_keys`                         |   ✓   |
| `tcmdbfwmkeys2`   | `tc_mdb_fwm_keys2`, `tc_mdb_fwm_keys2_`   |   ✓   |
| `tcmdbget`        | `tc_mdb_get`                              |   ✓   |
| `tcmdbget2`       | `tc_mdb_get2`, `tc_mdb_get2_`             |   ✓   |
| `tcmdbiterinit`   | `tc_mdb_iter_init`                        |   ✓   |
| `tcmdbiternext`   | `tc_mdb_iter_next`                        |   ✓   |
| `tcmdbiternext2`  | `tc_mdb_iter_next2`                       |   ✓   |
| `tcmdbmsiz`       | `tc_mdb_msiz`                             |   ✓   |
| `tcmdbnew`        | `tc_mdb_new`                              |   ✓   |
| `tcmdbnew2`       | `tc_mdb_new2`                             |   ✓   |
| `tcmdbout`        | `tc_mdb_out`                              |   ✓   |
| `tcmdbout2`       | `tc_mdb_out2`, `tc_mdb_out2_`             |   ✓   |
| `tcmdbput`        | `tc_mdb_put`                              |   ✓   |
| `tcmdbput2`       | `tc_mdb_put2`, `tc_mdb_put2_`             |   ✓   |
| `tcmdbputcat`     | `tc_mdb_put_cat`                          |   ✓   |
| `tcmdbputcat2`    | `tc_mdb_put_cat2`, `tc_mdb_put_cat2_`     |   ✓   |
| `tcmdbputkeep`    | `tc_mdb_put_keep`                         |   ✓   |
| `tcmdbputkeep2`   | `tc_mdb_put_keep2`, `tc_mdb_put_keep2_`   |   ✓   |
| `tcmdbrnum`       | `tc_mdb_rnum`                             |   ✓   |
| `tcmdbvanish`     | `tc_mdb_vanish`                           |   ✓   |
| `tcmdbvsiz`       | `tc_mdb_vsiz`                             |   ✓   |
| `tcmdbvsiz2`      | `tc_mdb_vsiz2`, `tc_mdb_vsiz2_`           |   ✓   |

### Hash Database API
| C Function Name   | Fortran Interface Name                    | Bound |
|-------------------|-------------------------------------------|-------|
| `tchdbadddouble`  | `tc_hdb_add_double`                       |   ✓   |
| `tchdbaddint`     | `tc_hdb_add_int`                          |   ✓   |
| `tchdbclose`      | `tc_hdb_close`                            |   ✓   |
| `tchdbcopy`       | `tc_hdb_copy`, `tc_hdb_copy_`             |   ✓   |
| `tchdbdel`        | `tc_hdb_del`                              |   ✓   |
| `tchdbecode`      | `tc_hdb_ecode`                            |   ✓   |
| `tchdberrmsg`     | `tc_hdb_err_msg`                          |   ✓   |
| `tchdbfsiz`       | `tc_hdb_fsiz`                             |   ✓   |
| `tchdbfwmkeys`    | `tc_hdb_fwm_keys`                         |   ✓   |
| `tchdbfwmkeys2`   | `tc_hdb_fwm_keys2`, `tc_hdb_fwm_keys2_`   |   ✓   |
| `tchdbget`        | `tc_hdb_get`                              |   ✓   |
| `tchdbget2`       | `tc_hdb_get2`, `tc_hdb_get2_`             |   ✓   |
| `tchdbget3`       | `tc_hdb_get3`                             |   ✓   |
| `tchdbiterinit`   | `tc_hdb_iter_init`                        |   ✓   |
| `tchdbiternext`   | `tc_hdb_iter_next`                        |   ✓   |
| `tchdbiternext2`  | `tc_hdb_iter_next2`                       |   ✓   |
| `tchdbiternext3`  | `tc_hdb_iter_next3`                       |   ✓   |
| `tchdbnew`        | `tc_hdb_new`                              |   ✓   |
| `tchdbopen`       | `tc_hdb_open`, `tc_hdb_open_`             |   ✓   |
| `tchdboptimize`   | `tc_hdb_optimize`                         |   ✓   |
| `tchdbout`        | `tc_hdb_out`                              |   ✓   |
| `tchdbout2`       | `tc_hdb_out2`, `tc_hdb_out2_`             |   ✓   |
| `tchdbpath`       | `tc_hdb_path`                             |   ✓   |
| `tchdbput`        | `tc_hdb_put`                              |   ✓   |
| `tchdbput2`       | `tc_hdb_put2`, `tc_hdb_put2_`             |   ✓   |
| `tchdbputasync`   | `tc_hdb_put_async`                        |   ✓   |
| `tchdbputasync2`  | `tc_hdb_put_async2`, `tc_hdb_put_async2_` |   ✓   |
| `tchdbputcat`     | `tc_hdb_put_cat`                          |   ✓   |
| `tchdbputcat2`    | `tc_hdb_put_cat2`, `tc_hdb_put_cat2_`     |   ✓   |
| `tchdbputkeep`    | `tc_hdb_put_keep`                         |   ✓   |
| `tchdbputkeep2`   | `tc_hdb_put_keep2`, `tc_hdb_put_keep2_`   |   ✓   |
| `tchdbrnum`       | `tc_hdb_rnum`                             |   ✓   |
| `tchdbsetdfunit`  | `tc_hdb_set_dfunit`                       |   ✓   |
| `tchdbsetcache`   | `tc_hdb_set_cache`                        |   ✓   |
| `tchdbsetmutex`   | `tc_hdb_set_mutex`                        |   ✓   |
| `tchdbsetxmsiz`   | `tc_hdb_set_xmsiz`                        |   ✓   |
| `tchdbsync`       | `tc_hdb_sync`                             |   ✓   |
| `tchdbtranabort`  | `tc_hdb_tran_abort`                       |   ✓   |
| `tchdbtranbegin`  | `tc_hdb_tran_begin`                       |   ✓   |
| `tchdbtrancommit` | `tc_hdb_tran_commit`                      |   ✓   |
| `tchdbtune`       | `tc_hdb_tune`                             |   ✓   |
| `tchdbvanish`     | `tc_hdb_vanish`                           |   ✓   |
| `tchdbvsiz`       | `tc_hdb_vsiz`                             |   ✓   |
| `tchdbvsiz2`      | `tc_hdb_vsiz2`, `tc_hdb_vsiz2_`           |   ✓   |

### Basic Utilities API
| C Variable Name   | Fortran Interface Name                    | Bound |
|-------------------|-------------------------------------------|-------|
| `*tcversion`      | `tc_version`                              |   ✓   |

### Miscellaneous Utilities API
| C Function Name   | Fortran Interface Name                    | Bound |
|-------------------|-------------------------------------------|-------|
| `tcdatestrwww`    | `tc_date_str_www`                         |   ✓   |
| `tcmd5hash`       | `tc_md5_hash`                             |   ✓   |
| `tcstrmktime`     | `tc_str_mk_time`                          |   ✓   |

## Licence
ISC
