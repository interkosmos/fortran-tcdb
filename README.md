# f08tcdb: Fortran 2008 Interface to Tokyo Cabinet
An ISO C binding interface to the Tokyo Cabinet key-value database for Fortran
2008. At the moment, only string-based hash databases are supported.

![Tokyo Cabinet](https://fallabs.com/tokyocabinet/logo.png)

Tested with Tokyo Cabinet 1.4.48 on FreeBSD 12 with GNU Fortran 8, but should be
compatible to other Unix-like operating systems and Fortran 2008 compilers.

## Build
At first, install Tokyo Cabinet. On FreeBSD, run:

```
# pkg install databases/tokyocabinet
```

Then, run:

```
$ make
```

Make sure that `LDFLAGS` points to the correct paths. `RPATH` might be
optional. Link Tokyo Cabinet with `-ltokyocabinet -lz -lbz2 -lrt -lpthread -lm
-lc`.

## Examples

* **hdb** opens a hash database and does read/write operations.

Build the examples with:

```
$ make examples
```

## Coverage

| Function Name     | Interface Name       | Bound |
|-------------------|----------------------|-------|
| `tchdbcopy`       | `tc_hdb_copy`        |   ✓   |
| `tchdbfsiz`       | `tc_hdb_fsiz`        |   ✓   |
| `tchdboptimize`   | `tc_hdb_optimize`    |   ✓   |
| `tchdbrnum`       | `tc_hdb_rnum`        |   ✓   |
| `tchdbsetcache`   | `tc_hdb_set_cache`   |   ✓   |
| `tchdbsetmutex`   | `tc_hdb_set_mutex`   |   ✓   |
| `tchdbsync`       | `tc_hdb_sync`        |   ✓   |
| `tchdbtranabort`  | `tc_hdb_tran_abort`  |   ✓   |
| `tchdbtranbegin`  | `tc_hdb_tran_begin`  |   ✓   |
| `tchdbtrancommit` | `tc_hdb_tran_commit` |   ✓   |
| `tchdbvanish`     | `tc_hdb_vanish`      |   ✓   |
| `tchdbclose`      | `tc_hdb_close`       |   ✓   |
| `tchdbdel`        | `tc_hdb_del`         |   ✓   |
| `tchdbecode`      | `tc_hdb_ecode`       |   ✓   |
| `tchdberrmsg`     | `tc_hdb_err_msg`     |   ✓   |
| `tchdbget2`       | `tc_hdb_get2`        |   ✓   |
| `tchdbiterinit`   | `tc_hdb_iter_init`   |   ✓   |
| `tchdbiternext2`  | `tc_hdb_iter_next2`  |   ✓   |
| `tchdbnew`        | `tc_hdb_new`         |   ✓   |
| `tchdbopen`       | `tc_hdb_open`        |   ✓   |
| `tchdbput2`       | `tc_hdb_put2`        |   ✓   |
| `tchdbtune`       | `tc_hdb_tune`        |   ✓   |
| `tchdbvsiz2`      | `tc_hdb_vsiz2`       |   ✓   |

## Licence
ISC
