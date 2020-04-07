# miniFS, a minimal file-system-like thing

What exactly is a file system? Roughly, it is two maps (one for dirs,
one for files). But there is also the POSIX API etc. This is a minimal
version, suitable to understand single-process use of a filesystem.



## Related packages

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSqzipIxfOtcWhtSEqcBUpEKPVp1ALtHYyVVBldz7WNP3idcaQTY0iHoLBMf9n4vNMUjDvoIi_gr2gE/pub?w=1034&amp;h=520">



## Modules



<img src="https://docs.google.com/drawings/d/e/2PACX-1vQcYK7STiRj8bxZBFvkYaqQtsAXxCrjAKrfD2GV_uDuec5DmEY3qNq1Nbj_h-om3L1HHA5JY_RBd_uF/pub?w=953&amp;h=352">

The code is modularized. So, for example, it is possible to take the FUSE filesystem operations (which are parameterized over an arbitrary implementation of FS ops), and combine with the NFS client ops to get an NFS client mounted via FUSE. On the server, you can take the NFS server (again, paramterized by some backend) and combine with the in-memory implementation to get an in-memory NFS server. Alternatively, you can combine with the "Unix ops" (a passthrough to an underlying POSIX filesystem), to serve local files over the network. Cool!



## Docs

<https://tomjridge.github.io/mini-fs/>




## Dependencies

| Dependency                   | Description  |
| ---------------------------- | ------------ |
| ocamlfuse                    | FUSE support |
| ppx_binprot                  |              |
| ppx_yojson                   |              |
| extunix                      | pread,pwrite |
| tjr_path_resolution, tjr_net |              |



## Executables

In bin/ there are some executables:

| Name              | Description                                      |
| ----------------- | ------------------------------------------------ |
| fuse_in_mem_main  | Fuse filesystem, backed by in-memory store       |
| fuse_nfs_client   | Fuse filesystem, backed by remote "NFS" server   |
| nfs_client_test   | Run some simple commands against a remote server |
| nfs_server_in_mem | "NFS" server, backed by in-memory store          |
| nfs_server_unix   | "NFS" server, backed by local filesystem         |

## Running the executables

The executables typically invoke `Fuse.main Sys.argv`. At such, command line arguments follow those provided by `Fuse.main`. A full list of options can be displayed by typing e.g. `./fuse_in_mem_main.exe --help`

Further docs on FUSE flags here: https://gist.github.com/tomjridge/ecc9d271ecce8e11cd517b01ee25d0e2
