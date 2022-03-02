## Path IO 1.6.3

* Fixed a bug that caused `removeDirLink` fail on Linux because of a
  trailing slash that used to be passed to the underlying
  `removeDirectoryLink` function from the `directory` package. [Issue
  59](https://github.com/mrkkrp/path-io/issues/59).

* Works with GHC 9.0.1.

## Path IO 1.6.2

* Fixed a bug in the `findFilesWith` and based on it `findFiles` functions.

## Path IO 1.6.1

* Fixed a space leak in `walkDirAccum`. [Issue
  55](https://github.com/mrkkrp/path-io/issues/55).

## Path IO 1.6.0

* Changed how `copyDirRecur` and `copyDirRecur'` functions work. Previously,
  the functions created empty directories in the destination directory when
  the source directory contained directory symlinks. The symlinked
  directories were not recursively traversed. It also copied symlinked files
  creating normal regular files in the target directory as the result. This
  is fixed so that the function now behaves much like the `cp` utility, not
  traversing symlinked directories, but recreating symlinks in the target
  directory according to their targets in the source directory.

* Fixed a bug in `createDirLink` which would always fail complaining that
  its destination location does not exist.

* Dropped support for GHC 8.2.

## Path IO 1.5.0

* Dropped support for GHC 8.0 and older.
* Added new functions: `getXdgDirList`, `createFileLink`, `createDirLink`,
  `removeDirLink`, `getSymlinkTarget`.

## Path IO 1.4.2

* Fixed various bugs in `listDirRecurRel`, `walkDirRel`, and
  `walkDirAccumRel` and clarified their behavior in the docs.

## Path IO 1.4.1

* Fixed a bug in `walkDirRel` that resulted in `NotAProperPrefix` exception
  every time the function was called.

## Path IO 1.4.0

* Added relative versions of some actions: `listDirRel`, `listDirRecurRel`,
  `walkDirRel`, and `walkDirAccumRel`.

* Dropped support for GHC 7.8.

## Path IO 1.3.3

* (Hopefully) fixed test suite failure with Cabal 2.0 and GHC 8.2.1.

## Path IO 1.3.2

* Reduce a number of `(MonadIO m, MonadThrow m)` constraints to just
  `MonadIO m` [#27](https://github.com/mrkkrp/path-io/issues/27)

## Path IO 1.3.1

* Made `listDirRecur` faster for deep directory trees.

## Path IO 1.3.0

* Change the default behavior of recursive traversal APIs to not follow
  symbolic links. The change affects the following functions:
  `listDirRecur`, `copyDirRecur`, and `copyDirRecur'`.

* Add `isSymlink` which allows to test whether a path is a symbolic link.

* Move the type functions `AbsPath` and `RelPath` to the `AnyPath` type
  class (previously they were standalone closed type families, now they are
  associated types of `AnyPath`).

* Improved the documentation and metadata.

## Path IO 1.2.3

* Allowed `time-1.7`.

## Path IO 1.2.2

* Fixed a bug in `setModificationTime` function that previously called
  `setAccessTime` instead of `setModificationTime` from `directory`.

* Added notes to all pieces of API that are conditional (some functions are
  only available if `directory-1.2.3.0` or later is used, now it's mentioned
  for every such function explicitely).

## Path IO 1.2.1

* Allowed `directory-1.3.0.0`.

* Added `getXdgDir`. Only available of `directory-1.2.3.0` or later is used.

* Various cosmetic improvements.

## Path IO 1.2.0

* Added `walkDir` function to traverse a directory tree with a handler.

* Added `walkDirAccum` function which is like `walkDir` but also accepts an
  output writer and returns the accumulated output.

* All recursive traversal functions (existing and new) now safely handle
  directory loops due to symbolic or hard links.

* Added “since” notes to public functions in API.

## Path IO 1.1.0

* Fixed bug in `copyDirRecur` when it was unable to fully copy read-only
  directories.

* Added `copyDirRecur'` function that works just like `copyDirRecur`, but
  does not preserve directory permissions.

## Path IO 1.0.1

* Fixed bug in `copyDirRecur` for non-existing destination paths when
  directory to copy does not contain sub-directories.

* Made `copyDirRecur` try to copy permissions for destination directory too
  (previously it only tried to copy them for sub-directories).

## Path IO 1.0.0

* Changed signature of `getAppUserDataDir`, so it takes `String` as the
  first argument.

* Removed deprecated `forgivingAbsence'` function.

* Made `findFile` lazier, it stops searching as soon as a file is found.

* Added some tests.

## Path IO 0.3.1

* Introduced synonym for `forgivingAbsence'` —
  `ignoringAbsence`. `forgivingAbsence'` is deprecated now, but it's still
  there.

* Added a handy shortcut `ensureDir` that is defined as
  `ensureDir = createDirIfMissing True`.

* Made `getHomeDir` and `getTempDir` more robust when they are influenced by
  values of environment variables.

## Path IO 0.3.0

* Added `forgivingAbsence`, `resolveFile`, and `resolveDir` functions, so
  the package now provides all functionality that `Path.IO` module in Stack
  has.

* Added closed type family `RelPath`, `makeRelative`, and
  `makeRelativeToCurrentDir` functions.

* Fixed signature of `getAppUserDataDir`.

## Path IO 0.2.0

* Added functions from `temporary`: `withTempFile`, `withTempDir`,
  `withSystemTempFile`, `withSystemTempDir`, `openTempFile`,
  `openBinaryTempFile`, and `createTempDir`. `temporary` is a lightweight
  and ubiquitous package, so depending on it should be OK.

## Path IO 0.1.1

* Fixed type signatures of `renameFile` and `copyFile`.

## Path IO 0.1.0

* Initial release.
