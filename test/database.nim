## Test Result Database

import "."/[test, testable]

type
  DbKind* {.pure.} = enum
    File
  Db* = ref object of RootRef
    kind*: DbKind
    uri*: string
  FileDb* = ref object of Db

proc load*(uri: string, kind: DbKind): Db =
  Db(uri: uri, kind: kind)

proc save*(db: Db) =
  discard

suite "test database":
  test "load":
    discard
