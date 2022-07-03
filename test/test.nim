## PURPOSE
##   [TAP](https://testanything.org/) compatible test framework
## Derived from:
##   minitest
##   Copyright xmonader
##   minitest framework
##   https://github.com/xmonader/nim-minitest

when defined runtests:
  import std/[strformat, strutils, macros, oids, sha1, tables, options, base64]
  export oids.`$`

  type TestKind* {.pure.} = enum
    Unit,
    Functional,
    Coverage,
    Benchmark,
    Syscall,

  type ResultKind* {.pure.} = enum
    Pass = 0,
    ## The test passed.
    Fail = 1,
    ## The test failed.
    Skip = 2,
    ## The test was skipped.
    Todo = 4
    ## The test is incomplete and needs to be fully implemented.

  type
    HashKind {.pure.} = enum
      Sha1
    HashValue {.union.} = object
      sha1: Sha1Digest
    HashTable = Table[HashKind, HashValue]
  type
    TestCaseIdKind* {.pure.} = enum
      NimSigHash, Oid
    TestCaseId* = object
      case kind*: TestCaseIdKind
      of NimSigHash:
        nimSigHash*: string
      of TestCaseIdKind.Oid:
        oid*: Oid
  type TestCase* = ref object of RootObj
    expected*: string
    ## Expected test outcome.
    actual*: string
    ## Actual test outcome.
    result*: ResultKind
    ## The result of the test. See `ResultKind`
    desc*: Option[string]
    ## Optional test description.
    diag*: Option[string]
    ## Optional test diagnostics.
    labels*: Option[seq[string]]
    ## Optional labels for a test.
    suite*: Option[string]
    ## Optional test suite.
    cite*: Option[string]
    ## Optional citation of test case source. (i.e. examples from a standards doc)
    id*: TestCaseId
    ## Optional unique test case identifier.
    ## This is used to track test case metadata through time. 
    programHashes*: HashTable
    ## Optional table of test source program hashes. This can be used to track changes to
    ## the program source code under test.
    ## See Also
    ##   - std/macros/symBodyHash
    inputHashes*: HashTable
    ## Optional table of test input hashes. This can be used to track changes to
    ## the program inputs under test.


  proc init*(test: var TestCase) {.tags: [TimeEffect].} =
    test.id = TestCaseId(kind: TestCaseIdKind.Oid, oid: genOid())
  

  proc `$`*(id: TestCaseId): string =
    case id.kind
    of TestCaseIdKind.Oid:
      ($id.kind) & ": " & $id.oid
    of TestCaseIdKind.NimSigHash:
      ($id.kind) & ": " & $id.nimSigHash


  var testCases: seq[TestCase]

  proc test(suite, desc, diag = ""; skip, todo = false; id = none(TestCaseId)) =
    var testCase = TestCase(
      result: (if skip: Skip elif todo: Todo else: Pass),
      desc: if desc.len > 0: some(desc) else: none(string),
      diag: if diag.len > 0: some(diag) else: none(string),
    )

  template check*(exp: untyped, idx = 0, desc, diag = "", skip, todo = false): void =
    test "", (if desc.len > 0: desc else: astToStr(exp)), diag, skip, todo
    var id: string
    id = $idx
    var lead = "not ok\t" & id & "\t"
    let expStr: string = astToStr(exp)
    if skip:
      if desc.len > 0:
        echo lead & "# SKIP " & desc
      else:
        echo lead & "# SKIP"
    elif todo:
      if desc.len > 0:
        echo lead & "# TODO " & desc
      else:
        echo lead & "# TODO"
    else:
      if not exp:
        echo lead & expStr
      else:
        lead[0..5] = "ok    "
        echo lead & expStr
      if desc.len > 0: echo "# " & desc

  macro suite*(name: string, exprs: untyped) =
    result = newStmtList()
    var diag: string
    for exp in exprs.children:
      case exp.kind
      of nnkCall, nnkCommand:
        case $exp[0]
        of "skip":
          if exp.len > 0 and exp[1].kind == nnkStrLit:
            diag = $exp[1]
          for skipped in exp:
            if skipped.kind == nnkStmtList:
              for i in skipped:
                if i.kind == nnkCall:
                  if $i[0] == "check":
                    var c = i[1]
                    add result, quote do:
                      check(`c`, diag = `diag`, skip = true)
        of "todo":
          if exp.len > 0 and exp[1].kind == nnkStrLit:
            diag = $exp[1]
          for skipped in exp:
            if skipped.kind == nnkStmtList:
              for i in skipped:
                if i.kind == nnkCall:
                  if $i[0] == "check":
                    var c = i[1]
                    add result, quote do:
                      check(`c`, diag = `diag`, todo = true)
        let t = $exp[0]
        if exp.len > 0 and exp[1].kind == nnkStrLit:
          diag = $exp[1]
                    #check(`c`, diag = `diag`, skip = true)
  #      of "todo"
  #        if exp.len > 0 and exp[1].kind == nnkStrLit:
  #          diag = $exp[1]
  #        for skipped in exp:
  #          if skipped.kind == nnkStmtList:
  #            for i in skipped:
  #              if i.kind == nnkCall:
  #                if $i[0] == "check":
  #                  var c = i[1]
  #                  add result, quote do:
  #                    check(`c`, diag = `diag`, todo = true)
        else: add(result, exp) 
      else: discard

  when isMainModule:
    check(3==1+2+8)
    check(6+5*2 == 16)
    check 6+5*2 == 16, todo = true, diag = "wip"

    suite "Arith":
      skip "math processor not installed":
        check(1+2==4)
        check(3+2==5)

    suite "Strs":
      check "HELLO".toLowerAscii() == "hello"
      check "".len > 0

    suite "skipped":
      todo "needs implemented":
        check "a" == "a"
#  dumpTree:
#    skip:
#      check(1+2==3)
#StmtList
#  Call
#    Ident "skip"
#    StmtList
#      Call
#        Ident "check"
#        Infix
#          Ident "=="
#          Infix
#            Ident "+"
#            IntLit 1
#            IntLit 2
#          IntLit 3
  #   "Arith"
  #   check(3+2==5)
  #   check(1==1, "", 1)
else:
  template suite*(label: string, exprs: untyped): untyped = discard
  template test*(label: string, exprs: untyped): untyped = discard
  template check*(label: string, exprs: untyped): untyped = discard
  # when defined test:
    # {.hint: "`test` is defined, but not `testRunTests`; define `testRunTests` to compile and execute builtin tests.".}
