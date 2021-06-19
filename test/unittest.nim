##***h*
##* TODO
##*   - implement overriding env and expect objects
##*   - hash test cases and combine with the subject of test hash so that
##*     tests are only executed when either changes
##*   - design test execution plan
##*     - concurrent & async
##*   - persist test results and metrics in a time or git-hash series
##*     - id
##*     - stdout/stderr output
##*     - setup/teardown timing
##*     - exec timing
##*     - exec coverage
##*     - exec profiling
##*     - memory profiling
##*     - memory leaks
##*     - fuzz results
##*   - load default env and expect from a config
##*****

import pkg/prelude/[common, lib, init]

when defined test:
  import ./types
  import std/[tables, pegs, re, strutils, exitprocs, typetraits]
  from std/hashes import hash
  from std/os import splitFile

  type StrLitMatcher = ref object of RootRef
  proc firstMatch(self: StrLitMatcher, corpus, pattern: string): Option[Slice[int]] =
    let offset = pattern.find(pattern)
    if offset == -1: return
    else: some offset..pattern.len

  type PegMatcher = ref object of RootRef
  proc firstMatch(self: PegMatcher, corpus, pattern: string): Option[Slice[int]] =
    var matches: seq[string]
    when compiles(pattern.findBounds(pattern.peg)):
      # will be faster without handling matches
      {.error: "Good news everybody! Change the next source line for a speed boost.".}
    let (offset, length) = pattern.findBounds(pattern.peg, matches)
    if offset == -1: return
    else: some offset..length

  type RegexMatcher = ref object of RootRef
  proc firstMatch(self: RegexMatcher, corpus, pattern: string): Option[Slice[int]] =
    var matches: seq[string]
    let (offset, length) = pattern.findBounds(pattern.peg, matches)
    if offset == -1: return
    else: some offset..length

  var testCaseIndex = 0
  var testCases: Table[string, seq[TestCase]]

  const
    nimRunTemplate {.strdefine.} = "nim {backend} {defines} r - 2>{testRunInputHash}"

  proc runTests() =
    if testCases.len > 0:
      echo "running " & $testCaseIndex & " tests"
      for sig, tests in testCases:
        for test in tests:
          echo test.subject.ident
          # echo test.subject.signature[0..4] & test.subject.ident & test.subject.signature[5..^1]
    else:
      echo "no tests found"

  exitprocs.addExitProc runTests

  macro conanicalSigForm(sym: typed): string =
    newLit getTypeImpl(sym).repr
  macro sigHash(sym: typed): untyped =
    newLit signatureHash sym
  macro implHash(sym: typed): untyped =
    newLit symBodyHash sym

proc extractTests(procDef: NimNode): (NimNode, seq[NimNode]) {.compileTime.} =
  result[0] = procDef.copy
  alias origBody, procDef[6]
  alias newBody, result[0][6]
  alias tests, result[1]
  result[0][6] = newStmtList()
  assert procDef.kind in RoutineNodes
  assert origBody.kind == nnkStmtList
  for child in origBody.children:
    child.matchAst:
    of nnkCall(ident"test", `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($tests.len), nil, nil, body)
    of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), nil, nil, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($tests.len), envOverride, nil, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", `title` @ nnkStrLit)), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"with", `envOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
      when defined test:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, nil, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
      when defined test:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), nil, expectOverride, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"with", `envOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"with", `envOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), nil, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkTupleConstr,
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkTupleConstr,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", `title` @ nnkStrLit)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    else: newBody.add child

func genSetConstructor(lhs, rhs: NimNode): NimNode {.compileTime.} =
  result = newStmtList()
  template clear(lhs: untyped): untyped =
    lhs.clear()
  template assign(lhs, rhs: untyped): untyped =
    lhs = rhs
  template i(lhs, rhs: untyped): untyped =
    #bind types.incl
    lhs.incl(rhs)
    #[ when `k`.typeof is PackedSet[int]:
      when `v`.typeof is int:
        `k`.incl `v` ]#
  rhs.matchAst:
  of `value` @ nnkIntLit:
    result.add getAst assign(lhs, rhs)
  of `setConstructor` @ nnkCurly:
    for child in setConstructor:
      result.add getAst clear(lhs)
      child.matchAst:
      of `value` @ nnkIntLit:
        result.add getAst i(lhs, rhs)
      of nnkInfix(ident"..", `low` @ nnkIntLit, `high` @ nnkIntLit):
        result.add getAst assign(lhs, rhs)
  of nnkInfix(ident"..", `low` @ nnkIntLit, `high` @ nnkIntLit):
    result.add getAst assign(lhs, rhs)

func composeExpect(lhs, rhs: NimNode): NimNode {.compileTime.} =
  newColonExpr lhs, rhs
#[   quote do:
    var expect = Expectation(
      exitCode: {},
      stdout: @[],
      stderr: @[]
    ) ]#

macro testable*(origProcDef: untyped): untyped =
  result = origProcDef
  matchAst(origProcDef, matchErrors):
  of {nnkProcDef, nnkFuncDef}:
    let (strippedProcDef, tests) = origProcDef.extractTests
    result = buildAst(stmtList):
      when defined(test) and defined(testCoverage):
        getAst(coverage.cov(strippedProcDef)) # instrument for branch coverage
      else: strippedProcDef
      when defined(test):
        if tests.len > 0:
          let ident: string = $origProcDef.name
          let symbol = origProcDef.name
          let module = origProcDef.lineInfoObj.filename.splitFile.name
          let modulePath = symbol.lineInfoObj.filename
          let macroBaseName = "test" & origProcDef.name.strVal
          let line = symbol.lineInfoObj.line
          let column = symbol.lineInfoObj.column
          var testIndex: int
          for test in tests:
            let procName = ident(macroBaseName & '_' & $testIndex)
            let stdin = """import "./""" & module & "\"\n" & test[3].repr
            let title = $test[0]
            var env = buildAst(stmtList):
              for override in test[1]:
                let k = override[0]
                let v = override[1]
                quote do: `k` = `v`
            var expect =
              if test[2].kind == nnkTupleConstr:
                test[2]
              else:
                nnkTupleConstr.newTree()
#[             buildAst(stmtList):
              for override in test[2]:
                let k = override[0]
                let v = override[1]
                let initializer = composeExpect(k, v) ]#

               #[  for field in Expectation().fields:
                  when field.typeof is PackedSet:
                    genSetConstructor k, v ]#
            #echo expect.repr
                #quote do: discard
                  #when `k`.typeof is PackedSet[int]:
                  #  when `v`.typeof is int:
                  #    `k`.incl `v`
            quote do:
              proc `procName` =
                var tests = testCases.mgetOrPut(`symbol`.sigHash, @[])
                var test = TestCase(
                    title: `title`,
                    subject: TestSubject(
                      ident: `ident`,
                      signature: `symbol`.conanicalSigForm,
                      module: `module`,
                      modulePath: `modulePath`,
                      line: `line`,
                      column: `column`,
                      sigHash: `symbol`.sigHash,
                      implHash: `symbol`.implHash),
                    env: ExecEnvironment(),
                    expect: initFromTuple(Expectation, `expect`),
                    stdin: `stdin`)
                with test.env:
                  `env`
                # with test.expect:
                #  `expect`
                tests.add(test)
                testCases[`symbol`.sigHash] = tests
              `procName`()
              testCaseIndex.inc()
            testIndex.inc()

proc lessThanTen*[T](x: T, o = ""): bool {.testable.} =
  test "lessThanTen":
    import std/unittest
    test "lessThanTen":
      check 5.lessThanTen
      check not 10.lessThanTen
      check not 11.lessThanTen
  test "other":
    echo "hello"
  test:
    echo "hello"
  test with (command: "nim r -"):
    quit 1
  with (command: "nim r -") test "lessThanTen":
    quit 1
  with (command: "nim r -"):
    test:
      echo "hello"
    expect (exitCode: 1):
      test:
        quit 1
      test "other":
        quit 1
  # test "lessThanTen" expect (exitCode: {0..10, int.high}):
    # quit 1
  test "lessThanTen" with (command: "nim r -") expect (exitCode: 0..10):
    quit 1
  test "lessThanTen" expect (exitCode: 0..10) with (command: "nim r -"):
    quit 1
  with (command: "nim r -") test "lessThanTen" expect (exitCode: 0..10):
    quit 1
  if x < 10:
    return true
  else:
    return false
