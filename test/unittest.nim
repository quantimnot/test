##***h* test/test
##* TODO
##*   - implement overriding env and expect objects
##*   - hash test cases and combine with the subject of test hash so that
##*     tests are only executed when either changes
##*   - design test execution plan
##*     - concurrent & async
##*   - persist test results and metrics in a timeseries
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

import pkg/prelude/[common, lib]

when defined test:
  when defined testCoverage:
    import pkg/coverage
    export coverage.CovData
  when defined testBench:
    discard
  when defined testMemLeaks:
    discard
  when defined testSyscalls:
    discard
  when defined testFileIo:
    discard

  iface TextMatcher:
    proc firstMatch(corpus, pattern: string): Option[Slice[int]]

  type TestKind* {.pure.} = enum
    Unit,
    Functional,
    Coverage,
    Benchmark,
    Syscall,
  type TestSubject* = object
    ident*: string
    signature*: string
    module*: string
    modulePath*: string
    line*: int
    column*: int
    sigHash*: string
    implHash*: string
  type ExecEnvironment* = object
    command*: string
    variables*: seq[(string, string)]
    workingDir*: string
  type Expectation* = object
    exitCode*: Slice[int] # TODO: should be set[int]?
    stdout*: seq[TextMatcher]
    stderr*: seq[TextMatcher]
  type TestCase* = object
    subject*: TestSubject
    env*: ExecEnvironment
    expect*: Expectation
    title*: string
    stdin*: string

  from std/hashes import hash
  from std/os import splitFile

  import std/strutils
  type StrLitMatcher = ref object of RootRef
  proc firstMatch(self: StrLitMatcher, corpus, pattern: string): Option[Slice[int]] =
    let offset = pattern.find(pattern)
    if offset == -1: return
    else: some offset..pattern.len

  import std/pegs
  type PegMatcher = ref object of RootRef
  proc firstMatch(self: PegMatcher, corpus, pattern: string): Option[Slice[int]] =
    var matches: seq[string]
    when compiles(pattern.findBounds(pattern.peg)):
      # will be faster without handling matches
      {.error: "Good news everybody! Change the next source line for a speed boost.".}
    let (offset, length) = pattern.findBounds(pattern.peg, matches)
    if offset == -1: return
    else: some offset..length

  import std/re
  type RegexMatcher = ref object of RootRef
  proc firstMatch(self: RegexMatcher, corpus, pattern: string): Option[Slice[int]] =
    var matches: seq[string]
    let (offset, length) = pattern.findBounds(pattern.peg, matches)
    if offset == -1: return
    else: some offset..length

  func toSlice*(returnCode: int): Slice[int] =
    returnCode..returnCode

  import std/tables
  var testCases: Table[string, seq[TestCase]]

  const
    nimRunTemplate {.strdefine.} = "nim {backend} {defines} r - 2>{testRunInputHash}"
    defaultEnvironment = ExecEnvironment(command: nimRunTemplate) # TODO: need?
    defaultExpectation = Expectation() # TODO: need?

  proc runTests() =
    if testCases.len > 0:
      echo "running " & $testCases.len & " tests"
      for sig, tests in testCases.pairs:
        for test in tests:
          echo test.subject.signature[0..4] & test.subject.ident & test.subject.signature[5..^1]

  from std/exitprocs import addExitProc
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
  macro override(a, b: NimNode): NimNode =
    var c = a.copy
    c
  for child in origBody.children:
    child.matchAst:
    of nnkCall(ident"test", `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($tests.len), nil, nil, body)
    of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), nil, nil, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", `envOverride` @ nnkPar)), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", `envOverride` @ nnkPar), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($tests.len), envOverride, nil, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", `title` @ nnkStrLit)), `body` @ nnkStmtList):
      when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"with", `envOverride` @ nnkPar, `body` @ nnkStmtList):
      when defined test:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, nil, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, nil, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"expect", `expectOverride` @ nnkPar), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"expect", `expectOverride` @ nnkPar)), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"expect", `expectOverride` @ nnkPar, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", `expectOverride` @ nnkPar, `body` @ nnkStmtList):
      when defined test:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), nil, expectOverride, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"with", `envOverride` @ nnkPar), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"with", `envOverride` @ nnkPar)), `body` @ nnkStmtList):
            when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"with", `envOverride` @ nnkPar, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), nil, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"expect", `expectOverride` @ nnkPar), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkPar,
       nnkCommand(ident"with", `envOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkPar,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", `title` @ nnkStrLit)))), `body` @ nnkStmtList):
        when defined test: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    else: newBody.add child

macro test*(origProcDef: untyped): untyped =
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
            let stdin = """import "./""" & module & """"\n""" & test[3].repr
            let title = $test[0]
            quote do:
              proc `procName` =
                var tests = unittest.testCases.getOrDefault(`symbol`.sigHash)
                tests.add(TestCase(
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
                    expect: Expectation(),
                    stdin: `stdin`))
              `procName`()
            testIndex.inc()
  debug repr result

proc lessThanTen*[T](x: T, o = ""): bool {.test.} =
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
  test "lessThanTen" expect (exitCode: 0..10):
    quit 1
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
