#[import std/unittest as stdUnittest
export stdUnittest.TestStatus
export stdUnittest.TestResult
export stdUnittest.suiteStarted
export stdUnittest.testStarted
export stdUnittest.check
export stdUnittest.checkpoint
export stdUnittest.fail]#

import pkg/prelude/[common, lib]

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
type ExitCodeRange* = Slice[int]
type ExecEnvironment* = object
  command*: string
  variables*: seq[(string, string)]
  workingDir*: string
type Expectation* = object
  exitCode*: ExitCodeRange
  stdout*: seq[TextMatcher]
  stderr*: seq[TextMatcher]
type TestCase* = object
  subject*: TestSubject
  env*: ExecEnvironment
  expect*: Expectation
  title*: string
  stdin*: string

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

  converter toExitCodeRange*(returnCode: int): ExitCodeRange =
    returnCode..returnCode

  var testCases: seq[TestCase]
  const
    nimRunTemplate {.strdefine.} = "nim {backend} {defines} r - 2>{testRunInputHash}"
    defaultEnvironment = ExecEnvironment(command: nimRunTemplate)
    defaultExpectation = Expectation()

  proc runTests() =
    if testCases.len > 0:
      log "running " & $testCases.len & " tests"
      for t in testCases:
        echo repr t

  from std/exitprocs import addExitProc
  exitprocs.addExitProc runTests

  macro conanicalSigForm(sym: typed): string =
    newLit getTypeImpl(sym).repr
  macro sigHash(sym: typed): untyped =
    newLit signatureHash sym
  macro implHash(sym: typed): untyped =
    newLit symBodyHash sym

  func buildTestCase(title = "", env = defaultEnvironment, expect = defaultExpectation, stdin = ""):
    TestCase {.compileTime, inline.} = TestCase(title: title, env: env, expect: expect, stdin: stdin)

proc extractTests(procDef: NimNode): (NimNode, seq[TestCase]) {.compileTime.} =
  result[0] = procDef.copy
  alias origBody, procDef[6]
  alias newBody, result[0][6]
  alias tests, result[1]
  result[0][6] = newStmtList()
  assert procDef.kind in RoutineNodes
  assert origBody.kind == nnkStmtList
  macro override[T: ExecEnvironment|Expectation](a: T, b: NimNode): T =
    var c = a.copy
    c
  for child in origBody.children:
    child.matchAst:
    of nnkCall(ident"test", `body` @ nnkStmtList):
      when defined test: tests.add buildTestCase($tests.len, stdin = body.repr)
    of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
      when defined test: tests.add buildTestCase($title, stdin = body.repr)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", `envOverride` @ nnkPar)), `body` @ nnkStmtList):
      when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride), stdin = body.repr)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", `envOverride` @ nnkPar), `body` @ nnkStmtList):
      when defined test: tests.add buildTestCase($tests.len, override(defaultEnvironment, envOverride), stdin = body.repr)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", `title` @ nnkStrLit)), `body` @ nnkStmtList):
      when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride), stdin = body.repr)
    of nnkCommand(ident"with", `envOverride` @ nnkPar, `body` @ nnkStmtList):
      when defined test:
        var env = override(defaultEnvironment, envOverride)
        body.matchAst:
        of nnkCall(ident"test", `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($tests.len, env, stdin = body.repr)
        of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($title, env = env, stdin = body.repr)
        of nnkCommand(ident"test",
          nnkCommand(ident"expect", `expectOverride` @ nnkPar), `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($tests.len, env, override(defaultExpectation, envOverride), body.repr)
        of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
          nnkCommand(ident"expect", `expectOverride` @ nnkPar)), `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($title, env, override(defaultExpectation, envOverride), body.repr)
        of nnkCommand(ident"expect", `expectOverride` @ nnkPar, `body` @ nnkStmtList):
          var expect = override(defaultExpectation, expectOverride)
          body.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add buildTestCase($tests.len, env, expect, stdin = body.repr)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add buildTestCase($title, env, expect, body.repr)
    of nnkCommand(ident"expect", `expectOverride` @ nnkPar, `body` @ nnkStmtList):
      when defined test:
        var expect = override(defaultExpectation, expectOverride)
        body.matchAst:
        of nnkCall(ident"test", `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($tests.len, expect = expect, stdin = body.repr)
        of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($title, expect = expect, stdin = body.repr)
        of nnkCommand(ident"test",
          nnkCommand(ident"with", `envOverride` @ nnkPar), `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($tests.len, override(defaultEnvironment, envOverride), expect, body.repr)
        of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
          nnkCommand(ident"with", `envOverride` @ nnkPar)), `body` @ nnkStmtList):
          when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride), expect, body.repr)
        of nnkCommand(ident"with", `envOverride` @ nnkPar, `body` @ nnkStmtList):
          var env = override(defaultEnvironment, envOverride)
          body.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined test: tests.add buildTestCase($tests.len, env, expect, body.repr)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined test: tests.add buildTestCase($title, env, expect, body.repr)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($title, expect = override(defaultExpectation, expectOverride), stdin = body.repr)
    of nnkCommand(ident"test",
       nnkCommand(ident"expect", `expectOverride` @ nnkPar), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($tests.len, expect = override(defaultExpectation, expectOverride), stdin = body.repr)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride),
                   override(defaultExpectation, expectOverride), body.repr)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar))), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($tests.len, override(defaultEnvironment, envOverride),
                   override(defaultExpectation, expectOverride), body.repr)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkPar,
       nnkCommand(ident"with", `envOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride),
                   override(defaultExpectation, expectOverride), body.repr)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkPar)))), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride),
                   override(defaultExpectation, expectOverride), body.repr)
    of nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkPar,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkPar,
       nnkCommand(ident"test", `title` @ nnkStrLit)))), `body` @ nnkStmtList):
        when defined test: tests.add buildTestCase($title, override(defaultEnvironment, envOverride),
                   override(defaultExpectation, expectOverride), body.repr)
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
          let sig: string = $origProcDef.name.repr
          let symbol = origProcDef.name
          let module = origProcDef.lineInfoObj.filename.splitFile.name
          let modulePath = symbol.lineInfoObj.filename
          let macroBaseName = "test" & origProcDef.name.strVal
          let line = symbol.lineInfoObj.line
          let column = symbol.lineInfoObj.column
          var testIndex: int
          for test in tests:
            let procName = ident(macroBaseName & '_' & $testIndex)
            let stdin = """import "./""" & module & """"\n""" & test.stdin
            let title = test.title
            quote do:
              proc `procName` =
                unittest.testCases.add(
                  TestCase(
                    title: `title`,
                    subject: TestSubject(
                      ident: `ident`,
                      signature: `sig`,
                      module: `module`,
                      modulePath: `modulePath`,
                      line: `line`,
                      column: `column`,
                      sigHash: `symbol`.sigHash,
                      implHash: `symbol`.implHash
                    ),
                    env: ExecEnvironment(),
                    expect: Expectation(),
                    stdin: `stdin`
                ))
              `procName`()
            testIndex.inc()
  #else:
    #when defined test:
    #  echo matchErrors
    #else: discard
  log repr result

proc lessThanTen*[T](x: T, o = ""): bool {.test.} =
  test "lessThanTen":
    import std/unittest
    test "lessThanTen":
      check 5.lessThanTen
      check not 10.lessThanTen
      check not 11.lessThanTen
  test "other":
    echo "hello"
  test with (command: "nim r -"):
    quit 1
  with (command: "nim r -") test "lessThanTen":
    quit 1
#[   with (command: "nim r -"):
    test "lessThanTen":
      quit 1 ]#
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
