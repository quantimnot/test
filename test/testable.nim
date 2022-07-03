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

import
  std/[options, with],
  pkg/prelude/[lib, init, alias, set_ctor_to_array_ctor]

when defined runtests:
  import ./types
  import std/[tables, pegs, re, strutils, exitprocs, typetraits]
  from std/hashes import hash
  from std/os import splitFile
  export types, init

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
  var testCases: Table[string, TableRef[string, seq[IsolatedTestCase]]]
  # module -> proc -> test

  const
    nimRunTemplate {.strdefine.} = "nim {backend} {defines} r - 2>{testRunInputHash}"

  const testableDatabase {.strdefine.} = ""
  const testableSession {.strdefine.} = ""
  const testableModules {.strdefine.} = ".*"
  const testableLabels {.strdefine.} = ".*"
  const testableSuites {.strdefine.} = ".*"
  const testableIds {.strdefine.} = ".*"

  proc runTests() =
    if testableDatabase.len == 0:
      echo "hint: no test result database defined; no results will be saved"
    else:
      echo "hint: using test result database @ " & testableDatabase
    if testableSession.len == 0:
      echo "hint: running tests with a temporary session; no results will be saved"
    else:
      echo "hint: running tests under session " & testableSession
    echo "hint: running tests that match module paths of " & testableModules
    echo "hint: running tests that match labels of " & testableLabels
    echo "hint: running tests that match suite of " & testableSuites
    echo "hint: running tests that match id of " & testableIds
    echo "hint: modules with tests: " & $testCases.len
    var testableCount = 0
    for (_, m) in testCases.pairs:
      testableCount += m.len
    echo "hint: procs/funcs under test: " & $testableCount
    echo "hint: isolated test programs: " & $testCaseIndex

    let testableModulesRe = testableModules.re
    let testableIdsRe = testableIds.re

    if testCases.len > 0:
      for module, testables in testCases:
        if module.match testableModulesRe:
          echo "  ", module
          for (testable, tests) in testables.pairs:
            if testable.match testableIdsRe:
              echo "    " , tests[0].subject.signature
              for test in tests:
                echo "      ", $(test.id), " ", test.title
                echo test.src.indent(8)
    else:
      echo "no tests found"

  # exitprocs.addExitProc runTests

  func sig(node: NimNode): string {.compileTime.} =
    var sigNode = node.copy
    sigNode[6] = newEmptyNode() # drop body
    repr sigNode
  # macro conanicalSigForm*(sym: typed): string =
    # newLit getTypeImpl(sym).repr
  macro sigHash*(sym: typed): untyped =
    newLit signatureHash sym
  macro implHash*(sym: typed): untyped =
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
      when defined runtests: tests.add newPar(newLit($tests.len), nil, nil, body)
    of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
      when defined runtests: tests.add newPar(newLit($title), nil, nil, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
      when defined runtests: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
      when defined runtests: tests.add newPar(newLit($tests.len), envOverride, nil, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", `title` @ nnkStrLit)), `body` @ nnkStmtList):
      when defined runtests: tests.add newPar(newLit($title), envOverride, nil, body)
    of nnkCommand(ident"with", `envOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
      when defined runtests:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($tests.len), envOverride, nil, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($title), envOverride, nil, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined runtests: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
      when defined runtests:
        for child in body.children:
          child.matchAst:
          of nnkCall(ident"test", `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
          of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($title), nil, expectOverride, body)
          of nnkCommand(ident"test",
            nnkCommand(ident"with", `envOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
          of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
            nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
            when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
          of nnkCommand(ident"with", `envOverride` @ nnkTupleConstr, `body` @ nnkStmtList):
            for child in body.children:
              child.matchAst:
              of nnkCall(ident"test", `body` @ nnkStmtList):
                when defined runtests: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
              of nnkCommand(ident"test", `title` @ nnkStrLit, `body` @ nnkStmtList):
                when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($title), nil, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($tests.len), nil, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"test",
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr))), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($tests.len), envOverride, expectOverride, body)
    of nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkTupleConstr,
       nnkCommand(ident"with", `envOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", nnkCommand(`title` @ nnkStrLit,
       nnkCommand(ident"expect", `expectOverride` @ nnkTupleConstr)))), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    of nnkCommand(ident"expect", nnkCommand(`expectOverride` @ nnkTupleConstr,
       nnkCommand(ident"with", nnkCommand(`envOverride` @ nnkTupleConstr,
       nnkCommand(ident"test", `title` @ nnkStrLit)))), `body` @ nnkStmtList):
        when defined runtests: tests.add newPar(newLit($title), envOverride, expectOverride, body)
    else: newBody.add child


macro testable*(origProcDef: untyped): untyped =
  result = origProcDef
  matchAst(origProcDef, matchErrors):
  of {nnkProcDef, nnkFuncDef}:
    let (strippedProcDef, tests) = origProcDef.extractTests
    result = buildAst(stmtList):
      when defined(runtests) and defined(testCoverage):
        getAst(coverage.cov(strippedProcDef)) # instrument for branch coverage
      else: strippedProcDef
      when defined runtests:
        if tests.len > 0:
          let ident: string = $origProcDef.name
          let symbol = strippedProcDef.name
          let signature = sig origProcDef
          let module = origProcDef.lineInfoObj.filename.splitFile.name
          let modulePath = symbol.lineInfoObj.filename
          let macroBaseName = "test" & origProcDef.name.strVal
          let line = symbol.lineInfoObj.line
          let column = symbol.lineInfoObj.column
          var testIndex: int
          for test in tests:
            let procName = ident(macroBaseName & '_' & $testIndex)
            let src = """import "./""" & module & "\"\n" & test[3].repr
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
            quote do:
              proc `procName` =
                var modules = testCases.mgetOrPut(`modulePath`, newTable[string, seq[IsolatedTestCase]]())
                let sigHash = sigHash(`symbol`)
                let sigImplHash = implHash(`symbol`)
                var tests = modules.mgetOrPut(sigHash, @[])
                var test = newUnitTestCase(
                    title = `title`,
                    subject = TestSubject(
                      ident: `ident`,
                      signature: `signature`,
                      module: `module`,
                      modulePath: `modulePath`,
                      line: `line`,
                      column: `column`,
                      sigHash: sigHash,
                      implHash: sigImplHash),
                    env = ExecEnvironment(),
                    expect = initFromTuple(Expectation, `expect`),
                    src = `src`)
                # with test.env:
                  # `env`
                # with test.expect:
                  # `expect`
                tests.add(test)
                modules[sigHash] = tests
              `procName`()
              testCaseIndex.inc()
            testIndex.inc()
  # debugEcho repr result


proc lessThanTen[T](x: T, o = ""): bool {.testable.} =
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
  # test "lessThanTen" expect (exitCode: setToArray({0..10, int.high}).toPackedSet):
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
