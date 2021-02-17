#[import std/unittest as stdUnittest
export stdUnittest.TestStatus
export stdUnittest.TestResult
export stdUnittest.suiteStarted
export stdUnittest.testStarted
export stdUnittest.check
export stdUnittest.checkpoint
export stdUnittest.fail]#

import pkg/prelude/[common, lib]

#proc runTests*()

when defined test:
  import pkg/coverage
  export coverage.CovData
  from std/hashes import hash
  from std/os import splitFile

  const TEST_RUN_NIM_CMD {.strdefine.} = "nim r"

  type TestSubject* = object
    ident*: string
    signature*: string
    module*: string
    modulePath*: string
    line*: int
    column*: int
    sigHash*: string
    implHash*: string
  type TextMatcher* = RootObj
  type ExitCodeRange* = Slice[int]
  type Expectation* = object
    exitCode*: ExitCodeRange
    stdout*: seq[TextMatcher]
    stderr*: seq[TextMatcher]
  converter toExitCodeRange*(returnCode: int): ExitCodeRange =
    returnCode..returnCode

  var testCases {.compileTime.}: seq[(TestSubject, string, string, Expectation)]

  #from std/exitprocs import addExitProc
  #exitprocs.addExitProc runTests

#[   proc addTestCase(symbol: NimNode, executor, source: string, expect = Expectation()) {.compileTime.} =
    unittest.testCases.add((
      TestSubject(
        ident: symbol.strVal,
        signature: symbol.repr,
        module: symbol.lineInfoObj.filename.splitFile.name,
        modulePath: symbol.lineInfoObj.filename,
        line: symbol.lineInfoObj.line,
        column: symbol.lineInfoObj.column,
        #sigHash: symbol.sigHash,
        #implHash: symbol.implHash
      ),
      executor,
      source,
      expect
    )) ]#

macro runTests*() =
  when defined test:
    log "running tests"
    for t in testCases:
      echo repr t
    quit()
  else: discard

proc extractTests(procDef: NimNode): (NimNode, NimNode) {.compileTime.} =
  ## Extract all first-level `nnkCommand(ident"test", nnkStrLit, nnkStmtList)`
  ## AST patterns from the proc and return them.
  result = (procDef.copy, newStmtList())
  alias origBody, procDef[6]
  alias newBody, result[0][6]
  alias tests, result[1]
  result[0][6] = newStmtList()
  assert procDef.kind in RoutineNodes
  assert origBody.kind == nnkStmtList
  for child in origBody.children:
    child.matchAst:
    of nnkCommand(ident"test", nnkStrLit, nnkStmtList):
      tests.add child
    else: newBody.add child

proc implHash(n: NimNode): string = n.symBodyHash

macro test*(origProcDef: untyped): untyped =
  result = origProcDef
  matchAst(origProcDef, matchErrors):
  of {nnkProcDef, nnkFuncDef}:
    let (strippedProcDef, tests) = origProcDef.extractTests
    when defined test:
      result = buildAst(stmtList):
        when defined testCoverage:
          getAst(coverage.cov(strippedProcDef)) # instrument for branch coverage
        else:
          strippedProcDef
        if tests.len > 0:
          let ident: string = $origProcDef.name
          let sig: string = $origProcDef.name.repr
          let symbol = origProcDef.name
          let module = origProcDef.lineInfoObj.filename.splitFile.name
          let modulePath = symbol.lineInfoObj.filename
          let macroBaseName = "test" & origProcDef.name.strVal
          let line = symbol.lineInfoObj.line
          let column = symbol.lineInfoObj.column
          for test in tests:
            let macroName = ident(macroBaseName & $hash(test[1].strVal))
            let source = "import " & currentSourcePath() & "\n" & test[2].repr
            let executor = TEST_RUN_NIM_CMD & " -d:testCoverage -"
            #unittest.addTestCase(origProcDef.name, command, source)
            quote do:
              macro `macroName` =
                unittest.testCases.add((
                  TestSubject(
                    ident: `ident`,
                    signature: `sig`,
                    module: `module`,
                    modulePath: `modulePath`,
                    line: `line`,
                    column: `column`,
                    sigHash: `symbol`.sigHash,
                    implHash: `symbol`.implHash
                  ),
                  `executor`,
                  `source`,
                  Expectation()
                ))
                #unittest.addTestCase(`symbol`, `executor`, `source`)
                #let output, exitCode = gorgeEx(`executor`, `source`, `symbol`.implHash)
                #echo output
              `macroName`
  else:
    when defined test:
      echo matchErrors
  log repr result

macro conanicalSigForm(sym: typed): string =
  newLit getTypeImpl(sym).repr
macro sigHash(sym: typed): untyped =
  newLit signatureHash sym
macro implHash(sym: typed): untyped =
  newLit symBodyHash sym

proc lessThanTen*[T](x: T, o = ""): bool {.test.} =
  test "lessThanTen":
    import std/unittest
    test "lessThanTen":
      check 5.lessThanTen
      check not 10.lessThanTen
      check not 11.lessThanTen
  if x < 10:
    return true
  else:
    return false
runTests()
