#****h*
## TODO
##   - 
#******

import
  std/[options, packedsets, setutils],
  pkg/iface,
  test
export test except TestCase
export packedsets

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
  proc firstMatch(corpus, pattern: string): Option[Slice[int]] {.used.}

#[ The above gets rewritten into:
type
  TextMatcher = Interface[InterfaceVTableTextMatcher]
  InterfaceVTableTextMatcher = object
    firstMatch: proc (this: RootRef; corpus, pattern: string): Option[
        Slice[int]] {.nimcall.}

proc initVTable(t: var InterfaceVTableTextMatcher; TIfaceUnpackedThis: typedesc) =
  t.firstMatch = proc (this: RootRef; corpus, pattern: string): Option[Slice[int]] =
    var ifaceUnpackedThis: TIfaceUnpackedThis
    unpackObj(this, ifaceUnpackedThis)
    forceReturnValue(Option[Slice[int]], checkRequiredMethod(
        firstMatch(ifaceUnpackedThis, corpus, pattern), TextMatcher))

proc firstMatch(this: TextMatcher; corpus, pattern: string): Option[Slice[int]] {.inline, stackTrace: false.} =
  result = this.private_vTable.firstMatch(this.private_obj, corpus, pattern)

converter toTextMatcher[T](a`gensym3: T): TextMatcher {.inline.} =
  to(a`gensym3, TextMatcher)
]#

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
  ## Command that will execute `IsolatedTestCase.src`.
  envVars*: seq[(string, string)]
  ## Environment variables set for program execution.
  # TODO: timeoutSec: int
type Expectation* = object
  exitCode: PackedSet[int]
  stdout*: seq[TextMatcher]
  stderr*: seq[TextMatcher]
type IsolatedTestCase* = ref object of test.TestCase
  subject*: TestSubject
  env*: ExecEnvironment
  expect*: Expectation
  title*: string
  src*: string

func toSlice*(val: int): Slice[int] =
  val..val

func incl*[T; O: int8|int16|uint8|uint16|char|byte](self: var PackedSet[T], val: set[O]): PackedSet[T] =
  for i in val.low..val.high:
    packedsets.incl(self, i)

func incl*[T](self: var PackedSet[T], val: Slice[T]): PackedSet[T] =
  for i in val.a..val.b:
    packedsets.incl(self, i)

func exitCode*(self: Expectation): PackedSet[int] =
  self.exitCode

template `exitCode=`*(self: var Expectation, value: untyped) =
  self.exitCode.clear()
  self.exitCode.assign value.toSet

func `exitCode=`*(self: var Expectation, value: PackedSet[int]) =
  self.exitCode.assign value

func `exitCode=`*(self: var Expectation, value: int) =
  self.exitCode.clear()
  self.exitCode.incl value

func `exitCode=`*(self: var Expectation, value: openArray[int]) =
  self.exitCode.clear()
  self.exitCode.assign value.toPackedSet

func `exitCode=`*[T: char|byte|int8|int16|uint8|uint16|range](self: var Expectation, value: set[T]) =
  self.exitCode.clear()
  for i in value:
    self.exitCode.incl(i)

func `exitCode=`*(self: var Expectation, value: Slice[int]) =
  self.exitCode.clear()
  for i in value.a..value.b:
    self.exitCode.incl(i)

proc newUnitTestCase*(subject: TestSubject, env: ExecEnvironment, expect: Expectation, title, src: string): owned IsolatedTestCase {.tags: [TimeEffect].} =
  result = IsolatedTestCase(subject: subject, env: env, expect: expect, title: title, src: src)
  result.id = TestCaseId(kind: TestCaseIdKind.NimSigHash, nimSigHash: subject.sigHash)
