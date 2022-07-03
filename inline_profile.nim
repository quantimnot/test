import
  std/[exitprocs, macros, sugar, monotimes, strformat, strutils],
  pkg/[test, isolated_test]

type
  Duration = uint32
  CpuCycle = uint16
  InlineProfile* = object
    modulePath*: string
    sig*: string
    when defined inlineProfileOnlyCallCount:
      calls*: int
    else:
      durations*: seq[Duration]
      cycles*: seq[CpuCycle]


proc rdtsc(): uint64 {.inline.} =
  ## Execute the rdtsc, read Time Stamp Counter, instruction
  ## returns the 64 bit TSC value.
  var lo, hi: uint32
  {.emit: """asm volatile ("rdtsc\n\t" :"=a"(`lo`), "=d"(`hi`));""".}
  uint64(lo) or (uint64(hi) shl 32)


when not defined inlineProfileOnlyCallCount:
  func calls*(profile: InlineProfile): int =
    profile.durations.len

  template calcAverage(values) {.dirty.} =
    if profile.values.len > 0:
      when defined inlineProfileDropFirst:
        let len = if profile.values.len == 1: 1 else: profile.values.len - 1
        let inclRange = (if len == 1: 0 else: 1)..^1
      else:
        let len = profile.values.len
        let inclRange = 0..^1
      for value in profile.values[inclRange]:
        result += typeof(result) value
      result /= typeof(result) len

  func calcAverageDuration*(profile: InlineProfile): float32 =
    calcAverage durations

  func calcAverageCycles*(profile: InlineProfile): float32 =
    calcAverage cycles

func `$`*(profile: InlineProfile): string =
  result = profile.modulePath & '\n'
  when defined inlineProfileOnlyCallCount:
    result &= indent(dedent(&"""
    {profile.sig}
      calls: {profile.calls}"""), 2)
  else:
    result &= indent(dedent(&"""
    {profile.sig}
      calls: {profile.calls}
      avg duration (ns): {calcAverageDuration profile}
      avg cycles: {calcAverageCycles profile}"""), 2)
  when defined inlineProfilePrintRawObersvations:
    result &= '\n' & indent(dedent(&"""
    durations (ns): {profile.durations}
    cycles: {profile.cycles}"""), 4)

macro profile*(routine) =
  func sig(node: NimNode): string =
    var sigNode = node.copy
    sigNode[6] = newEmptyNode() # drop body
    repr sigNode
  result = newStmtList()
  var routine = routine
  let profileVarIdent = ident routine.name.strVal & "Profile"
  let routineSig = sig routine
  template globalVars(id, rsig) =
    var id = InlineProfile(modulePath: currentSourcePath(), sig: rsig)
    addExitProc proc() = echo $id
  result.add getAst globalVars(profileVarIdent, routineSig)
  template routineStart(id) =
    {.noSideEffect.}:
      when defined inlineProfileOnlyCallCount:
        id.calls.inc
      else:
        let startTime = ticks(getMonoTime())
        let startCycles = rdtsc()
        defer:
          when defined(gcArc) or defined(gcOrc):
            {.warning: "Nim Arc/Orc bug workaround".}
            id.cycles.add CpuCycle rdtsc() - startCycles
            let duration = Duration ticks(getMonoTime()) - startTime
            discard $duration # so wasteful, but I don't know how to do better
            id.durations.add duration
          else:
            id.cycles.add CpuCycle rdtsc() - startCycles
            id.durations.add Duration ticks(getMonoTime()) - startTime
  routine.last.insert 0, getAst routineStart profileVarIdent
  result.add routine
  # echo repr result

test:
  proc test*(a, b: int): int {.profile.} =
    a + b

  var accum: int
  for i in 0..10:
    accum = test(accum, i)
