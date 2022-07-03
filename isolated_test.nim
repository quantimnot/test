import
  std/[macros, os, strformat, strutils, sha1, osproc, compilesettings],
  test/tags


macro isolatedTest*(opts: untyped{nkStrLit}, tests: untyped{nkStmtList}) =
  ## Writes the test statements to a uniquely named file and then runs them.
  # TODO: This currently has to be untyped because `repr` doesn't always return valid Nim.
  when defined test:
    when isMainModule or inTestTags([splitFile(currentSourcePath()).name]):
      {.push hint[GlobalVar]: off.}
      let modulePathParts = tests.lineInfoObj.filename.splitFile
      let modulePath = modulePathParts.dir / modulePathParts.name
      let moduleName = modulePathParts.name
      let input = &"""
import "{modulePath}"
{repr tests}
"""
      return quote do:
        let input = `input`
        let testModulePath = querySetting(nimcacheDir) / `moduleName` & "_test_" & $secureHash(input) & ".nim"
        let cmd = getCurrentCompilerExe() & " --verbosity:0 --warnings:off --hints:off " & `opts` & " r " & testModulePath
        if not fileExists testModulePath:
          writeFile testModulePath, input
        let (output, exitCode) = execCmdEx(cmd)
        echo output
        if exitCode != QuitSuccess:
          quit exitCode

  else: discard

template isolatedTest*(tests: untyped{nkStmtList}) =
  ## Writes the test statements to a uniquely named file and then runs them.
  isolatedTest(""):
    tests
