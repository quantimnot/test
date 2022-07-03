from std/os import splitFile

include test/testable

when defined test:
  import test/tags
  import std/unittest
  export unittest

template test*(codeBlock) =
  bind splitFile
  when defined test:
    when isMainModule or inTestTags([splitFile(currentSourcePath()).name]):
      {.push hint[GlobalVar]: off.}
      codeBlock
      {.pop.}

import isolated_test
export isolated_test
