#****h* test/tags
## PURPOSE
##   Scope test code to specific tags that can be set at compile time.
## EXAMPLE
##   ```sh
##   nim r -d:test=tag,othertag,anothertag
##   ```
## TODO
##   - [ ] support tag exclusion: `nim r -d:test=-tagname` or maybe `nim r -d:test=!tagname`
##   - [ ] support tag re: `nim r -d:test=/tag.+/`
#******
import std/[sets, strutils]

#****id* tags/test
const test {.strdefine.}: string = ""
  ## PURPOSE
  ##   Holds the test tags that enable test code.
#******

#****c* tags/testTags
const testTags* = (
  func(): HashSet[string] =
    if test.len == 0 or test == "true":
      return ["*"].toHashSet
    else:
      return test.split(',').toHashSet
)()
  ## PURPOSE
  ##   Holds the test tags that enable test code.
  ## DESCRIPTION
  ##   The test tags are set at compilation like `-d:test=tag0,tag1,...`.
  ##   The default tag is 'true', which means everything should be enabled.
#******

#****f* tags/allTagsEnabled
func allTagsEnabled*: bool =
  ## PURPOSE
  ##   Returns whether all test tags are enabled.
  "*" in testTags
#******

#****f* tags/inTestTags
func inTestTags*(tags: openArray[string]): bool =
  ## PURPOSE
  ##   Tests whether any tag in a set is contained in the set of test tags.
  allTagsEnabled() or not disjoint(tags.toHashSet, testTags)
#******
