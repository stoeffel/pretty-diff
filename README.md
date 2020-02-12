# pretty-diff

Pretty printing a diff of two values.

## Usage

```hs
import qualified Pretty.Diff as Diff
import Data.Default (def)

Diff.pretty def "1234" "_23"
```
Will create a string that looks like this:

```
 ▼ ▼
"1234"
╷
│
╵
"_23"
 ▲
```
