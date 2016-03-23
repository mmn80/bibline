bibline
=======

An incremental parser and command line utility for processing `BibTeX` files.

## Parser

The parser is implemented with the `pipes-parse` combinators, and runs in
constant memory, unless the results are sorted, in which case it's
*O(results)*.
When parsing errors are encountered, a message and a `Producer` containing
unused input are returned.

I tried to implement all the rules mentioned on the [Wikipedia page][wiki]
and on the [home page][spec] (could not find a more precise spec).
Author names and keywords are parsed into components, but the rest of the
fields are kept as-is.

An example use of the parser:
```Haskell
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Text.IO as PT
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Text.Bibline.Parser (biblined, showEntryCompact)

main = runEffect $
         for (biblined PT.stdin >-> P.map (T.pack . showEntryCompact)) $
           liftIO . T.putStr
```

## Command line utility

The command line utility can be used to filter and sort `BibTeX` content
from `stdin` to either `BibTeX` or a compact display format on `stdout`.

With option `-o` the first result's `file` will be opened with either
the default program registered in the OS, or your own command specified with
`--opencmd`.

For example, to create a new file with references sorted by year, use:
```
cat references.bib | bibline -s year -b > references1.bib
```

## License

Copyright © 2016 Călin Ardelean

MIT license. See the [license file][MIT] for details.

[wiki]: https://en.wikipedia.org/wiki/BibTeX "Wikipedia BibTeX page"
[spec]: http://www.bibtex.org/Format/ "BibTeX home page"
[MIT]: https://github.com/mmn80/bibline/blob/master/LICENSE.md "MIT License File"
