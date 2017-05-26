# HsBooru
*booru scraper in Haskell

## Why?

Because everything else was shit

## Dependencies

There's a .cabal now, but here are some of the important dependencies

- **scalpel-core**: used for processing HTML
- **http-client**: used for web requests
- **intset**: used to store efficient interval sets (see below)
- **acid-state**: used for the internal state / database
- **optparse-applicative**: used for the CLI
- **streaming**: used for streaming fetch/store AKA worker pattern
- **pipes-concurrency**: used to connect aforementioned streams in parallel

### Note on bitrot

This uses Data.IntervalSet, which currently fails to build on modern GHC
versions in its release version. There's an open pull request that fixes it,
unmerged for reasons unknown to me. You can find the necessary patch here:

- https://github.com/pxqr/intset/pull/3

If you're a gentoo user, you can use my ebuild for this:

- https://github.com/haasn/gentoo-overlay/tree/master/dev-haskell/intset

You might be wondering why I switched away from `Xapian-Haskell` and started
using my own xapian C shim; the answer is simple: `Xapian-Haskell` is
bitrotten to hell and back and I don't even know where to begin fixing it. It
also has a tendency to silently corrupt stuff because of treading
`std::string`s like `const char *`s and vice versa.

So to avoid all of these issues, and avoid reliance on a bitrotten library
that needs custom patches to build anyway, I've included a small wrapper for
the (very few) functions I actually need.

## Installation

`cabal install` etc. should work. If you're a gentoo user, you can use my
ebuild:

https://github.com/haasn/gentoo-overlay/blob/master/net-misc/hsbooru/hsbooru-9999.ebuild

## Configuration

All configuration is done via command line parameters, there is no persistent
configuration file. There are not that many anyway.

## Usage

The program has a built-in `--help` command. Since I'm lazy, I'll just
copy/paste it:

```
hsbooru - a haskell *booru scraper using xapian

Usage: hsbooru COMMAND (-d|--dbDir DIR) [-i|--imageDir DIR] [-b|--batchsize N] [-p|--parallelism N]
               [-j|--jobs N] [-r|--retryCount N]

Available options:
  -d,--dbDir DIR           Database directory
  -i,--imageDir DIR        Directory to store images in. Defaults to `<dbDir>/images`.
  -b,--batchsize N         How many posts to fetch before committing them all to the database. Since
                           this is a synchronous operation, using a lower value reduces
                           throughput. (default: 1000)
  -p,--parallelism N       How many in-flight requests to maintain per thread. Increasing this can
                           improve throughput but going too high risks running into network errors
                           as the site kills connections. (default: 2)
  -j,--jobs N              How many threads to scrape from in parallel. Defaults to the of detected
                           CPU cores, but no more than 4. Going too high can be slower, if the
                           server decides to rate limit.
  -r,--retryCount N        How often to retry each network request before giving up. (default: 3)
  -h,--help                Show this help text

Available commands:
  scrape                   Scrape posts from websites
  update                   Update all previously scraped websites
  retry                    Reset the deleted post database for named sites
  info                     Show some statistics about a named site
```

The semantics of `update` mean that it will re-scrape every site it currently
has in its database (i.e. you've already started scraping at some points). So
you can use this to make a daily cronjob or something.

When you specify `retry SITE`, hsbooru will reset the "deleted" database for
that site. The scraper is generally smart enough to distinguish between
scraper failure (e.g. network/server) and a post acutally being confirmed as
missing. But maybe a post got deleted and then un-deleted or something. Fuck
knows how these mods work. Or maybe something else went wrong. Anyway, this
command allows you to retry all posts we've already marked as "deleted".
