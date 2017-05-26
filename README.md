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

Also, you'll need to have xapian-core installed, which cabal can't do
automatically. Refer to your packages. I don't think the version really
matters, but I'm on 1.4.X.

## Installation

**IMPORTANT**: Remember to install xapian (usually called `xapian-core`) first!

Since this depends on a newer version of `intset` than what's available in the
repos, I've added it as a local submodule. To build it correctly, you need to
use the `new`-style cabal build system, like this:

```
$ git submodule update --init
$ cabal new-build
```

You can also install the package using `cabal new-install`... well, you could
in theory, assuming the cabal people actually got around to implementing that
command. Oh well, life is hard.

In the absence of the new build system, you can also use a local sandbox to
build the package:

```
$ git submodule update --init
$ cabal sandbox init
$ cabal sandbox add-source extern/intset
$ cabal install
```

This method also allows you to run tests (`cabal test`)

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
