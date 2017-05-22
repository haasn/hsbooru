# HsBooru
*booru scraper in Haskell

## Why?

Because everything else was shit

## Dependencies

There's a .cabal now, but here are some of the important dependencies

- **scalpel-core**: used for processing HTML
- **http-client**: used for web requests
- **intset**: used to store efficient interval sets
- **acid-state**: used for the internal state / database
- **Xapian-Haskell**: used to store post metadata and tag search

### Note on bitrot

Both Xapian-Haskell and intset currently fail to build/work in their current
release versions. I use custom patches for both. If you really want to use
this, then you need to apply them yourself:

- https://github.com/pxqr/intset/pull/3
- https://github.com/haasn/gentoo-overlay/blob/master/dev-haskell/xapian-haskell/files/xapian-haskell.patch

If you're a gentoo user, you can use my ebuilds for these:

- https://github.com/haasn/gentoo-overlay/tree/master/dev-haskell/intset
- https://github.com/haasn/gentoo-overlay/tree/master/dev-haskell/xapian-haskell

I'd also strongly avoid trying to use xapian-haskell or relying on it too
much, it's stupidly bitrotten and broken to the core; half the stuff just
plain doesn't work, others segfault, etc.

The few functions I access seem to sort of work. One of my next goals with
this program is to move away from xapian-haskell and maybe write my own
minimalistic bindings for xapian instead, since my needs are very basic.

## Configuration

There are some tunables near the beginning of the code. You may want to play
around with the `threadCount`, especially on systems with fewer cores. You might
also want to drop down the `batchSize` to compensate.

Everything is stored relative to `dbDir`, including the images. You can, of
course, trivially change these paths if need be.

## Usage

Since I'm too lazy to write in-program help or a man page, this is how you use
it:

```bash
$ hsbooru [sitename...]
```

If no sitename is specified, it will re-scrape all of the currently active
sites. So for your cronjob you can just run `hsbooru` and it will continue
scraping whatever it is you're scraping. If you want to add a new site, or do
the initial sync of a new site, you'll need to specify the name on the command
line.

But of course, currently only `gelbooru` is supported...
