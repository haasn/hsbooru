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

The program has a built-in `--help` command now. Since I'm lazy, I'll just
copy/paste it:

```
hsbooru - a haskell *booru scraper using xapian

Usage: hsbooru [SITE..] [-u|--update] [-r|--retry]
  Scrape posts from *booru sites, download the images, and store metadata in a
  xapian DB. Currently supported sites: gelbooru

Available options:
  SITE..                   Sites to scrape
  -u,--update              Update all previously scraped sites
  -r,--retry               Retry all previously failed posts as well as new
                           posts
  -h,--help                Show this help text

```

The semantics of `--update` mean that it will re-scrape every site it
currently has in its database (i.e. you've already started scraping), but not
any new sites that get added.

So you can put `hsbooru --update` into your daily cronjob, and then it will
keep re-scraping all of the sites you have listed.

When you specify `--retry`, hsbooru will also reset the "failed" database.
Normally, if the server fails serving a page, it gets marked off as "failed" -
since this usually means that the post was deleted - and won't be retried
again. Specifying `--retry` allows you to override this and retry all of the
failed posts as well. Might make a good biyearly cron job to retry stuff that
only failed because of sporadic issues.

**Note**: Individiual page requests will already be retried multiple times, in
case of connection failure. So this only really helps if the server actually
answered, but didn't send us the post data for whatever reason (e.g. rate
limits, transmission got cut off early, etc.)
