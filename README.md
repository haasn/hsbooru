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

## Configuration

There are some tunables in `HsBooru/Conf.hs`, in lieau of a more sensible
configuration system.

## Usage

The program has a built-in `--help` command now. Since I'm lazy, I'll just
copy/paste it:

```
hsbooru - a haskell *booru scraper using xapian

Usage: hsbooru COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  scrape                   Scrape posts from websites
  update                   Update all previously scraped websites
  retry                    Reset the failed post database for named sites
```

The semantics of `update` mean that it will re-scrape every site it currently
has in its database (i.e. you've already started scraping at some points). So
you can use this to make a daily cronjob or something.

When you specify `retry SITE`, hsbooru will reset the "failed" database for
that site. Normally, if the server fails serving a page, it gets marked off as
"failed" - since this usually means that the post was deleted - and won't be
retried again. Manually running `retry` allows you to clear this database,
which means that the next scrape will include all of the previously-failed
posts as well.

**Note**: Individiual page requests will already be retried multiple times, in
case of connection failure. So this only really helps if the server actually
answered, but didn't send us the post data for whatever reason (e.g. rate
limits, transmission got cut off early, etc.)
