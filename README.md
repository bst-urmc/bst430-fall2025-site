You probably want to be [here](https://urmc-bst.github.io/bst430-fall2021-site/)

# Notes

*  Built with distill / `rmarkdown::render_site()`, deployed to docs/ with ghpages.
*  Consider running `rmarkdown::clean_site(preview = FALSE)` to remove stale content.
*  For ...reasons...  `docs/site_libs` occasionally get replaced with non-versioned directories.  Don't let it (rmarkdown?) do this, it will break the site (invisible pages). The site_libs should have full version strings after the javascript identifier.

