# willmatthews.xyz site infrastructure

![Haskell Build Site](https://github.com/WillMatthews/willmatthews.xyz/actions/workflows/haskell.yml/badge.svg)
![Version](https://img.shields.io/github/v/tag/WillMatthews/willmatthews.xyz?label=version)

This is the infrastructure for my personal site, which is currently under construction.

## Building

To build the site, you will need to have `stack` installed.

```bash
stack build
stack exec site build
```

to run the site locally, you can use

```bash
stack exec site watch
```

## Technologies Used

- hakyll
- sass (hakyll-sass)
- typescript

## Plans

This will replace my existing site, which is made in hugo.
My current site is far too bloated and rubbish, I feel like I need a change.
There is a certain virtue in making it all very lightweight.

I plan on building it ALL up from the bottom, style and all.

This will also be treated as a learning experience to develop my haskell skills (the blade has seriously dulled).

## Timeline

New blog will be up before the end of 2024.

2024:

- August: Basic Prototype of site running
- September: Finish Styling
- December: Improve usability (see gwern.net sidenotes etc)

2025:

- February: Add embeddings, semantic search, etc.
- April: Publications as HTML

## TODO

- Typescript (should I use Elm instead? I have no idea)
- Render LaTeX inline functions (I want to pre-render them all. Mathjax is too heavy).
- Semantic suggestions with embeddings (Pre-calculate and assign for each post).
- Search
- RSS
- Post list
- Tag handling

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
