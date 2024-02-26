# willmatthews.xyz site infrastructure

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

- March: Basic Prototype of site running
- April: Finish Styling
- May: Improve usability (see gwern.net sidenotes etc)
- June: Add embeddings, semantic search, etc.
- July: Publications as HTML

## TODO

- [ ] Typescript
- [ ] Render LaTeX
- [ ] Render code blocks
- [ ] Semantic suggestions with embeddings
- [ ] Search
- [ ] RSS
- [ ] Post list
- [ ] Tag handling

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
