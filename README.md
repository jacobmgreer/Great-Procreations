# Great-Procreations

**Role in the ecosystem:** Browser-based deep ancestry visualization. Renders pedigree charts of arbitrary depth using D3.js and an original layout algorithm, deployed as a GitHub Pages site.

## What this project does

`Great-Procreations` is an interactive genealogical visualization tool built entirely for the browser. It takes structured family tree data (sourced from `Family-Tree`) and renders it as a navigable circular pedigree — a "sunburst of ancestry" — that can display dozens of generations without losing structural legibility.

The core challenge it solves is the **exponential ancestor problem**: a pedigree doubles in size with every generation (2 parents, 4 grandparents, 8 great-grandparents, …). Standard tree layouts collapse under this growth within 5–6 generations. `Great-Procreations` uses a custom layout algorithm to manage this growth through adaptive arc compression and progressive disclosure.

## Layout algorithm

The visualization is built on D3.js with a custom radial layout:

- **Radial arcs by generation:** Each generation occupies a concentric ring. Generation 0 (the subject) is the center; generation N is the Nth ring outward.
- **Arc compression:** When a ring becomes too full for individual arcs to be legible, nodes are grouped and compressed. Clicking a compressed arc expands it.
- **Progressive disclosure:** Only the visible rings are fully rendered. Ancestors beyond the current zoom level are kept in memory but not drawn until navigated to.
- **Bidirectional traversal:** The visualization supports navigating forward (to descendants) and backward (to ancestors) from any node, making it possible to explore the full tree from any starting point.

## Data model

Relationships are bidirectional linked lists — each person node has references to parents and children, allowing traversal in either direction from any starting point.

## Deployment

Hosted on GitHub Pages at `https://jacobmgreer.github.io/Great-Procreations/`. The page is self-contained HTML/CSS/JS — no server required, no build step, no dependencies beyond D3.js (loaded from CDN).
