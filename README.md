_diagrams-opengl_ is a very immature backend for [diagrams].  Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

Right now, to use the 3D portion of this project, it is necessary to
build [the nurbs-surface branch of splines][splines], and
[the nurbs branch of diagrams-lib][diagrams-lib].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell
[splines]: https://github.com/bergey/splines/tree/nurbs-surface
[diagrams-lib]: https://github.com/diagrams/diagrams-lib/tree/nurbs

# TODO
- figure out design for embedding 2D diagrams in 3D (on a plane)
- move into diagrams organization on github
