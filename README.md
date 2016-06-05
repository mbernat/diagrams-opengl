This project is (starting to be) maintained again.
The master branch is rather outdated (and will stay that way for the time being).
The work of bringing this project up-to-date with current libs can be found at the `update-to-2016-deps` branch.

_diagrams-opengl_ is a very immature backend for [diagrams].  Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell

Eventually OpenGL should render both 2D and 3D Diagrams.  The 2D works
now, but there's a fair bit of work making it less buggy.  Right now,
for 3D, your best bet is
[the POV-Ray Backend](https://github.com/diagrams/diagrams-povray).

If you want to contribute, there's work to be done both on the OpenGL
side (writing shaders, translating the 2D Haskell code to 3D) and on
the library design side (adding 3D primitives and useful functions for
working with them).
