## 1 Code Overview

![Project Dependency Graph](./doc/DEPENDENCY_GRAPH.png)

- The top level of this project hosts the primary Haskell modules.

- The embedded submodules `simula-wayland`, `simula-osvr`, and `simula-openvr` host FFI marshalling modules (via c2hs) to the respective C libraries shown above.

- In order to run the vive-compositor, this graph also shows a dependency on `nvidia-381.26.13` or greater, which `nix` cannot provide. You do not need nvidia drivers, however, to run the `base-compositor`.

- Simula is a spiritual fork (i.e., complete reimplementation and improvement over) [motorcar](https://github.com/evil0sheep/motorcar). To read about motorcar, see [Toward General Purpose 3D User Interfaces: Extending Windowing Systems to Three Dimensions](https://github.com/evil0sheep/MastersThesis/blob/master/thesis.pdf?raw=true).

## 2 How to Contribute

See the this project's [Issues](https://github.com/georgewsinger/SimulaHS/issues) for a list of ways to immediately contribute. Issued may be tagged as "new contributor" if they are especially appropriate for people getting adjusted to the code. In addition, [TODO.md](./TODO.md) sketches some additional issues with the code that may be solved via future contributions.

DISCLAIMER: All pull requests to this repo implicitly consent to, and will be subsumed under the terms of the [LICENSE](./LICENSE).
