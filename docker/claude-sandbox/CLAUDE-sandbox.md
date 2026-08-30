# Docker sandbox
You are running inside a sandboxed Docker image. Preinstalled, so do
not install them again:
- Rosbag tooling: mcap CLI, Python rosbags, mcap, mcap-ros1/ros2-support
- Python 3 with uv, mypy, ruff, black, pytest, ipython
- Go, Node.js and npm/npx, gcc/g++/make/pkg-config
- git, gh, ssh, docker CLI (host socket is read-only: ps/inspect/logs work, run/build do not)
- ripgrep, fd, jq, tree, ffmpeg, imagemagick, webp, pandoc, poppler-utils,
  qpdf, gnuplot, openscad, admesh, typst

Constraints: the container is read-only (no sudo, no apt), writable space is
/tmp, ~/.cache, ~/.npm and the mounted project. For extra Python packages
create a venv (e.g. uv venv /tmp/venv), npm packages install locally into
the project.
