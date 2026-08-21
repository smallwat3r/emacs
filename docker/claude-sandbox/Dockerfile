# Sandboxed environment for running Claude Code via Docker.
# Built automatically on first use by bin/claude-docker,
# or manually via sw-claude-rebuild-sandbox in Emacs.
# Pinned by digest so a repointed tag cannot change the base under us.
# Update: docker pull node:22-slim, then copy the new digest from
# docker inspect -f '{{index .RepoDigests 0}}' node:22-slim
FROM node:22-slim@sha256:d649c27dae7ba0137b3cef5dd75baa422c08dc3d9e3fc0c23dfb172dc3cc6436

# Pinned versions and checksums, grouped here so upgrades happen in one place.
# The shas are recorded on purpose: fetching them from the same server as the
# artifact would verify nothing. The uv pin lives in its COPY --from below
# (the legacy builder cannot expand an ARG there).
#
# Go: update version and shas from https://go.dev/dl
ARG GO_VERSION=go1.26.6
ARG GO_SHA256_AMD64=708effb774be8237570d0add163225abbdfaf4fca28b2611df167beba4feef89
ARG GO_SHA256_ARM64=d0507e9e9d7fe012aae570108cbd76c15de879e17130ab8cb90d4d7445cb1f2e
# Typst: update from https://github.com/typst/typst/releases (no upstream
# checksum files, so compute sha256sum on the downloaded tarballs)
ARG TYPST_VERSION=v0.15.1
ARG TYPST_SHA256_AMD64=a6d077d0a95eed5a2eba715b2dae06be954f624ccbf85758a03f389ded33118c
ARG TYPST_SHA256_ARM64=5aa8d74a3d906e60ea12a66ac2f37f8eef1b14cbad7182a745e393a10c23dcee
# Ocrab font: update from https://github.com/smallwat3r/ocrab-font/releases,
# compute sha256sum on the downloaded fonts/ocrab.otf
ARG OCRAB_VERSION=1.0.4
ARG OCRAB_SHA256=af49c6c6f72568c62a0243df25b5912e8e3a7a73c3200284d4d8a49e29abab07
# Claude Code: exact version from npm
ARG CLAUDE_CODE_VERSION=2.1.233

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl ca-certificates \
    && curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) \
    signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] \
    https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list \
    && curl -fsSL https://download.docker.com/linux/debian/gpg \
    -o /usr/share/keyrings/docker-archive-keyring.asc \
    && echo "deb [arch=$(dpkg --print-architecture) \
    signed-by=/usr/share/keyrings/docker-archive-keyring.asc] \
    https://download.docker.com/linux/debian \
    $(. /etc/os-release && echo "$VERSION_CODENAME") stable" \
    > /etc/apt/sources.list.d/docker.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
    # Version control and remote
    git \
    openssh-client \
    gh \
    # SSH-over-proxy tunnel for the egress allowlist (see bin/claude-docker)
    netcat-openbsd \
    # Search and filesystem
    ripgrep \
    fd-find \
    tree \
    jq \
    # Needed by tar to unpack the Typst .tar.xz below
    xz-utils \
    # Build tools
    make \
    gcc \
    g++ \
    pkg-config \
    python3 \
    python3-pip \
    # Docker CLI (talks to host via socket proxy)
    docker-ce-cli \
    # Media processing
    ffmpeg \
    imagemagick \
    webp \
    # Document conversion
    pandoc \
    # PDF utilities (pdftotext, pdfunite, pdftoppm, qpdf...)
    poppler-utils \
    qpdf \
    # Plotting
    gnuplot \
    # 3D / CAD
    openscad \
    admesh \
    && rm -rf /var/lib/apt/lists/*

# Python package manager, copied from its official image (pinned)
COPY --from=ghcr.io/astral-sh/uv:0.12.5 /uv /uvx /usr/local/bin/

# Python tooling (available system-wide)
RUN uv pip install --system --break-system-packages \
    mypy \
    ruff \
    black \
    pytest \
    ipython

# Go (official tarball - apt's is outdated), pinned and checksum-verified
RUN ARCH="$(dpkg --print-architecture)" \
    && case "${ARCH}" in \
    amd64) GO_SHA256="${GO_SHA256_AMD64}" ;; \
    arm64) GO_SHA256="${GO_SHA256_ARM64}" ;; \
    *) echo "unsupported arch: ${ARCH}" >&2; exit 1 ;; \
    esac \
    && curl -fsSL "https://go.dev/dl/${GO_VERSION}.linux-${ARCH}.tar.gz" \
    -o /tmp/go.tar.gz \
    && echo "${GO_SHA256}  /tmp/go.tar.gz" | sha256sum -c - \
    && tar -C /usr/local -xzf /tmp/go.tar.gz \
    && rm /tmp/go.tar.gz
ENV PATH="/usr/local/go/bin:/home/node/go/bin:/home/node/.local/bin:${PATH}"

# Typst (official musl tarball), pinned and checksum-verified
RUN ARCH="$(dpkg --print-architecture)" \
    && case "${ARCH}" in \
    amd64) TYPST_ARCH=x86_64; TYPST_SHA256="${TYPST_SHA256_AMD64}" ;; \
    arm64) TYPST_ARCH=aarch64; TYPST_SHA256="${TYPST_SHA256_ARM64}" ;; \
    *) echo "unsupported arch: ${ARCH}" >&2; exit 1 ;; \
    esac \
    && curl -fsSL "https://github.com/typst/typst/releases/download/${TYPST_VERSION}/typst-${TYPST_ARCH}-unknown-linux-musl.tar.xz" \
    -o /tmp/typst.tar.xz \
    && echo "${TYPST_SHA256}  /tmp/typst.tar.xz" | sha256sum -c - \
    && tar -C /usr/local/bin --strip-components=1 -xJf /tmp/typst.tar.xz \
    "typst-${TYPST_ARCH}-unknown-linux-musl/typst" \
    && rm /tmp/typst.tar.xz

# Ocrab font (https://github.com/smallwat3r/ocrab-font), pinned to a release
# tag and checksum-verified. Typst picks it up from the system font dirs.
RUN mkdir -p /usr/share/fonts/opentype/ocrab \
    && curl -fsSL "https://raw.githubusercontent.com/smallwat3r/ocrab-font/${OCRAB_VERSION}/fonts/ocrab.otf" \
    -o /usr/share/fonts/opentype/ocrab/ocrab.otf \
    && echo "${OCRAB_SHA256}  /usr/share/fonts/opentype/ocrab/ocrab.otf" | sha256sum -c -

# Let openscad render headless (no display) via the CLI
ENV QT_QPA_PLATFORM=offscreen

# The host's ~/.claude is shared into the container, but its config stores
# absolute paths under the host home (e.g. plugin marketplace locations).
# Symlink the host home to /home/node so those paths resolve, and the
# container runs with HOME set to the host home (see bin/claude-docker).
# Defaults to /home/node, keeping the build a no-op when they already match.
ARG HOST_HOME=/home/node
# Stamp the home this image was built for, so the launcher rebuilds when a
# cached image was built for a different (or default) home (see bin/claude-docker).
LABEL host_home="${HOST_HOME}"
RUN if [ "$HOST_HOME" != /home/node ]; then \
    mkdir -p "$(dirname "$HOST_HOME")" \
    && ln -s /home/node "$HOST_HOME"; \
    fi

# Same dir as /home/node/.local/bin via the symlink, but /doctor checks PATH for
# the literal $HOME/.local/bin (the host home), so add it to silence the warning.
ENV PATH="${HOST_HOME}/.local/bin:${PATH}"

# Claude Code, pinned at the top. Update by bumping the version and rebuilding.
RUN npm install -g "@anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}"

USER node
WORKDIR /home/node

# Rebuilding is the only update path, so disable the in-container auto-updater
# (it fails anyway: the binary lives on the read-only image layer).
ENV DISABLE_AUTOUPDATER=1

ENTRYPOINT ["claude"]
