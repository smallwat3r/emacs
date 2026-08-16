# Sandboxed environment for running Claude Code via Docker.
# Built automatically on first use by bin/claude-docker,
# or manually via sw-claude-rebuild-sandbox in Emacs.
# Pinned by digest so a repointed tag cannot change the base under us.
# Update: docker pull node:22-slim, then copy the new digest from
# docker inspect -f '{{index .RepoDigests 0}}' node:22-slim
FROM node:22-slim@sha256:d649c27dae7ba0137b3cef5dd75baa422c08dc3d9e3fc0c23dfb172dc3cc6436

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

# Go (official tarball - apt's is outdated), pinned and checksum-verified.
# The shas are recorded here on purpose: fetching them from go.dev alongside
# the tarball would verify nothing. Update both from https://go.dev/dl
ARG GO_VERSION=go1.26.6
ARG GO_SHA256_AMD64=708effb774be8237570d0add163225abbdfaf4fca28b2611df167beba4feef89
ARG GO_SHA256_ARM64=d0507e9e9d7fe012aae570108cbd76c15de879e17130ab8cb90d4d7445cb1f2e
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

# Claude Code, pinned to an exact version from npm.
# Update by bumping the version and rebuilding.
ARG CLAUDE_CODE_VERSION=2.1.233
RUN npm install -g "@anthropic-ai/claude-code@${CLAUDE_CODE_VERSION}"

USER node
WORKDIR /home/node

# Rebuilding is the only update path, so disable the in-container auto-updater
# (it fails anyway: the binary lives on the read-only image layer).
ENV DISABLE_AUTOUPDATER=1

ENTRYPOINT ["claude"]
