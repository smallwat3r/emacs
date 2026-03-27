# Sandboxed environment for running Claude Code via Docker.
# Built automatically on first use by bin/claude-docker,
# or manually via sw-claude-rebuild-sandbox in Emacs.
FROM node:22-slim

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
    # Search and filesystem
    ripgrep \
    fd-find \
    tree \
    jq \
    # Build tools
    make \
    python3 \
    python3-pip \
    # Docker CLI (talks to host via socket proxy)
    docker-ce-cli \
    # Media processing
    ffmpeg \
    imagemagick \
    webp \
    && rm -rf /var/lib/apt/lists/*

# Python package manager
RUN curl -LsSf https://astral.sh/uv/install.sh | sh \
    && mv /root/.local/bin/uv /usr/local/bin/ \
    && mv /root/.local/bin/uvx /usr/local/bin/

RUN npm install -g @anthropic-ai/claude-code@latest

USER node
WORKDIR /home/node

ENTRYPOINT ["claude"]
