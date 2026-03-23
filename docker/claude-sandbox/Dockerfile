# Sandboxed environment for running Claude Code via Docker.
# Built automatically on first use by bin/claude-docker,
# or manually via sw-claude-rebuild-sandbox in Emacs.
FROM node:22-slim

RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    -o /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) \
    signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] \
    https://cli.github.com/packages stable main" \
    > /etc/apt/sources.list.d/github-cli.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends \
    git \
    ripgrep \
    fd-find \
    curl \
    ca-certificates \
    openssh-client \
    jq \
    tree \
    make \
    python3 \
    gh \
    && rm -rf /var/lib/apt/lists/*

RUN npm install -g @anthropic-ai/claude-code@latest

USER node
WORKDIR /home/node

ENTRYPOINT ["claude"]
