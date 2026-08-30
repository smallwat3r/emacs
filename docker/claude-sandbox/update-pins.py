#!/usr/bin/env python3
"""Refresh every pinned version, sha256 and image digest in the sandbox
Dockerfile.

Versions come from the upstream release feeds, checksums are computed on the
freshly downloaded artifacts. All edits happen on an in-memory copy written
out only once everything has resolved, so a failed run never leaves the
Dockerfile with mixed pins.

Usage: update-pins.py [dockerfile]   (defaults to the one next to it)
"""

import hashlib
import json
import re
import sys
import urllib.error
import urllib.request
from pathlib import Path

REGISTRY_ACCEPT = (
    "application/vnd.oci.image.index.v1+json, "
    "application/vnd.docker.distribution.manifest.list.v2+json"
)


def fetch(url: str, headers: dict | None = None, method: str = "GET"):
    req = urllib.request.Request(url, headers=headers or {}, method=method)
    return urllib.request.urlopen(req, timeout=60)


def fetch_json(url: str, headers: dict | None = None):
    with fetch(url, headers) as resp:
        return json.load(resp)


def fetch_sha256(url: str) -> str:
    with fetch(url) as resp:
        return hashlib.sha256(resp.read()).hexdigest()


def github_latest_tag(repo: str) -> str:
    tag = fetch_json(f"https://api.github.com/repos/{repo}/releases/latest")["tag_name"]
    if not tag:
        sys.exit(f"failed to resolve latest release of {repo}")
    return tag


def dockerhub_digest(image: str, tag: str) -> str:
    token = fetch_json(
        "https://auth.docker.io/token"
        f"?service=registry.docker.io&scope=repository:library/{image}:pull"
    )["token"]
    with fetch(
        f"https://registry-1.docker.io/v2/library/{image}/manifests/{tag}",
        headers={"Authorization": f"Bearer {token}", "Accept": REGISTRY_ACCEPT},
        method="HEAD",
    ) as resp:
        digest = resp.headers["Docker-Content-Digest"]
    if not digest:
        sys.exit(f"failed to resolve digest of {image}:{tag}")
    return digest


def substitute(text: str, pattern: str, replacement: str, label: str) -> str:
    new_text, count = re.subn(pattern, replacement, text)
    if count == 0:
        sys.exit(f"nothing matched for {label} in the Dockerfile")
    return new_text


def set_arg(text: str, name: str, value: str) -> str:
    if not value:
        sys.exit(f"failed to resolve {name}")
    return substitute(text, rf"(?m)^ARG {name}=.*$", f"ARG {name}={value}", name)


def pin_image_digest(text: str, image: str, tag: str) -> str:
    digest = dockerhub_digest(image, tag)
    text = substitute(
        text, rf"{image}:{tag}@sha256:[0-9a-f]+", f"{image}:{tag}@{digest}", image
    )
    print(f"{image}:{tag} {digest}")
    return text


def pin_base_images(text: str) -> str:
    # node tracks the current LTS line (also rewrites the update comments)
    lts = next(r for r in fetch_json("https://nodejs.org/dist/index.json") if r["lts"])
    major = lts["version"].removeprefix("v").split(".")[0]
    text = substitute(text, r"node:\d+-slim", f"node:{major}-slim", "node tag")
    return pin_image_digest(text, "node", f"{major}-slim")


def pin_go(text: str) -> str:
    go = fetch_json("https://go.dev/dl/?mode=json")[0]
    text = set_arg(text, "GO_VERSION", go["version"])
    for arch in ("amd64", "arm64"):
        tarball = f"{go['version']}.linux-{arch}.tar.gz"
        sha = next((f["sha256"] for f in go["files"] if f["filename"] == tarball), None)
        text = set_arg(text, f"GO_SHA256_{arch.upper()}", sha)
    print("go", go["version"])
    return text


def pin_typst(text: str) -> str:
    typst = github_latest_tag("typst/typst")
    url = f"https://github.com/typst/typst/releases/download/{typst}"
    text = set_arg(text, "TYPST_VERSION", typst)
    for arg_arch, typst_arch in [("AMD64", "x86_64"), ("ARM64", "aarch64")]:
        sha = fetch_sha256(f"{url}/typst-{typst_arch}-unknown-linux-musl.tar.xz")
        text = set_arg(text, f"TYPST_SHA256_{arg_arch}", sha)
    print("typst", typst)
    return text


def pin_mcap(text: str) -> str:
    # mcap releases share a repo with the language libraries, so filter for
    # the CLI's release train rather than taking the repo's latest release
    releases = fetch_json(
        "https://api.github.com/repos/foxglove/mcap/releases?per_page=100"
    )
    prefix = "releases/mcap-cli/"
    tags = (r["tag_name"] for r in releases)
    mcap = next((t.removeprefix(prefix) for t in tags if t.startswith(prefix)), None)
    text = set_arg(text, "MCAP_VERSION", mcap)
    url = f"https://github.com/foxglove/mcap/releases/download/{prefix}{mcap}"
    for arch in ("amd64", "arm64"):
        text = set_arg(
            text,
            f"MCAP_SHA256_{arch.upper()}",
            fetch_sha256(f"{url}/mcap-linux-{arch}"),
        )
    print("mcap", mcap)
    return text


def pin_ocrab(text: str) -> str:
    ocrab = github_latest_tag("smallwat3r/ocrab-font")
    url = f"https://raw.githubusercontent.com/smallwat3r/ocrab-font/{ocrab}/fonts/ocrab.otf"
    text = set_arg(text, "OCRAB_VERSION", ocrab)
    text = set_arg(text, "OCRAB_SHA256", fetch_sha256(url))
    print("ocrab", ocrab)
    return text


def pin_uv(text: str) -> str:
    # uv is pinned by tag only: its COPY --from cannot take an ARG
    uv = github_latest_tag("astral-sh/uv")
    text = substitute(text, r"astral-sh/uv:[0-9.]+", f"astral-sh/uv:{uv}", "uv")
    print("uv", uv)
    return text


def pin_claude_code(text: str) -> str:
    latest = fetch_json("https://registry.npmjs.org/@anthropic-ai/claude-code/latest")
    text = set_arg(text, "CLAUDE_CODE_VERSION", latest["version"])
    print("claude-code", latest["version"])
    return text


def main() -> None:
    dockerfile = (
        Path(sys.argv[1]) if len(sys.argv) > 1 else Path(__file__).parent / "Dockerfile"
    )
    text = dockerfile.read_text()

    for pin in (
        pin_base_images,
        pin_go,
        pin_typst,
        pin_mcap,
        pin_ocrab,
        pin_uv,
        pin_claude_code,
    ):
        text = pin(text)

    staged = dockerfile.with_name(dockerfile.name + ".new")
    staged.write_text(text)
    staged.replace(dockerfile)
    print(f"\nDone. Review: git diff {dockerfile}  then rebuild the sandbox image.")


if __name__ == "__main__":
    try:
        main()
    except urllib.error.URLError as err:
        sys.exit(f"download failed: {err}")
