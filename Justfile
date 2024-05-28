# SPDX-FileCopyrightText: 2024 Cognitive Disorders Research Lab
#
# SPDX-License-Identifier: Apache-2.0
# SPDX-License-Identifier: MIT

# Rebuild project when rs and toml files change
watch:
    just build
    cargo watch -c -i "*.rs" -i "*.toml" -s "just build"

# Pull out rust and toml code fences into their files
build:
    cargo build

# Run bacon
bacon:
    bacon

# make-compatible bootstrap
deps:
    cargo install cargo-binstall
    cargo binstall --locked bacon \
    cargo-watch just mdbook mdbook-inline-highlighting \
    mdbook-toc mdbook-plantuml
