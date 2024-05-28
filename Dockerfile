# SPDX-FileCopyrightText: 2024 Cognitive Disorders Research Lab
#
# SPDX-License-Identifier: Apache-2.0
# SPDX-License-Identifier: MIT

# This Dockerfile is used to build a Docker image for the aaprop project
# The image is built in two stages:
# 1. The first stage uses the official Rust image as the builder image
#    It builds the Rust project and creates a binary
# 2. The second stage uses the official Debian image as the base image
#    It copies the binary from the builder image and sets it as the entry point of the container

# Use the official Rust image as the builder image
# Use the 1.75 version of the Rust image since it's the MSRV (Minimum Supported Rust Version) for the aaprop project
FROM rust:1.75.0 as builder

# Set the working directory in the builder image to /usr/src
WORKDIR /usr/src

# Create a new Rust project named aaprop
RUN USER=root cargo new --lib kaleidoscope

# Change the working directory to the aaprop directory
WORKDIR /usr/src/kaleidoscope

# Create the appropriate directory structure for the first build
RUN mkdir -p src/kaleidoscope_lib && mv -v src/lib.rs src/kaleidoscope_lib/lib.rs

# Copy the Cargo.toml and Cargo.lock files to the aaprop directory
COPY Cargo.toml Cargo.lock ./

# Build the Rust project
# This step is done separately to take advantage of Docker's layer caching
# Any changes in the source code will not invalidate the cached dependencies
RUN cargo build --lib --no-default-features --release

# Remove the auto-generated main.rs file
# This file will be replaced with the actual source code
RUN rm -rfv src/*

# Remove the auto-generated binary and dependencies
# These will be replaced with the actual binary and dependencies
RUN rm -rfv target/release/deps/kaleidoscope*

# Add the actual source code to the src directory
ADD src src

# Build the Rust project with the actual source code
RUN cargo build --features standalone --no-default-features --release --locked

# Use the official distroless image as the base image
FROM gcr.io/distroless/cc-debian12

# Copy the binary from the builder image to the base image
COPY --from=builder /usr/src/kaleidoscope/target/release/kaleidoscope /usr/local/bin/kaleidoscope

# Change the user to a non-root user for security
USER 1000
