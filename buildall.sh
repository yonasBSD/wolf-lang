#!/bin/bash
set -e # Stop script if any build fails

echo "🦀 Building for Linux..."
cargo build --release

echo "🪟 Building for Windows..."
cargo build --target x86_64-pc-windows-gnu --release

echo "✅ Done! Artifacts are in target/release and target/x86_64-pc-windows-gnu/release"