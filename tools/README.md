# Tools

This directory contains third-party tools used by the project.

## Structure

Each tool is in its own subdirectory:

| Directory | Tool | Purpose |
|-----------|------|---------|
| `puml/` | PlantUML | UML diagram generation |

## PlantUML

Used by `make diagrams` to generate SVG diagrams from `.puml` source files.

```bash
java -jar tools/puml/plantuml-1.2025.9.jar -tsvg docs/diagrams/*.puml
```
