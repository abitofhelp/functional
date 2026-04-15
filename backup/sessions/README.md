# backup/sessions/

Durable, git-tracked archive of session artifacts for this project.

## What lives here

Three kinds of files, all using the same filename convention so
chronological sort works across kinds:

```
<YYYYMMDDTHHMMSSZ>__<original-basename>
```

| Kind | How it gets here | Source of the content |
|---|---|---|
| `*_alignment_note.md` | Written directly by the assistant when a slice opens with a design decision. Committed with the slice's first commit. | The assistant's pre-coding review of the frozen docs + GPT check. |
| `*_pr_review_package.md` | Written directly by the assistant before a PR is opened. Committed with the slice's PR. | The assistant's self-contained review document for GPT. |
| `*` (time-driven snapshots) | Mirrored here by `scripts/python/shared/session_snapshot`, typically via the `/snapshot` custom slash command. | A Claude Code memory file from `~/.claude/projects/<encoded-project>/memory/`, preserved verbatim after the `<timestamp>__` prefix. |

Filenames sort naturally by UTC timestamp regardless of how many
different kinds coexist in the directory. Recovery of any file is a
plain `cp` with prefix strip (see
`scripts/python/shared/session_snapshot/README.md` for the
`--restore` tool or the manual procedure).

## Relationship to `backup/sessions/raw/`

`raw/` is **gitignored** and holds compressed full-session `.jsonl`
backups produced by `scripts/python/shared/jsonl_snapshot`. Those
files are large (~10 MB per snapshot) and intended for external
storage. Git-tracked files stay up here in `backup/sessions/`.

See `scripts/python/shared/jsonl_snapshot/README.md` for the full
forensic-tier workflow.

## What this is NOT

- **Not the Claude Code `/export` command output.** The CLI
  `/export` has been broken for some time; this directory is the
  workflow that replaces it.
- **Not a full conversation transcript.** Memory files are
  curated recap summaries written by the assistant at key
  workflow points. Use `jsonl_snapshot` and the raw session
  `.jsonl` under `~/.claude/projects/.../<uuid>.jsonl` for
  forensic recovery of verbatim conversation content.
- **Not a replacement for the Claude Code memory system.**
  Memory files in `~/.claude/projects/.../memory/` remain the
  auto-loaded source of strategic context at session start.
  This directory is the off-machine-durable backup copy that
  survives a local-state loss.
