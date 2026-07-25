# Git Branching Cheat Sheet

## Core concept

**`main` branch** = always working, always deployable.
**Feature branches** = experimental work lives here until proven.

---

## When to branch

| Situation | Approach |
|-----------|----------|
| Bug fix — small, obvious | Fix directly on `main` |
| Bug fix — risky or complex | Short branch → test → merge |
| New feature or experiment | Feature branch → merge when confident |
| Major rewrite / uncertain | Long-lived feature branch |

---

## Essential commands

### See where you are
```bash
git branch                  # list branches (* = current)
git status                  # show uncommitted changes
git log --oneline -10       # last 10 commits on current branch
```

### Create and switch to a new branch
```bash
git checkout -b feature/my-feature     # create + switch in one step
```

### Switch between branches
```bash
git checkout main                      # go back to main
git checkout feature/my-feature        # go to feature branch
```

### Save your work on a branch
```bash
git add R/utils.R                      # stage specific file
git add -p                             # stage interactively (choose hunks)
git commit -m "Add qs serialization support"
```

### Merge a finished feature back into main
```bash
git checkout main                      # switch to main first
git merge feature/my-feature           # bring in the feature branch
```

### Delete a branch after merging
```bash
git branch -d feature/my-feature       # safe delete (warns if unmerged)
git branch -D feature/my-feature       # force delete
```

### See differences
```bash
git diff                               # unstaged changes
git diff main..feature/my-feature      # what the branch adds vs main
git log main..feature/my-feature       # commits on branch not yet in main
```

---

## Typical feature branch workflow

```bash
# 1. Start from main
git checkout main
git pull                               # make sure main is up to date

# 2. Create feature branch
git checkout -b feature/qs-serialization

# 3. Make changes, commit as you go
git add R/utils.R DESCRIPTION
git commit -m "Add qs read/write support to read_file and save_file"

# 4. Test thoroughly on the branch

# 5. Merge back when confident
git checkout main
git merge feature/qs-serialization

# 6. Clean up
git branch -d feature/qs-serialization
```

---

## If things go wrong on a branch

```bash
# Discard all uncommitted changes (careful — irreversible)
git checkout -- .

# Undo the last commit but keep the changes
git reset HEAD~1

# Abandon the branch entirely and go back to main
git checkout main
git branch -D feature/my-feature      # delete the branch
```

---

## GitHub: pushing a branch

```bash
git push -u origin feature/qs-serialization    # first push
git push                                        # subsequent pushes
```

To propose merging into main on GitHub: open a **Pull Request** from
the branch. Useful for review before merging, even when working solo.

---

## Quick reference card

| What | Command |
|------|---------|
| Where am I? | `git branch` |
| New branch | `git checkout -b feature/name` |
| Switch branch | `git checkout main` |
| Save work | `git add file.R && git commit -m "message"` |
| Merge to main | `git checkout main && git merge feature/name` |
| Delete branch | `git branch -d feature/name` |
| What changed? | `git diff main..feature/name` |
