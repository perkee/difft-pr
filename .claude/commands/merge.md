Merge the current branch's pull request into main.

Steps:
1. Run `gh pr checks` to verify CI is passing.
2. Run `gh pr merge --squash --delete-branch` to merge the PR.
3. Checkout main and pull the latest: `git checkout main && git pull origin main`.
