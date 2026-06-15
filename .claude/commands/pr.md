Commit all staged and unstaged changes, push the current branch, and create a pull request.

Steps:
1. Run `git status` and `git diff` to understand all changes.
2. Run `git log --oneline -5` to match the repo's commit message style.
3. Stage all changed and new files relevant to the work (do NOT stage secrets or generated files).
4. Create a commit with a concise message summarizing the "why" of the changes. End with:
   `Co-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>`
5. Push the branch with `git push -u origin HEAD`.
6. Create a PR with `gh pr create` using this format:
   - Title: short, under 70 characters
   - Body:
     ```
     ## Summary
     <1-3 bullet points>

     ## Test plan
     <checklist of verification steps>

     🤖 Generated with [Claude Code](https://claude.com/claude-code)
     ```
7. After pushing, run `gh pr checks <number> --watch` and print the CI results.
8. Print the PR URL.

If $ARGUMENTS is provided, use it as the commit message instead of generating one.
