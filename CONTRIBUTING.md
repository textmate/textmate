# Contributing

You can send pull requests via GitHub. Patches should:

1. Follow the style of the existing code.
2. One commit should do exactly one thing.
3. Commit messages should start with a summary line below 70 characters followed by a blank line, and then the reasoning/analysis for why the change was made (if appropriate).
4. Commits that fix a bug in a previous commit should:
    1. If existing commit is already upstream: Start with `fixup!` and then the summary line of the commit it fixes. If you are writing your commit message in TextMate then type `fix⇥` to get the prefix and a menu allowing you to pick the summary line from one of the last 15 commits. If you commit from a terminal you can use `git commit --fixup=«commit»`
    2. If existing commit is a local commit: You should amend the existing commit! Either use `git commit --amend` or do a `fixup!` followed by `git rebase -i` (to merge in the fixup). I.e. do not send a pull request where some of the commits you want us to pull fixes bugs introduced in other commits you also want us to pull.
5. Rebase your branch against the upstream’s master. We don’t want to pull redundant merge commits.
6. **Be clear about what license applies to your patch:** The files within this repository are under the [GPL 3][] (or later) but (as the original creator) we are still allowed to create non-free derivatives. However, if patches are given to us under GPL then those cannot make it into any non-free derivatives we may later wish to create. So to make it easier for us (and avoid any legal issues) we prefer if patches are released as public domain.

There is the [textmate][] mailing list and [#textmate][] IRC channel at [freenode.net][] where this project can be discussed.

## GitHub Workflow

Developing patches should follow this workflow:

### Initial Setup

1.	Fork on GitHub (click Fork button)
2.	Clone to computer: `git clone git@github.com:«github account»/textmate.git`
3.	cd into your repo: `cd textmate`
4.	Set up remote upstream: `git remote add -f upstream git://github.com/textmate/textmate.git`

### Adding a Feature

1.	Create a branch for the new feature: `git checkout -b my_new_feature`
2.	Work on your feature, add and commit as usual

Creating a branch is not strictly necessary, but it makes it easy to delete your branch when the feature has been merged into upstream, diff your branch with the version that actually ended in upstream, and to submit pull requests for multiple features (branches).

### Pushing to GitHub

8.	Push branch to GitHub: `git push origin my_new_feature`
9.	Issue pull request: Click Pull Request button on GitHub

### Useful Commands

If a lot of changes has happened upstream you can replay your local changes on top of these, this is done with `rebase`, e.g.:

	git fetch upstream
	git rebase upstream/master

This will fetch changes and re-apply your commits on top of these.

This is generally better than merge, as it will give a clear picture of which commits are local to your branch. It will also “prune” any of your local commits if the same changes have been applied upstream.

You can use `-i` with `rebase` for an “interactive” rebase. This allows you to drop, re-arrange, merge, and reword commits, e.g.:

	git rebase -i upstream/master

## Changing a xib File

When you change a `xib` file then please look at the diff before you push. If the diff seems to have a lot of changes unrelated to what actually did change, please revert back to `HEAD` and open the pristine `xib` in Xcode and save that (without changing anything).

Commit this saved `xib` with a commit message of `Save xib file with Xcode «version»`. Here version is the version of Xcode you are using, but be sure you don’t downgrade the format. To check the version that `resources/English.lproj/MainMenu.xib` was last saved with, you can run (add appropriate grep if desired):

	git log --oneline resources/English.lproj/MainMenu.xib

You can safely assume that all `xib` files without such message are saved with Xcode 4.4 or earlier (i.e. you won’t downgrade them).

After this, re-apply your change and commit. If the change is non-trivial it is a good idea to write how you made the change in the commit body. E.g. a commit message could be:

	Disable install button when we can’t install
	
	The install button’s “enabled” property
	has been bound to the “canInstall”
	property of File’s Owner.

[GPL 3]:          http://www.gnu.org/copyleft/gpl.html
[textmate]:       http://lists.macromates.com/listinfo/textmate
[#textmate]:      irc://irc.freenode.net/#textmate
[freenode.net]:   http://freenode.net/
