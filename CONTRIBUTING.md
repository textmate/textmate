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
