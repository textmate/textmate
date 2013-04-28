title: Matching Context Scopes

# Non-content Scopes

There are a few scopes which are not related to the content.

## Dynamic Scopes

Presently I think only `dyn.selection` is available. This allow key bindings which are only active when there is a selection. One example of this could be to overload the tab key to shift right, only when text is selected.

Another example is the “Wrap in Braces” command in the included Avian bundle. This is bould to `{` and indents the selection plus puts braces on first/last line.

Other dynamic scopes planned: `dyn.caret.(begin|end).(line|document)`. This will allow keys/snippets to only trigger when caret is at the beginning/ending of the line or document.

## SCM Scopes

The SCM system used is available as `scm.attr.«name»`. A few more SCM scopes are available, for Git the current branch, and status (although that is presently not standardized).

## Other

The path is available (reversed) as `attr.rev-path.«path»`. This is so that we can bind to e.g. `attr.rev-path.erb` to target all files with an ERb extension. This is more relevant for events or injection than key bindings.

OS version is available. This allows us to have different versions of the same command for different OS versions (sometimes easier than testing for the OS version in the command when there is little code sharing between the two versions).
