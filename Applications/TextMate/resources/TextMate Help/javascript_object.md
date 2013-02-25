title: Commands: JavaScript API for HTML Output

# TextMate JavaScript Object API

The object has the following methods available:

	system()                 See below for information.
	log(msg)                 Adds a message to the system console (using NSLog).
	open(path, options)      Opens a file on disk as a document in the current application.
	                         options may be either a selection range string or a (line) number.

In addition, these properties are exposed:

	busy       (boolean)     The busy spinner in the output window will be displayed when this is true.
	progress   (double, 0-1) Controls the value displayed in the determinate progress indicator.

## TextMate.system()

Also see <http://developer.apple.com/documentation/AppleApplications/Conceptual/Dashboard_ProgTopics/Articles/CommandLine.html>.

### Synchronous Operation

Example:

	obj = TextMate.system("/usr/bin/id -un", null);

Result is an object with following properties:

	outputString:  The output of the command, as placed on stdout.
	errorString:   The output of the command, as placed on stderr.
	status:        The exit status of the command.

### Asynchronous Operation

Example:

	obj = TextMate.system("/usr/bin/id -un", handler);

Handler is called when the command is finished and given an object with the following properties:

	outputString:  The last output of the command, as placed on stdout.
	errorString:   The last output of the command, as placed on stderr.
	status:        The exit status of the command.

Result is an object with following properties/methods:

	outputString:  The current string written to stdout (standard output) by the command.
	errorString:   The current string written to stderr (standard error output) by the command.
	status:        The command’s exit status, as defined by the command.
	onreadoutput:  A function called whenever the command writes to stdout. The handler must accept a single argument; when called, the argument contains the current string placed on stdout.
	onreaderror:   A function called whenever the command writes to stderr. The handler must accept a single argument; when called, the argument contains the current string placed on stderr.
	cancel():      Cancels the execution of the command.
	write(string): Writes a string to stdin (standard input).
	close():       Closes stdin (EOF).
