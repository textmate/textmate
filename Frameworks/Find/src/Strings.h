#define FFString(msg, description) NSLocalizedStringFromTableInBundle(msg, @"Find", [NSBundle bundleForClass:NSClassFromString(@"Find")], description)

// ===========================
// = Find all/find in folder =
// ===========================

#define MSG_ZERO_MATCHES_FMT                 FFString(@"No results found for “%@”.",        @"Find all completion status, zero matches.")
#define MSG_ONE_MATCH_FMT                    FFString(@"Found one result for “%@”.",        @"Find all completion status, one match.")
#define MSG_MULTIPLE_MATCHES_FMT             FFString(@"Found %2$@ results for “%1$@”.",    @"Find all completion status, multiple matches.")

#define MSG_SEARCHED_FILES_ONE               FFString(@" (searched one file in %2$.1f seconds)",  @"Find all completion status, one file")
#define MSG_SEARCHED_FILES_MULTIPLE          FFString(@" (searched %@ files in %.1f seconds)",    @"Find all completion status, multiple files")

#define MSG_SEARCHING_FOLDER_FMT             FFString(@"Searching “%@”…",     @"Find all progress status.")
#define MSG_SEARCHING_FMT                    FFString(@"Searching…",          @"Find all initial progress status.")

#define MSG_FIND_ALL_ABORTED                 FFString(@"Stopped search in folder “%@”.", @"Find all was prematurely terminated.")

#define MSG_FIND_ALL_IN_DOCUMENT_HEADER      FFString(@"Find All Results in “%@”",                @"Find all in document, results list header text.")
#define MSG_FIND_ALL_RESULTS                 FFString(@"Find All Results in “%@” matching “%@”",  @"Find all in files, results list header text.")
#define MSG_FIND_ALL_RESULTS_OPEN_FILES      FFString(@"Find All Results in Open Documents",      @"Find all in open documents, results list header text.")

#define MSG_REPLACE_ALL_RESULTS              FFString(@"%d replacements made across %d file(s).", @"Replace all, replacement count status text")

#define MSG_WINDOW_TITLE                     FFString(@"Find",       @"Default window title.")
#define MSG_FIND_IN_FOLDER_WINDOW_TITLE      FFString(@"Find — %@",  @"Find in folder, window title with path.")
