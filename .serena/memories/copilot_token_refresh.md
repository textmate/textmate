# Copilot Token Refresh (from lsp-client Go project)

## Config Location
`~/.config/github-copilot/` directory containing:
- `hosts.json` or `apps.json` — stores `oauth_token` per host (e.g. `github.com`)
- `token.json` — cached API token with expiry

## Token Refresh Flow
1. On startup, `startCopilotTokenMaintenance()` launches a background goroutine (singleton via sync.Once)
2. `copilotTokenRefresher` loads OAuth token from `apps.json` or `hosts.json` (looks for `oauth_token` field under a `github.com` host entry)
3. Refreshes API token via `GET https://api.github.com/copilot_internal/v2/token` with `Authorization: token <oauth_token>` header
4. Response: `{ "token": "...", "expires_at": <unix_timestamp> }`
5. Writes to `token.json` (atomic via tmp+rename) and updates `copilot_token`/`copilot_token_expires_at` in `apps.json`
6. Sleeps until 2 minutes before expiry, then refreshes again. Min sleep: 1min, max: 30min
7. File locking via `.lock` file with stale detection (2min max age)

## Authentication Flow (Device Flow)
1. `signInInitiate` → returns `userCode` + `verificationUri`
2. User opens browser, enters code
3. `signInConfirm` with `userCode` → returns status + username
4. `checkStatus` → `OK`, `AlreadySignedIn`, `NotAuthorized`, `NotSignedIn`

## Auto Re-auth on Error
`sendRequestWithAuth` detects auth errors (code 1000, "NotSignedIn", etc.) and triggers `handleReauthentication()` which retries the original request once after re-auth.

## Editor Info Sent to Copilot
`setEditorInfo` with `editorInfo: {name: "Textmate", version: "2.0.23"}` and `editorPluginInfo: {name: "lsp-client", version: "0.1.0"}`
