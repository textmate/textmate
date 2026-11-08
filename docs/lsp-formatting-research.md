# LSP textDocument Formatting Research

## 1. Request/Response Formats

### textDocument/formatting

**Request JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "textDocument/formatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.ts"
    },
    "options": {
      "tabSize": 2,
      "insertSpaces": true,
      "trimTrailingWhitespace": true,
      "insertFinalNewline": true,
      "trimFinalNewlines": true
    },
    "workDoneToken": "optional-request-id"
  }
}
```

**Response JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": [
    {
      "range": {
        "start": { "line": 0, "character": 0 },
        "end": { "line": 5, "character": 10 }
      },
      "newText": "formatted code here"
    },
    {
      "range": {
        "start": { "line": 10, "character": 0 },
        "end": { "line": 10, "character": 0 }
      },
      "newText": "\n"
    }
  ]
}
```

Or null if no formatting is needed:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": null
}
```

**Parameter Type:** DocumentFormattingParams
**Response Type:** TextEdit[] | null

---

### textDocument/rangeFormatting

**Request JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "textDocument/rangeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.ts"
    },
    "range": {
      "start": { "line": 5, "character": 0 },
      "end": { "line": 15, "character": 20 }
    },
    "options": {
      "tabSize": 2,
      "insertSpaces": true,
      "trimTrailingWhitespace": true,
      "insertFinalNewline": true,
      "trimFinalNewlines": true
    },
    "workDoneToken": "optional-request-id"
  }
}
```

**Response JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": [
    {
      "range": {
        "start": { "line": 5, "character": 0 },
        "end": { "line": 6, "character": 10 }
      },
      "newText": "formatted range"
    }
  ]
}
```

**Parameter Type:** DocumentRangeFormattingParams
**Response Type:** TextEdit[] | null

---

### textDocument/onTypeFormatting

**Request JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "textDocument/onTypeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///path/to/file.ts"
    },
    "position": {
      "line": 10,
      "character": 5
    },
    "ch": "}",
    "options": {
      "tabSize": 2,
      "insertSpaces": true,
      "trimTrailingWhitespace": true,
      "insertFinalNewline": true,
      "trimFinalNewlines": true
    }
  }
}
```

**Response JSON-RPC Structure:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": [
    {
      "range": {
        "start": { "line": 8, "character": 0 },
        "end": { "line": 8, "character": 0 }
      },
      "newText": "  "
    },
    {
      "range": {
        "start": { "line": 10, "character": 5 },
        "end": { "line": 10, "character": 6 }
      },
      "newText": " }\n"
    }
  ]
}
```

**Parameter Type:** DocumentOnTypeFormattingParams
**Response Type:** TextEdit[] | null

---

## 2. FormattingOptions Structure

**Complete TypeScript Interface:**
```typescript
interface FormattingOptions {
  tabSize: uinteger;
  insertSpaces: boolean;
  trimTrailingWhitespace?: boolean;
  insertFinalNewline?: boolean;
  trimFinalNewlines?: boolean;
  [key: string]: boolean | integer | string | undefined;
}
```

**JSON Example:**
```json
{
  "tabSize": 2,
  "insertSpaces": true,
  "trimTrailingWhitespace": true,
  "insertFinalNewline": true,
  "trimFinalNewlines": true,
  "customKey": "customValue"
}
```

**Field Definitions:**

| Field | Type | Required | Introduced | Description |
|-------|------|----------|------------|-------------|
| `tabSize` | uinteger | Yes | Core | Size of a tab in spaces |
| `insertSpaces` | boolean | Yes | Core | Prefer spaces over tabs |
| `trimTrailingWhitespace` | boolean | Optional | 3.15.0 | Trim trailing whitespaces on a line |
| `insertFinalNewline` | boolean | Optional | 3.15.0 | Insert a newline character at the end of the file if one does not exist |
| `trimFinalNewlines` | boolean | Optional | 3.15.0 | Trim all newlines after the final newline at the end of the file |
| `[key: string]` | boolean \| integer \| string | Optional | Core | Signature for further implementation-specific properties |

---

## 3. TextEdit Structure

**Complete TypeScript Interface:**
```typescript
interface TextEdit {
  range: Range;
  newText: string;
}

interface Range {
  start: Position;
  end: Position;
}

interface Position {
  line: uinteger;
  character: uinteger;
}
```

**JSON Example:**
```json
{
  "range": {
    "start": { "line": 0, "character": 0 },
    "end": { "line": 0, "character": 10 }
  },
  "newText": "replacement text"
}
```

**Rules:**
- To insert text into a document, create a range where `start === end`
- For delete operations, use an empty string for `newText`
- Text edit ranges must never overlap
- No part of the original document must be manipulated by more than one edit
- All text edit ranges refer to positions in the original document

---

## 4. Server Capability Advertisement

### Initialize Response Structure

The server declares formatting support in the `serverCapabilities` section of the initialize response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "capabilities": {
      "documentFormattingProvider": true,
      "documentRangeFormattingProvider": true,
      "documentOnTypeFormattingProvider": {
        "firstTriggerCharacter": "}",
        "moreTriggerCharacter": [";", "\n"]
      }
    }
  }
}
```

### Capability Types

**documentFormattingProvider:**
- Type: `boolean | DocumentFormattingOptions`
- Indicates support for `textDocument/formatting`
- Simple boolean form: `true` or `false`
- Options form allows specifying work done progress support

**documentRangeFormattingProvider:**
- Type: `boolean | DocumentRangeFormattingOptions`
- Indicates support for `textDocument/rangeFormatting`
- Same structure as documentFormattingProvider

**documentOnTypeFormattingProvider:**
- Type: `DocumentOnTypeFormattingOptions` (not boolean)
- Indicates support for `textDocument/onTypeFormatting`
- MUST include `firstTriggerCharacter` (single character)
- MAY include `moreTriggerCharacter` (array of additional characters)

### DocumentOnTypeFormattingOptions

```typescript
interface DocumentOnTypeFormattingOptions {
  firstTriggerCharacter: string;
  moreTriggerCharacter?: string[];
}
```

**Example in initialize response:**
```json
{
  "documentOnTypeFormattingProvider": {
    "firstTriggerCharacter": "}",
    "moreTriggerCharacter": [";", ")", "\n", ","]
  }
}
```

---

## 5. Common Behaviors from Popular LSP Servers

### clangd (C/C++)

**Formatting Capabilities:**
- `documentFormattingProvider`: true (full document formatting via clang-format)
- `documentRangeFormattingProvider`: true (range formatting)
- `documentOnTypeFormattingProvider`: typically triggers on `}`
  - Recent versions also support newline (`\n`) as trigger character
  - Uses clang-format style from `.clang-format` file

**Behavior Notes:**
- Respects project's `.clang-format` configuration file
- Reformats code after closing braces and newlines
- Can be combined with semantic re-indentation
- Previous versions had issues with newline-triggered formatting causing crashes in edge cases (comments, ternary operators)
- LSP 3.18+ adds support for `textDocument/rangesFormatting` (multiple ranges in one request)

**Option Handling:**
- Respects `tabSize` and `insertSpaces` from FormattingOptions
- Applies clang-format rules on top of these settings

---

### typescript-language-server (TypeScript/JavaScript)

**Formatting Capabilities:**
- `documentFormattingProvider`: true (delegates to TypeScript/JavaScript tsserver)
- `documentRangeFormattingProvider`: true
- `documentOnTypeFormattingProvider`: Not widely documented as implemented

**Behavior Notes:**
- Converts LSP FormattingOptions to tsserver FormatCodeSettings:
  - LSP `tabSize` → tsserver `indentSize`
  - LSP `insertSpaces` → tsserver `convertTabsToSpaces`
- Requests client configuration via `workspace/configuration` request
  - Scope URI: document's URI
  - Section: `"formattingOptions"`
  - Client should return: `{ "tabSize": number, "insertSpaces": boolean }`
- Options may be provided in request params OR via workspace/configuration
- Known issue: Earlier versions failed when FormattingOptions was undefined (now optional in LSP)

**Option Handling:**
- Can receive options directly in request params
- Can ask client for file-specific formatting configuration
- Dynamically adapts to different files' indentation preferences

---

### intelephense (PHP)

**Formatting Capabilities:**
- `documentFormattingProvider`: true (PSR-12 compatible formatting)
- `documentRangeFormattingProvider`: true
- `documentOnTypeFormattingProvider`: Not explicitly documented

**Behavior Notes:**
- Provides lossless PSR-12 compatible formatting
- Formats combined HTML/PHP/JS/CSS documents
- Supports formatter on/off comments:
  - PHP: `// @formatter:off` and `// @formatter:on`
  - HTML: `<!-- @formatter:off -->` and `<!-- @formatter:on -->`
  - JS/CSS: `/* @formatter:off */` and `/* @formatter:on */`
- Can be enabled/disabled via `intelephense.format.enable` setting (default: true)
- Integrates with editor default formatter selection

**Option Handling:**
- Respects FormattingOptions from request
- PSR-12 standard has specific indentation rules that may override user preferences
- Configuration through VS Code settings when used as extension

---

## 6. Complete Request/Response Examples

### Example 1: Full Document Formatting (TypeScript file with multiple edits)

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 42,
  "method": "textDocument/formatting",
  "params": {
    "textDocument": {
      "uri": "file:///Users/dev/project/src/index.ts"
    },
    "options": {
      "tabSize": 4,
      "insertSpaces": true,
      "trimTrailingWhitespace": true,
      "insertFinalNewline": true,
      "trimFinalNewlines": true
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 42,
  "result": [
    {
      "range": {
        "start": { "line": 0, "character": 0 },
        "end": { "line": 0, "character": 1 }
      },
      "newText": ""
    },
    {
      "range": {
        "start": { "line": 5, "character": 0 },
        "end": { "line": 5, "character": 0 }
      },
      "newText": "    "
    },
    {
      "range": {
        "start": { "line": 25, "character": 80 },
        "end": { "line": 25, "character": 85 }
      },
      "newText": ""
    },
    {
      "range": {
        "start": { "line": 100, "character": 0 },
        "end": { "line": 100, "character": 0 }
      },
      "newText": "\n"
    }
  ]
}
```

---

### Example 2: Range Formatting (C++ code)

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 43,
  "method": "textDocument/rangeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///home/dev/cpp-project/main.cpp"
    },
    "range": {
      "start": { "line": 10, "character": 0 },
      "end": { "line": 20, "character": 0 }
    },
    "options": {
      "tabSize": 2,
      "insertSpaces": true
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 43,
  "result": [
    {
      "range": {
        "start": { "line": 10, "character": 0 },
        "end": { "line": 10, "character": 4 }
      },
      "newText": "  "
    },
    {
      "range": {
        "start": { "line": 15, "character": 0 },
        "end": { "line": 15, "character": 6 }
      },
      "newText": "    "
    }
  ]
}
```

---

### Example 3: On-Type Formatting (closing brace)

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 44,
  "method": "textDocument/onTypeFormatting",
  "params": {
    "textDocument": {
      "uri": "file:///workspace/app.js"
    },
    "position": {
      "line": 8,
      "character": 2
    },
    "ch": "}",
    "options": {
      "tabSize": 2,
      "insertSpaces": true,
      "trimTrailingWhitespace": true
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 44,
  "result": [
    {
      "range": {
        "start": { "line": 5, "character": 0 },
        "end": { "line": 5, "character": 0 }
      },
      "newText": "  "
    },
    {
      "range": {
        "start": { "line": 8, "character": 0 },
        "end": { "line": 8, "character": 2 }
      },
      "newText": "}"
    }
  ]
}
```

---

## 7. Key Implementation Considerations

### Overlapping Edits
Text edits MUST NOT overlap. The server should return edits that don't conflict with each other.

### Edit Ordering
Clients typically apply edits in reverse order (from bottom to top, right to left) to avoid position shifts.

### Optional Parameters
- All FormattingOptions fields except `tabSize` and `insertSpaces` are optional
- Clients can omit FormattingOptions entirely (server may use defaults)
- `workDoneToken` is optional in request

### Error Responses
If formatting fails, the server should return an error:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32603,
    "message": "Internal error: formatting not supported for this file type"
  }
}
```

### Position Encoding
LSP 3.17+ allows negotiating character encoding (UTF-8, UTF-16, UTF-32) during initialization to properly calculate character positions.

---

## References

- [LSP 3.17 Official Specification](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/)
- [LSP FormattingOptions - Rust lsp-types](https://docs.rs/lsp-types/0.74.2/lsp_types/struct.FormattingOptions.html)
- [clangd Features and Formatting](https://clangd.llvm.org/features)
- [clangd onTypeFormatting Implementation (LLVM Review D60605)](https://reviews.llvm.org/D60605)
- [TypeScript Language Server GitHub](https://github.com/typescript-language-server/typescript-language-server)
- [TypeScript Language Server Documentation](https://github.com/typescript-language-server/typescript-language-server/blob/master/docs/configuration.md)
- [Intelephense PHP Language Server](https://intelephense.com/)
- [VS Code Intelephense Extension](https://github.com/bmewburn/vscode-intelephense)
