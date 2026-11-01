# TextMate theme preview

A longer sample for **bold text**, *italic text*, and ***both together***.
Use `inline code` to inspect markup and code span colors.

## Links and images

[Documentation](https://example.com "A link title")

![A sample image](preview.png "Image description")

An automatic link: <https://example.com>.
Escaped punctuation: \*literal asterisks\* and \`literal backticks\`.

## Quotations and lists

> A quotation with **emphasis**.
>
> A second paragraph with `inline code`.

1. First numbered item
2. Second numbered item
   - A nested bullet
   - Another nested bullet
3. Final numbered item

- [x] Foreground and background
- [x] Headings, links, and emphasis
- [ ] Selection, caret, and invisible characters

## Embedded code

```javascript
// A comment inside a fenced code block.
const name = "TextMate";
function greet(count = 42) {
  return `${name}: ${count}`;
}
```

```diff
- const appearance = "light";
+ const appearance = "dark";
```

## Table and separators

| Scope | Example | Style |
| --- | --- | --- |
| Comment | `// hello` | Muted |
| String | `"world"` | Accent |
| Keyword | `return` | Emphasized |

---

~~Strikethrough text~~ and a footnote reference.[^preview]

[^preview]: This document is only a theme sample.

<div class="preview">Embedded <strong>HTML</strong> and &amp; entities.</div>
