/**
 * Theme preview: comments, documentation, and syntax styles.
 * @param {string} name - The person to greet.
 * Select text, fold a block, and scroll to inspect the whole sample.
 */
import { readFile } from "node:fs/promises";

export const VERSION = "2.4.0";
const LIMIT = 1_024;
const palette = [0xffcc00, 0b1010, 3.14159, 6.02e23];
const escape = "Tab:\t newline:\n quote:\" Unicode:\u2605";
const expression = /(?<word>[a-z]+)\s+(\d+)/giu;

/** A small service with private state and asynchronous methods. */
export class ThemePreview {
  #cache = new Map();

  constructor(name = "TextMate", options = {}) {
    this.name = name;
    this.options = { enabled: true, timeout: null, ...options };
  }

  get label() {
    return `${this.name} — ${VERSION}`;
  }

  async load(path) {
    try {
      const source = await readFile(path, "utf8");
      const rows = source.split(/\r?\n/).filter(Boolean);
      this.#cache.set(path, rows);
      return rows.map((row, index) => ({ index, row }));
    } catch (error) {
      console.warn(`Could not load ${path}: ${error.message}`);
      throw new Error("Preview failed", { cause: error });
    } finally {
      this.options.lastRead = Date.now();
    }
  }
}

function* counter(max = LIMIT) {
  for (let index = 0; index < max; index += 1) {
    if (index % 2 === 0 && index !== 10) yield index;
  }
}

const preview = new ThemePreview("Hello, world!");
const { enabled, timeout = 500 } = preview.options;
const description = `A multiline template:
  name: ${preview.label}
  status: ${enabled ? "ready" : "waiting"}
  timeout: ${timeout ?? "none"}
`;

// Optional chaining, nullish values, spread, and function calls.
const first = preview?.items?.[0] ?? "Nothing yet";
const values = [...counter(8)].reduce((sum, item) => sum + item, 0);
console.log(description, first, values, expression.test("theme 42"));
