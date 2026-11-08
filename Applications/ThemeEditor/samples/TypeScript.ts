/** Typed theme preview: interfaces, generics, unions, and decorators. */
import type { ReadonlyDeep } from "./types";

export type Status = "idle" | "loading" | "ready" | "failed";
export type Identifier = string & { readonly brand: unique symbol };
export const DEFAULT_TIMEOUT: number = 5_000;

interface ThemeOptions {
  readonly name: string;
  dark?: boolean;
  colors: Record<string, `#${string}`>;
}

interface Result<T> {
  value: T;
  status: Status;
  error?: Error | null;
}

/** Retains a typed value without changing it. */
function identity<T extends object>(value: T): Readonly<T> {
  return Object.freeze(value);
}

export class Repository<T extends { id: string }> {
  private readonly records = new Map<string, T>();

  constructor(public readonly name: string) {}

  add(record: T): this {
    this.records.set(record.id, record);
    return this;
  }

  find(id: string): Result<T | undefined> {
    const value = this.records.get(id);
    return { value, status: value ? "ready" : "idle" };
  }

  async all(): Promise<readonly T[]> {
    await Promise.resolve();
    return [...this.records.values()];
  }
}

const options = {
  name: "TextMate",
  dark: false,
  colors: { background: "#ffffff", foreground: "#202020" },
} satisfies ThemeOptions;

const point: readonly [number, number] = [12.5, -4];
const message = `Theme: ${options.name}\nPosition: ${point.join(", ")}`;
const pattern: RegExp = /^(?<name>[a-z]+)-(\d+)$/i;

function describe(status: Status): string {
  switch (status) {
    case "ready": return "Everything is ready";
    case "failed": throw new Error("Try again");
    default: return "Please wait…";
  }
}

console.log(identity(options), message, describe("ready"), pattern);
