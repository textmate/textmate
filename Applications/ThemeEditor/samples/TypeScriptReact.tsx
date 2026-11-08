/** React theme preview: typed components, hooks, events, and JSX. */
import { useEffect, useId, useMemo, useState } from "react";
import type { ChangeEvent, CSSProperties, ReactNode } from "react";

type Status = "idle" | "loading" | "ready" | "failed";
type Theme = Readonly<{
  id: string;
  name: string;
  appearance: "light" | "dark";
  accent?: `#${string}`;
}>;

interface PanelProps {
  title: string;
  children: ReactNode;
  footer?: ReactNode;
}

const initialThemes = [
  { id: "dawn", name: "Dawn", appearance: "light", accent: "#C47832" },
  { id: "night", name: "Night Sky", appearance: "dark", accent: "#8BB8FF" },
] satisfies readonly Theme[];

// The trailing comma distinguishes a generic arrow function from a JSX tag.
const first = <T,>(items: readonly T[]): T | undefined => items[0];

function Panel({ title, children, footer }: PanelProps) {
  return (
    <section className="panel" aria-label={title}>
      <header><h2>{title}</h2></header>
      <div className="panel__content">{children}</div>
      {footer && <footer>{footer}</footer>}
    </section>
  );
}

export default function ThemeGallery() {
  const inputId = useId();
  const [query, setQuery] = useState<string>("");
  const [selected, setSelected] = useState<Theme | null>(first(initialThemes) ?? null);
  const [status, setStatus] = useState<Status>("loading");
  const [ticks, setTicks] = useState(0);

  useEffect(() => {
    setStatus("ready");
    const timer = window.setInterval(() => setTicks(value => value + 1), 1_000);
    return () => window.clearInterval(timer);
  }, []);

  const visible = useMemo(
    () => initialThemes.filter(theme => theme.name.toLowerCase().includes(query.toLowerCase())),
    [query],
  );

  function handleSearch(event: ChangeEvent<HTMLInputElement>): void {
    setQuery(event.currentTarget.value);
  }

  const previewStyle: CSSProperties = {
    color: selected?.accent ?? "#808080",
    padding: 12,
    borderRadius: "0.5rem",
  };
  const summary = `${visible.length} themes • ${ticks}s elapsed`;

  return (
    <Panel title="Theme gallery" footer={<small>{summary}</small>}>
      {/* JSX comments, attributes, strings, and embedded expressions. */}
      <label htmlFor={inputId}>Find a theme &amp; preview its colors</label>
      <input
        id={inputId}
        type="search"
        value={query}
        onChange={handleSearch}
        placeholder={'Try "Night"…'}
        disabled={status !== "ready"}
      />
      <ul role="list" data-count={visible.length}>
        {visible.map(theme => (
          <li key={theme.id}>
            <button
              type="button"
              aria-pressed={selected?.id === theme.id}
              onClick={() => setSelected(theme)}
            >
              <span aria-hidden="true">{theme.appearance === "dark" ? "☾" : "☀"}</span>
              {" "}{theme.name}
            </button>
          </li>
        ))}
      </ul>
      {visible.length === 0 ? <p>No matching themes.</p> : null}
      <aside style={previewStyle}>
        <>
          <strong>{selected?.name ?? "Select a theme"}</strong>
          <code>{'const message = "Hello\\nReact";'}</code>
          <br />
          <span title="A &lt; B">Typed props + JSX expressions</span>
        </>
      </aside>
    </Panel>
  );
}
