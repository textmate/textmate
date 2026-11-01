-- Theme preview: SQL comments, identifiers, functions, and window expressions.
/* A multiline comment.
   Select a query or fold a block to inspect the editor's decorations. */
BEGIN;

CREATE TABLE IF NOT EXISTS themes (
    id          INTEGER PRIMARY KEY,
    name        VARCHAR(120) NOT NULL UNIQUE,
    appearance  VARCHAR(12) DEFAULT 'light',
    accent      CHAR(7) DEFAULT '#58a6ff',
    enabled     BOOLEAN NOT NULL DEFAULT TRUE,
    created_at  TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    CHECK (appearance IN ('light', 'dark', 'unspecified'))
);

INSERT INTO themes (id, name, appearance, accent)
VALUES
    (1, 'TextMate', 'light', '#58a6ff'),
    (2, 'Midnight', 'dark', '#ffcc00'),
    (3, 'Author''s Choice', 'unspecified', NULL);

WITH recent_themes AS (
    SELECT id, name, appearance, accent, created_at,
           ROW_NUMBER() OVER (
               PARTITION BY appearance
               ORDER BY created_at DESC
           ) AS position
    FROM themes
    WHERE enabled = TRUE
      AND name NOT LIKE '%temporary%'
), totals AS (
    SELECT appearance, COUNT(*) AS theme_count
    FROM themes
    GROUP BY appearance
    HAVING COUNT(*) > 0
)
SELECT r.name AS "Theme name",
       UPPER(r.appearance) AS appearance,
       COALESCE(r.accent, '#808080') AS accent,
       CASE
           WHEN r.position = 1 THEN 'Most recent'
           ELSE 'Earlier'
       END AS description,
       t.theme_count,
       ROUND(3.14159 * 100.0, 2) AS sample_number
FROM recent_themes AS r
INNER JOIN totals AS t ON t.appearance = r.appearance
WHERE r.position BETWEEN 1 AND 10
ORDER BY r.appearance ASC, r.name ASC
LIMIT 50;

UPDATE themes
SET accent = '#9b59b6', enabled = FALSE
WHERE id IN (2, 3);

-- The sample is never executed against a database.
ROLLBACK;
