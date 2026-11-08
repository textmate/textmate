// Package preview demonstrates Go syntax for the theme editor.
package main

import (
    "context"
    "encoding/json"
    "fmt"
    "sync"
    "time"
)

const DefaultLimit = 1_024

// Theme contains the appearance and palette of an editor theme.
type Theme struct {
    Name   string            `json:"name"`
    Dark   bool              `json:"dark"`
    Colors map[string]string `json:"colors,omitempty"`
}

type Store[T any] struct {
    mu     sync.RWMutex
    values map[string]T
}

func (s *Store[T]) Find(key string) (T, bool) {
    s.mu.RLock()
    defer s.mu.RUnlock()
    value, ok := s.values[key]
    return value, ok
}

func load(ctx context.Context, output chan<- Theme) error {
    defer close(output)
    select {
    case <-ctx.Done():
        return ctx.Err()
    case output <- Theme{Name: "TextMate", Dark: false}:
        return nil
    }
}

func main() {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()
    output := make(chan Theme, 1)
    go func() {
        if err := load(ctx, output); err != nil {
            fmt.Printf("Load failed: %v\n", err)
        }
    }()

    numbers := []float64{3.14159, 6.02e23, -42, 0xff}
    message := `A raw string with "quotes".
This line demonstrates multiline strings.`
    escaped := "Tab:\t newline:\n star:\u2605"
    for theme := range output {
        data, err := json.MarshalIndent(theme, "", "  ")
        if err != nil {
            panic(err)
        }
        fmt.Println(string(data), message, escaped)
    }
    for index, number := range numbers {
        fmt.Printf("%d: %.2f\n", index, number)
    }
}
