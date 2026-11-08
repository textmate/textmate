package example.preview;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Predicate;

/**
 * Theme preview with Java annotations, generics, records, and text blocks.
 * @param <T> the type of stored item
 */
public final class ThemePreview<T> {
    public static final int DEFAULT_LIMIT = 1_024;
    private final List<T> items;

    public record Theme(String name, boolean dark, Map<String, String> colors) {}

    public ThemePreview(List<T> items) {
        this.items = List.copyOf(items);
    }

    public Optional<T> find(Predicate<T> predicate) {
        return items.stream().filter(predicate).findFirst();
    }

    @Override
    public String toString() {
        return "ThemePreview[count=%d]".formatted(items.size());
    }

    public static String read(Path path) throws IOException {
        try (var reader = Files.newBufferedReader(path)) {
            return reader.lines().reduce("", (left, right) -> left + right + "\n");
        } catch (IOException error) {
            System.err.println("Cannot read: " + path);
            throw error;
        }
    }

    public static void main(String[] args) {
        var theme = new Theme("TextMate", false, Map.of("accent", "#58a6ff"));
        var preview = new ThemePreview<>(List.of(theme));
        double[] values = {3.14159, 6.02e23, -42.0, 0xff};
        String escaped = "Tab:\t newline:\n star:\u2605";
        String document = """
            <section class="preview">
              <h1>Hello, world!</h1>
            </section>
            """;

        preview.find(item -> !item.dark()).ifPresent(System.out::println);
        for (int index = 0; index < values.length; index++) {
            System.out.printf("%d: %.2f%n", index, values[index]);
        }
        String mode = switch (theme.name()) {
            case "TextMate" -> "light";
            default -> "custom";
        };
        System.out.println(mode + escaped + document);
    }
}
