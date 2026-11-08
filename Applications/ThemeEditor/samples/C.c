/**
 * Theme preview: C comments, preprocessor directives, types, and pointers.
 * Select a function or fold a block to inspect editor decorations.
 */
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define APP_NAME "TextMate"
#define MAX_THEMES 1024
#define ARRAY_COUNT(a) (sizeof(a) / sizeof((a)[0]))

#if defined(__APPLE__)
static const char *platform = "macOS";
#else
static const char *platform = "Other";
#endif

typedef enum appearance {
    APPEARANCE_LIGHT = 0,
    APPEARANCE_DARK = 1
} appearance_t;

typedef struct theme {
    const char *name;
    appearance_t appearance;
    uint32_t background;
} theme_t;

/* Function pointers and a callback signature. */
typedef void (*theme_callback_t)(const theme_t *theme, void *context);

static bool visit_themes(const theme_t *themes, size_t count,
                         theme_callback_t callback, void *context)
{
    if (themes == NULL || callback == NULL)
        return false;
    for (size_t index = 0; index < count; ++index) {
        const theme_t *current = &themes[index];
        callback(current, context);
    }
    return true;
}

static void print_theme(const theme_t *theme, void *context)
{
    (void)context;
    printf("%s: #%06x\n", theme->name, theme->background);
}

int main(void)
{
    const theme_t themes[] = {
        { APP_NAME, APPEARANCE_LIGHT, 0xffffff },
        { "Midnight", APPEARANCE_DARK, 0x1d2233 }
    };
    double ratio = 3.14159e0;
    char escaped[] = "Tab:\t newline:\n quote:\"";
    printf("%s on %s: %.2f %s\n", APP_NAME, platform, ratio, escaped);
    return visit_themes(themes, ARRAY_COUNT(themes), print_theme, NULL)
        ? EXIT_SUCCESS : EXIT_FAILURE;
}
