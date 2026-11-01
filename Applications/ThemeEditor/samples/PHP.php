<?php

declare(strict_types=1);

namespace Preview;

use DateTimeImmutable;
use RuntimeException;

/**
 * Theme preview: namespaces, attributes, types, variables, and interpolation.
 * @template T of object
 */
final class Repository
{
    public const DEFAULT_LIMIT = 1_024;
    private array $items = [];

    public function __construct(public readonly string $name = 'TextMate') {}

    public function add(object $item): static
    {
        $this->items[] = $item;
        return $this;
    }

    public function find(callable $predicate): ?object
    {
        foreach ($this->items as $item) {
            if ($predicate($item)) {
                return $item;
            }
        }
        return null;
    }
}

$theme = (object) ['name' => 'TextMate', 'dark' => false, 'accent' => '#58a6ff'];
$store = (new Repository())->add($theme);
$numbers = [0xff, 0b1010, 3.14159, 6.02e23, -42];
$escaped = "Tab:\t newline:\n quote:\"";
$pattern = '/(?<name>[a-z]+)\s+(\d+)/i';
$report = <<<REPORT
Theme: {$theme->name}
Accent: {$theme->accent}
Message: Hello, world!
REPORT;

try {
    $result = $store->find(fn (object $item): bool => !$item->dark);
    $mode = match ($result?->dark) {
        true => 'dark',
        false => 'light',
        default => 'unspecified',
    };
    preg_match($pattern, 'theme 42', $matches);
    echo $report . PHP_EOL . $mode . $escaped;
} catch (RuntimeException $error) {
    error_log($error->getMessage());
} finally {
    echo (new DateTimeImmutable())->format('Y-m-d');
}
