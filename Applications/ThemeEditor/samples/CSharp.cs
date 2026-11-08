using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace ThemeEditor.Preview;

/// <summary>A sample covering C# types, properties, LINQ, and async code.</summary>
public sealed record Theme(string Name, bool Dark, string Accent);

[Serializable]
public sealed class ThemeRepository<T> where T : class
{
    private readonly List<T> _items = new();
    public const int DefaultLimit = 1_024;
    public event EventHandler? Changed;

    public int Count => _items.Count;
    public IReadOnlyList<T> Items { get => _items.AsReadOnly(); }

    public void Add(T item)
    {
        ArgumentNullException.ThrowIfNull(item);
        _items.Add(item);
        Changed?.Invoke(this, EventArgs.Empty);
    }

    public async Task<T?> FindAsync(Func<T, bool> predicate,
                                   CancellationToken cancellationToken = default)
    {
        await Task.Delay(10, cancellationToken);
        return _items.FirstOrDefault(predicate);
    }
}

public static class Program
{
    public static async Task Main()
    {
        var store = new ThemeRepository<Theme>();
        store.Add(new Theme("TextMate", false, "#58a6ff"));
        store.Add(new Theme("Midnight", true, "#ffcc00"));
        var theme = await store.FindAsync(item => item.Name == "TextMate");
        double[] values = { 3.14159, 6.02e23, -42, 0xff };
        string escaped = "Tab:\t newline:\n star:\u2605";
        string path = @"C:\Themes\Preview.json";
        string report = $"Theme: {theme?.Name ?? "Unknown"}, count: {store.Count}";

        foreach (var item in store.Items.Where(item => !item.Dark))
        {
            Console.WriteLine($"{item.Name}: {item.Accent}");
        }
        string mode = theme switch
        {
            { Dark: true } => "dark",
            { Dark: false } => "light",
            null => "unspecified"
        };
        Console.WriteLine($"{report}\n{mode}\n{path}\n{escaped}");
    }
}
