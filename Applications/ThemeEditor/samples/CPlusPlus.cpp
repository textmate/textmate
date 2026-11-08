/** Theme preview: C++ templates, attributes, namespaces, and raw strings. */
#include <algorithm>
#include <iostream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

#define PREVIEW_VERSION "2.4.0"

namespace preview {
constexpr auto limit = 1'024;
enum class Appearance { Light, Dark };

struct Theme {
    std::string name;
    Appearance appearance = Appearance::Light;
    unsigned background = 0xffffff;
};

template <typename T>
class Repository {
public:
    explicit Repository(std::vector<T> values) : values_(std::move(values)) {}
    virtual ~Repository() = default;

    [[nodiscard]] std::optional<T> find(std::string_view name) const {
        for (auto const& value : values_) {
            if (value.name == name) return value;
        }
        return std::nullopt;
    }

    template <typename Callback>
    void each(Callback&& callback) const {
        std::for_each(values_.begin(), values_.end(), callback);
    }

private:
    std::vector<T> values_;
};
} // namespace preview

int main() {
    using namespace preview;
    auto store = std::make_unique<Repository<Theme>>(
        std::vector<Theme>{{"TextMate"}, {"Midnight", Appearance::Dark, 0x1d2233}});
    auto print = [](Theme const& theme) {
        std::cout << theme.name << ": " << std::hex << theme.background << '\n';
    };

    try {
        store->each(print);
        if (auto found = store->find("TextMate"); found.has_value()) {
            std::cout << "Found " << found->name << std::endl;
        }
    } catch (std::exception const& error) {
        std::cerr << error.what() << '\n';
    }

    auto raw = R"html(<div class="preview">Hello, world!</div>)html";
    const double values[] = {3.14159, 6.02e23, -42.0};
    std::cout << raw << "\t" << PREVIEW_VERSION << values[0] << '\n';
    return 0;
}
