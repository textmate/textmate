#include <buffer/buffer.h>
#include <selection/selection.h>

void test_column_expansion_limit ()
{
    ng::buffer_t buffer;
    std::string lines;
    for(size_t i = 0; i < 10000; ++i) lines += "abc\n";
    buffer.insert(0, lines);
    auto range = ng::range_t(0, buffer.begin(9999)+1, true);
    auto rows = ng::dissect_columnar(buffer, ng::ranges_t(range), 129);
    OAK_ASSERT(rows.size() == 129);
    OAK_ASSERT(rows.last().first.index == buffer.begin(128));
    OAK_ASSERT(ng::dissect_columnar(buffer, ng::ranges_t(range), 0).empty());
    OAK_ASSERT(ng::dissect_columnar(buffer, ng::ranges_t(ng::range_t(0, 1))).size() == 1);
}
