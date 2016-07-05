#!/usr/bin/env ruby

# Add text::is_east_asian_width
#
# This checks if the character needs to be counted as double-width (for soft wrap and similar).
#
# I used the following script to generate the tables, it should be improved to collapse the ranges:

# based on 45f847d01ec774ec4a621f553116b9a6b5dff865

fn_EastAsianWidth='EastAsianWidth.txt'
fn_EmojiSources='EmojiSources.txt'

[fn_EastAsianWidth, fn_EmojiSources].map do |fn|
  unless File.file?(fn)
    system("wget http://www.unicode.org/Public/UNIDATA/#{fn}  ") || raise("download #{fn} failed")
  end
end

fixed, start, stop = [ ], [ ], [ ]
open(fn_EastAsianWidth) do |io|
  io.grep(/^([0-9A-F]+)(?:..([0-9A-F]+))?;[A-Za-z]*(?:W|F)/) do
    if $2
      start << "0x#$1"
      stop << "0x#$2"
    else
      fixed << "0x#$1"
    end
  end
end

fixed.freeze
start.freeze
stop.freeze
# puts "static uint32_t Fixed[]      = { #{fixed.join(', ')} };\n"
# puts "static uint32_t RangeBegin[] = { #{start.join(', ')} };\n"
# puts "static uint32_t RangeEnd[]   = { #{stop.join(', ')} };\n"

puts '--------------------------------------------------------------'
fixed2, start2, stop2 = [ ], [ ], [ ]
open(fn_EmojiSources) do |io|
  io.grep(/^([0-9A-F ]+)\;([0-9A-F ]+)?\;([0-9A-F ]+)?\;([0-9A-F ]+)?/) do
    if $1
      b, e = $1.split(' ')
      next if b.to_i(16) < 256
      if e
        start2 << "0x#{b}"
        stop2 << "0x#{e}"
      else
        fixed2 << "0x#{b}"
      end
    end
  end
end

fixed2 = fixed2.map { |e| e.to_i 16 }
start2 = start2.map { |e| e.to_i 16 }
stop2  = stop2.map { |e| e.to_i 16 }

emoji_array = fixed2.dup
start2.zip(stop2).each do |x,y|
  emoji_array << (x..y).to_a
end
emoji_array = emoji_array.flatten.uniq.sort
emoji_array.freeze


def arrays_merge_expand_flatten( fixed, start, stop )
  fixed = fixed.map { |e| e.to_i 16 }
  start = start.map { |e| e.to_i 16 }
  stop  = stop.map { |e| e.to_i 16 }

  array_all = fixed.dup
  start.zip(stop).each do |x,y|
    array_all << (x..y).to_a
  end
  array_all = array_all.flatten.uniq.sort
  array_all.freeze
end

def array_split_reduce_putstr( origin_array )
  array_all = origin_array.sort.dup
  fixed, start, stop = [ ], [ ], [ ]
  while rBegin = array_all.shift
    rEnd = array_all.shift
    if !rEnd
      fixed << rBegin
      break
    elsif rBegin+1 == rEnd
      while last = array_all.shift
        if rEnd+1 == last
          rEnd = last
        else
          array_all.unshift(last)
          break
        end
      end
      start << rBegin
      stop << rEnd
    else
      fixed << rBegin
      array_all.unshift(rEnd)
    end
  end

  fixed = fixed.map { |e| '%8s' % ("0x%X" % e) }
  start = start.map { |e| '%8s' % ("0x%X" % e) }
  stop  = stop.map { |e| '%8s' % ("0x%X" % e) }

  if origin_array == (ary2=arrays_merge_expand_flatten(fixed, start, stop))
    s1 = fixed.each_slice(10).map{|e| e.join(',') }.join(",\n    ")
    s2 = start.each_slice(10).map{|e| e.join(',') }.join(",\n    ")
    s3 = stop.each_slice(10).map{|e| e.join(',') }.join(",\n    ")
    puts "static uint32_t Fixed[]      = {\n    #{s1} };\n"
    puts "static uint32_t RangeBegin[] = {\n    #{s2} };\n"
    puts "static uint32_t RangeEnd[]   = {\n    #{s3} };\n"
  else
    p (origin_array - ary2).map { |e| "0x%X" % e }
    p (ary2 - origin_array).map { |e| "0x%X" % e }
    raise 'ERROR, Merged arrays different .'
  end

end

a1 = arrays_merge_expand_flatten(fixed, start, stop)
puts "// East Asian Width count: #{a1.size}"
array_split_reduce_putstr(a1)

puts '--------------------------------------------------------------'
puts "// Emoji count: #{emoji_array.size}"
array_split_reduce_putstr(emoji_array)

puts '--------------------------------------------------------------'
a2 = a1 - emoji_array
puts "// East Asian Width(#{a1.size}) - emoji(#{emoji_array.size}) = count: #{a2.size}  (#{(a1&emoji_array).size})"
array_split_reduce_putstr(a2)
