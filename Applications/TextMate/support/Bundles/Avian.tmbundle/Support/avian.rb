require 'cgi'
require 'open3'
require 'openssl'
require 'pathname'
require 'securerandom'
require 'shellwords'
require 'tempfile'
require 'uri'

module Avian
  MAGIC = 'TM3CRYPT'.b.freeze
  PBKDF2_ITERATIONS = 200_000

  module_function

  def prompt_password(prompt)
    script = <<~APPLESCRIPT
      tell application "System Events"
        activate
        text returned of (display dialog "#{prompt}" with title "TextMate" default answer "" with hidden answer buttons {"Cancel", "OK"} default button "OK" cancel button "Cancel")
      end tell
    APPLESCRIPT
    password, error, status = Open3.capture3('/usr/bin/osascript', '-e', script)
    abort(error.empty? ? 'Password entry cancelled.' : error) unless status.success?
    password.delete_suffix!("\n")
    abort 'Password must not be empty.' if password.empty?
    password
  end

  def encrypt(plaintext, password)
    salt = SecureRandom.random_bytes(16)
    iv = SecureRandom.random_bytes(16)
    key_material = OpenSSL::PKCS5.pbkdf2_hmac(password, salt, PBKDF2_ITERATIONS, 64, 'sha256')
    encryption_key = key_material.byteslice(0, 32)
    authentication_key = key_material.byteslice(32, 32)
    cipher = OpenSSL::Cipher.new('aes-256-cbc').encrypt
    cipher.key = encryption_key
    cipher.iv = iv
    ciphertext = cipher.update(plaintext) + cipher.final
    envelope = MAGIC + salt + iv + ciphertext
    envelope + OpenSSL::HMAC.digest('sha256', authentication_key, envelope)
  ensure
    password&.clear
    key_material&.clear
    encryption_key&.clear
    authentication_key&.clear
  end

  def decrypt(payload, password)
    if payload.start_with?(MAGIC)
      decrypt_v3(payload, password)
    elsif payload.start_with?('Salted__'.b)
      decrypt_legacy(payload, password)
    else
      abort 'Unsupported encrypted document format.'
    end
  ensure
    password&.clear
  end

  def decrypt_v3(payload, password)
    abort 'Encrypted document is truncated.' if payload.bytesize < MAGIC.bytesize + 80
    offset = MAGIC.bytesize
    salt = payload.byteslice(offset, 16)
    iv = payload.byteslice(offset + 16, 16)
    ciphertext = payload.byteslice(offset + 32, payload.bytesize - offset - 64)
    supplied_mac = payload.byteslice(-32, 32)
    key_material = OpenSSL::PKCS5.pbkdf2_hmac(password, salt, PBKDF2_ITERATIONS, 64, 'sha256')
    encryption_key = key_material.byteslice(0, 32)
    authentication_key = key_material.byteslice(32, 32)
    authenticated_data = payload.byteslice(0, payload.bytesize - 32)
    expected_mac = OpenSSL::HMAC.digest('sha256', authentication_key, authenticated_data)
    abort 'Authentication failed. The password is incorrect or the document was modified.' unless secure_compare(supplied_mac, expected_mac)
    cipher = OpenSSL::Cipher.new('aes-256-cbc').decrypt
    cipher.key = encryption_key
    cipher.iv = iv
    cipher.update(ciphertext) + cipher.final
  rescue OpenSSL::Cipher::CipherError
    abort 'Authentication failed. The password is incorrect or the document was modified.'
  ensure
    key_material&.clear
    encryption_key&.clear
    authentication_key&.clear
  end

  def secure_compare(left, right)
    return false unless left && right && left.bytesize == right.bytesize
    difference = 0
    left.bytes.zip(right.bytes) { |a, b| difference |= a ^ b }
    difference.zero?
  end

  def decrypt_legacy(payload, password)
    %w[md5 sha256].each do |digest|
      pass_read, pass_write = IO.pipe
      pass_write.write(password)
      pass_write.close
      output, _error, status = Open3.capture3(
        '/usr/bin/openssl', 'enc', '-d', '-aes-128-cbc', '-md', digest,
        '-pass', 'fd:3', { 3 => pass_read, stdin_data: payload, binmode: true }
      )
      pass_read.close
      return output if status.success?
    end
    abort 'Unable to decrypt this legacy document. The password may be incorrect.'
  end

  def selected_paths(value)
    paths = Shellwords.shellwords(value.to_s).map { |path| File.expand_path(path) }
    abort 'Select one or more items in the file browser.' if paths.empty?
    missing = paths.reject { |path| File.exist?(path) }
    abort "Selected item does not exist: #{missing.first}" unless missing.empty?
    paths
  rescue ArgumentError => error
    abort "Unable to parse selected files: #{error.message}"
  end

  def compress(paths)
    directories = paths.map { |path| Pathname(path).dirname.ascend.to_a }
    destination = directories.first.find { |candidate| directories.all? { |items| items.include?(candidate) } }
    abort 'Selected items do not share a parent directory.' unless destination

    base = paths.one? ? File.basename(paths.first) : 'Archive'
    archive = reserve_unique_path(destination.join(base).to_s, '.tbz')
    relative_paths = paths.map { |path| Pathname(path).relative_path_from(destination).to_s }
    temporary = Tempfile.new([".#{base}", '.tbz'], destination.to_s)
    temporary.close
    success = system('/usr/bin/tar', '-cjf', temporary.path, '-C', destination.to_s, '--', *relative_paths)
    abort 'Unable to create archive.' unless success
    File.rename(temporary.path, archive)
    published = true
    archive
  ensure
    temporary&.unlink if temporary && File.exist?(temporary.path)
    File.unlink(archive) if archive && !published && File.exist?(archive)
  end

  def reserve_unique_path(head, tail)
    index = 1
    loop do
      candidate = index == 1 ? head + tail : format('%s %d%s', head, index, tail)
      begin
        File.open(candidate, File::WRONLY | File::CREAT | File::EXCL, 0o600) { }
        return candidate
      rescue Errno::EEXIST
        index += 1
      end
    end
  end

  def decompress(format, payload)
    program = { gzip: '/usr/bin/gunzip', bzip2: '/usr/bin/bunzip2' }.fetch(format)
    output, error, status = Open3.capture3(program, '-c', stdin_data: payload, binmode: true)
    abort(error.empty? ? "Unable to decompress #{format} data." : error) unless status.success?
    output
  end

  def png_description(path, display_name)
    path = File.expand_path(path) if path
    abort 'PNG image has no readable file path.' unless path && File.file?(path) && File.readable?(path)
    output, error, status = Open3.capture3('/usr/bin/sips', '-g', 'pixelWidth', '-g', 'pixelHeight', path)
    abort(error.empty? ? 'Unable to inspect PNG image.' : error) unless status.success?
    dimensions = output.lines.grep(/pixel(?:Width|Height):/)
    abort 'PNG dimensions were not reported.' unless dimensions.size == 2
    "PNG Image: ‘#{display_name}’\n#{dimensions.join}"
  end

  def images_html(paths)
    title = paths.length > 1 ? 'Selected Images' : File.basename(paths.first)
    tags = paths.map do |path|
      url = 'file://' + URI::DEFAULT_PARSER.escape(File.expand_path(path))
      %(<img src="#{CGI.escapeHTML(url)}" alt="#{CGI.escapeHTML(File.basename(path))}">)
    end
    <<~HTML
      <!doctype html>
      <html><head><meta charset="utf-8"><title>#{CGI.escapeHTML(title)}</title></head>
      <body>#{tags.join("<br>\n")}</body></html>
    HTML
  end
end
