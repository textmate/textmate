TBZ_FILES  = $(DST)/bad_bzip.tbz $(DST)/bad_tar.tbz $(DST)/bad_content.tbz $(DST)/bad_signature.tbz $(DST)/wrong_signature.tbz $(DST)/good.tbz $(DST)/forbidden.tbz
SECKEY     = $(DST)/secret.pem
PUBKEY     = $(DST)/public.pem

all: $(TBZ_FILES) $(PUBKEY) $(DST)/forbidden.tbz
.PHONY: all

$(DST):
	@echo "Create fixtures for Software Update tests…"
	@mkdir -p '$@'

$(SECKEY): $(DST)
	@openssl 2>/dev/null dsaparam 512 < /dev/urandom -genkey -out '$@'

$(PUBKEY): $(SECKEY)
	@openssl 2>/dev/null dsa -in '$<' -pubout -out '$@'

.INTERMEDIATE: $(SECKEY)

$(TBZ_FILES): $(SECKEY) $(DST) Makefile

$(DST)/bad_bzip.tbz:
	@dd 2>/dev/null if=/dev/urandom bs=512 count=1 > '$@'
	@xattr -w 'x-amz-meta-x-signature' "$$(openssl dgst -dss1 -sign '$(SECKEY)' '$@'|openssl enc -base64)" '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/bad_tar.tbz:
	@dd 2>/dev/null if=/dev/urandom bs=512 count=1|bzip2 > '$@'
	@xattr -w 'x-amz-meta-x-signature' "$$(openssl dgst -dss1 -sign '$(SECKEY)' '$@'|openssl enc -base64)" '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/bad_content.tbz:
	@tar --no-recursion -jcf '$@' .
	@xattr -w 'x-amz-meta-x-signature' "$$(openssl dgst -dss1 -sign '$(SECKEY)' '$@'|openssl enc -base64)" '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/bad_signature.tbz:
	@tar --no-recursion -jcf '$@' .
	@xattr -w 'x-amz-meta-x-signature' 'ABCD' '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/wrong_signature.tbz: $(SECKEY)
	@tar --no-recursion -jcf '$@' .
	@xattr -w 'x-amz-meta-x-signature' "$$(echo foo|openssl dgst -dss1 -sign '$(SECKEY)' /dev/stdin|openssl enc -base64)" '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/good.tbz:
	@mkdir -p /tmp/sw-update-test/Avian.app/Contents/MacOS && touch /tmp/sw-update-test/Avian.app/Contents/MacOS/Avian
	@tar -jcf '$@' -C /tmp/sw-update-test Avian.app
	@rm -rf /tmp/sw-update-test
	@xattr -w 'x-amz-meta-x-signature' "$$(openssl dgst -dss1 -sign '$(SECKEY)' '$@'|openssl enc -base64)" '$@'
	@xattr -w 'x-amz-meta-x-signee' 'org.textmate.duff' '$@'
$(DST)/forbidden.tbz:
	@touch '$@' && chmod 0 '$@'

.PHONY: clean
clean:
	rm -rf '$(DST)'
